{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Webar.Codec.GCbor.TH
  ( EncodedName,
    mkEncodedName,
    toSnakeCase,
    ProductOptions,
    defaultProductOptions,
    simpleProductOptions,
    SumOptions,
    simpleSumOptions,
    deriveProductToGCbor,
    deriveSumToGCbor,
    deriveProductFromGCbor,
    deriveSumFromGCbor,
    deriveProductGCbor,
    deriveSumGCbor,
  )
where

import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import Data.Either (partitionEithers)
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI
import qualified Data.Text.Unsafe as TU
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Webar.Codec.GCbor.Internal.Decoding
import Webar.Codec.GCbor.Internal.Encoding
import Webar.Codec.GCbor.Internal.Ord (GCborOrd (compareGCbor))
import qualified Webar.Text.Normalized as NT

newtype EncodedName = EncodedName {getEncodedName :: NT.NFText}

toNFText :: String -> NT.NFText
toNFText n = case NT.fromAscii (T.pack n) of
  Just v -> v
  Nothing -> error "Only ascii character is allowed"

mkEncodedName :: String -> EncodedName
mkEncodedName = EncodedName . toNFText

toSnakeCase :: Int -> String -> EncodedName
toSnakeCase n s = mkEncodedName (Aeson.camelTo2 '_' (drop n s))

data FieldOptions = FieldOptions
  { fieldName :: EncodedName,
    omissible :: Bool
  }

newtype RecordOptions = RecordOptions
  { fieldOptions :: String -> FieldOptions
  }

defaultRecordOptions :: RecordOptions
defaultRecordOptions =
  RecordOptions
    { fieldOptions = \f ->
        FieldOptions
          { fieldName = mkEncodedName f,
            omissible = False
          }
    }

simpleRecordOptions :: (String -> EncodedName) -> [String] -> RecordOptions
simpleRecordOptions modifier omitted =
  let omissibleFields = S.fromList omitted
   in RecordOptions
        { fieldOptions = \f ->
            FieldOptions
              { fieldName = modifier f,
                omissible = S.member f omissibleFields
              }
        }

data ConstructorOptions = ConstructorOptions
  { conName :: EncodedName,
    conRecordOptions :: RecordOptions
  }

newtype SumOptions = SumOptions
  { constructorOptions :: String -> ConstructorOptions
  }

simpleSumOptions :: (String -> EncodedName) -> (String -> EncodedName) -> [String] -> SumOptions
simpleSumOptions conModifier fieldModifier omitted =
  SumOptions
    { constructorOptions = \con ->
        ConstructorOptions
          { conName = conModifier con,
            conRecordOptions = simpleRecordOptions fieldModifier omitted
          }
    }

newtype ProductOptions = ProductOptions
  { productRecordOptions :: RecordOptions
  }

simpleProductOptions :: (String -> EncodedName) -> [String] -> ProductOptions
simpleProductOptions fieldModifier omitted =
  ProductOptions
    { productRecordOptions = simpleRecordOptions fieldModifier omitted
    }

defaultProductOptions :: ProductOptions
defaultProductOptions =
  ProductOptions
    { productRecordOptions = defaultRecordOptions
    }

data TypeConInfo = TypeConInfo
  { tciName :: Name,
    tciVars :: [TyVarBndr ()],
    tciRoles :: [Role]
  }

data ProductDec = ProductDec
  { dpTypeCon :: TypeConInfo,
    dpCon :: Con
  }

reifyProductCon :: Name -> Q ([TyVarBndr ()], Con)
reifyProductCon n =
  reify n >>= \case
    TyConI (DataD _ _ tv _ [c] _) -> pure (tv, c)
    TyConI (NewtypeD _ _ tv _ con _) -> pure (tv, con)
    _ -> fail "only data with single constructor or newtype decl is allowed"

reifyProductDec :: Name -> Q ProductDec
reifyProductDec n = do
  (tv, con) <- reifyProductCon n
  roles <- reifyRoles n
  pure
    ProductDec
      { dpTypeCon = TypeConInfo {tciName = n, tciVars = tv, tciRoles = roles},
        dpCon = con
      }

data SumDec = SumDec
  { dsName :: Name,
    dsTypeCon :: TypeConInfo,
    dsCons :: [Con]
  }

reifySumCon :: Name -> Q ([TyVarBndr ()], [Con])
reifySumCon n =
  reify n >>= \case
    TyConI (DataD _ _ tv _ cs _) -> pure (tv, cs)
    TyConI (NewtypeD _ _ tv _ con _) -> pure (tv, [con])
    _ -> fail "only data or newtype decl is allowed"

reifySumDec :: Name -> Q SumDec
reifySumDec n = do
  (tv, cons) <- reifySumCon n
  roles <- reifyRoles n
  pure
    SumDec
      { dsName = n,
        dsTypeCon = TypeConInfo {tciName = n, tciVars = tv, tciRoles = roles},
        dsCons = cons
      }

getConOpt :: SumOptions -> Name -> ConstructorOptions
getConOpt opt (Name (OccName name) _) = constructorOptions opt name

mkInstance :: Name -> TypeConInfo -> Name -> Exp -> DecQ
mkInstance cls tci funName funBody = do
  (constraints, tyWithArg) <-
    foldlM
      ( \(cs, t) (var, role) -> do
          tyArg <-
            newName
              ( case var of
                  PlainTV (Name (OccName n) _) _ -> n
                  KindedTV (Name (OccName n) _) _ _ -> n
              )
          case role of
            PhantomR -> pure (cs, t `AppT` VarT tyArg)
            _ ->
              pure
                ( ConT cls `AppT` VarT tyArg : cs,
                  t `AppT` VarT tyArg
                )
      )
      ([], ConT (tciName tci))
      (zip (tciVars tci) (tciRoles tci))
  pure
    ( InstanceD
        Nothing
        constraints
        (AppT (ConT cls) tyWithArg)
        [ValD (VarP funName) (NormalB funBody) []]
    )

sortFields :: RecordOptions -> [VarBangType] -> [(Name, FieldOptions)]
sortFields opts fs =
  L.sortBy
    ( \(_, f1) (_, f2) ->
        compareGCbor
          (getEncodedName (fieldName f1))
          (getEncodedName (fieldName f2))
    )
    (fmap (\(n@(Name (OccName name) _), _, _) -> (n, fieldOptions opts name)) fs)

mkNormalEncoder :: Name -> NonEmpty BangType -> Q (Pat, Exp)
mkNormalEncoder con fs = do
  (pats, eh :| et) <-
    NEL.unzip
      <$> traverse
        ( \_ -> do
            var <- newName "f"
            pure (VarP var, VarE 'toGCbor `AppE` VarE var)
        )
        fs
  pure
    ( ConP con [] (NEL.toList pats),
      foldl' (\a e -> InfixE (Just a) (VarE '(<>)) (Just e)) eh et
    )

mkRecordEncoder :: RecordOptions -> Name -> [VarBangType] -> Q (Pat, Exp, Exp)
mkRecordEncoder opt con fs = do
  (pats, sizeExprs, exprs) <-
    unzip3
      <$> traverse
        ( \( name@(Name (OccName varName) _),
             FieldOptions {fieldName = EncodedName serName, omissible = om}
             ) -> do
              var <- newName varName
              (size, expr) <-
                if om
                  then do
                    sizExp <- [|omissibleFieldSize $(varE var)|]
                    serExp <- [|encodeOmissibleField $(lift serName) $(varE var)|]
                    pure (Just sizExp, serExp)
                  else do
                    serExp <- [|encodeField $(lift serName) $(varE var)|]
                    pure (Nothing, serExp)
              pure ((name, VarP var), size, expr)
        )
        (sortFields opt fs)
  pure
    ( RecP con pats,
      sizeExpr 0 [] sizeExprs,
      case exprs of
        [] -> VarE 'mempty
        h : t -> foldl' (\acc e -> InfixE (Just acc) (VarE '(<>)) (Just e)) h t
    )
  where
    sizeExpr :: Int -> [Exp] -> [Maybe Exp] -> Exp
    sizeExpr n es [] =
      foldr'
        (\fe acc -> InfixE (Just acc) (VarE '(+)) (Just fe))
        (LitE (IntegerL (fromIntegral n)))
        es
    sizeExpr n es (Nothing : xs) = sizeExpr (n + 1) es xs
    sizeExpr n es (Just e : xs) = sizeExpr n (e : es) xs

productEncoder :: ProductOptions -> Con -> Q Exp
productEncoder _ (NormalC _ []) = fail "Empty normal constructor is not supported"
productEncoder _ (NormalC name fs@(h : t)) = do
  (p, e) <- mkNormalEncoder name (h :| t)
  lamE
    [pure p]
    [|
      encodeNormalProduct $(lift (fromIntegral (length fs) :: Word))
        <> $(pure e)
      |]
productEncoder _ (RecC _ []) = fail "Empty record constructor is not supported"
productEncoder opt (RecC name fs) = do
  (p, s, e) <- mkRecordEncoder (productRecordOptions opt) name fs
  lamE [pure p] [|encodeRecordProduct $(pure s) <> $(pure e)|]
productEncoder _ _ = fail "Unsupported constructor"

sumEncoder :: SumOptions -> [Con] -> Q Exp
sumEncoder opt cs =
  LamCaseE
    <$> traverse
      ( \case
          NormalC con [] -> do
            let conOpt = getConOpt opt con
            expr <- [|encodeUnitSum $(lift (getEncodedName (conName conOpt)))|]
            pure (Match (ConP con [] []) (NormalB expr) [])
          NormalC con [_] -> do
            let conOpt = getConOpt opt con
            var <- newName "v"
            expr <- [|encodeUnarySum $(lift (getEncodedName (conName conOpt))) $(varE var)|]
            pure (Match (ConP con [] [VarP var]) (NormalB expr) [])
          NormalC con fs@(h : t) -> do
            let conOpt = getConOpt opt con
            (pat, enc) <- mkNormalEncoder con (h :| t)
            expr <-
              [|
                encodeNormalSum
                  $(lift (getEncodedName (conName conOpt)))
                  $(lift (fromIntegral (length fs) :: Word))
                  <> $(pure enc)
                |]
            pure (Match pat (NormalB expr) [])
          RecC con fs -> do
            let conOpt = getConOpt opt con
            (pat, siz, enc) <- mkRecordEncoder (conRecordOptions conOpt) con fs
            expr <-
              [|
                encodeRecordSum $(lift (getEncodedName (conName conOpt))) $(pure siz)
                  <> $(pure enc)
                |]
            pure (Match pat (NormalB expr) [])
          _ -> fail "Unsupported constructor"
      )
      cs

mkToGCborInstance :: TypeConInfo -> Exp -> DecQ
mkToGCborInstance tci = mkInstance ''ToGCbor tci 'toGCbor

mkProductToGCborInst :: ProductOptions -> ProductDec -> Q Dec
mkProductToGCborInst opt dec =
  productEncoder opt (dpCon dec) >>= mkToGCborInstance (dpTypeCon dec)

deriveProductToGCbor :: ProductOptions -> Name -> DecsQ
deriveProductToGCbor opt name =
  reifyProductDec name >>= fmap L.singleton . mkProductToGCborInst opt

mkSumToGCborInst :: SumOptions -> SumDec -> Q Dec
mkSumToGCborInst opt dec =
  sumEncoder opt (dsCons dec) >>= mkToGCborInstance (dsTypeCon dec)

deriveSumToGCbor :: SumOptions -> Name -> DecsQ
deriveSumToGCbor opt name =
  reifySumDec name >>= fmap L.singleton . mkSumToGCborInst opt

sizeNfText :: NT.NFText -> Int
sizeNfText l = TU.lengthWord8 (NT.toText l)
{-# INLINE sizeNfText #-}

equalNfText :: Int -> NT.NFText -> NT.NFText -> Bool
equalNfText len l r =
  let TI.Text arr1 off1 _ = NT.toText l
      TI.Text arr2 off2 _ = NT.toText r
   in TA.equal arr1 off1 arr2 off2 len
{-# INLINE equalNfText #-}

matchNfText :: Name -> [(NT.NFText, Exp)] -> Exp -> Q Exp
matchNfText var cs err =
  let mp =
        foldr'
          ( \p@(k, _) ->
              IM.alter
                ( \case
                    Just ps -> Just (p : ps)
                    Nothing -> Just [p]
                )
                (sizeNfText k)
          )
          IM.empty
          cs
   in do
        ms <-
          traverse
            ( \(len, ps) -> do
                guards <-
                  traverse
                    ( \(k, v) -> do
                        g <- [|equalNfText $(lift len) $(varE var) $(lift k)|]
                        pure (NormalG g, v)
                    )
                    ps
                pure
                  ( Match
                      (LitP (IntegerL (fromIntegral len)))
                      (GuardedB guards)
                      []
                  )
            )
            (IM.toAscList mp)
        lenExpr <- [|sizeNfText $(varE var)|]
        pure (CaseE lenExpr (ms ++ [Match WildP (NormalB err) []]))

mkNormalDecoder :: Name -> NonEmpty BangType -> Q Exp
mkNormalDecoder con (_ :| t) = do
  expr <- [|$(conE con) <$> fromGCbor|]
  foldM (\e _ -> [|$(pure e) <*> fromGCbor|]) expr t

mkRecordDecoder :: Name -> RecordOptions -> Name -> [VarBangType] -> Q Exp
mkRecordDecoder decodeFun opt con fs = do
  let fields = zip [(0 :: Word) ..] (sortFields opt fs)
  keyVar <- newName "fieldIndex"
  fieldToIdx <- do
    key <- newName "key"
    matches <-
      traverse
        ( \(idx, (_, FieldOptions {fieldName = EncodedName serName})) ->
            fmap (\e -> (serName, e)) [|Known $(lift idx)|]
        )
        fields
    caseExpr <- matchNfText key matches (ConE 'Unknown)
    pure [FunD keyVar [Clause [VarP key] (NormalB caseExpr) []]]
  initState <- newName "state"
  bodyE <- do
    (finalState, revFieldExprs, revStmts) <-
      foldM
        ( \(state, fieldExprs, stmts)
           (idx, (name@(Name (OccName varName) _), fieldOpt)) -> do
              var <- newName varName
              nextState <- newName "state"
              pat <- [p|RecordResult $(varP var) $(varP nextState)|]
              expr <-
                if omissible fieldOpt
                  then [|decodeOmissibleField $(varE keyVar) $(lift idx) $(varE state)|]
                  else [|decodeRequiredField $(varE keyVar) $(lift idx) $(varE state)|]
              pure
                ( nextState,
                  (name, VarE var) : fieldExprs,
                  BindS pat expr : stmts
                )
        )
        (initState, [], [])
        fields
    endStmt <-
      NoBindS
        <$> [|
          pure
            ( RecordResult
                $(pure (RecConE con (reverse revFieldExprs)))
                $(varE finalState)
            )
          |]
    pure
      ( DoE
          Nothing
          (reverse (endStmt : revStmts))
      )
  LetE fieldToIdx
    <$> [|
      $(varE decodeFun)
        $(lift (show con))
        $(varE keyVar)
        $(pure (LamE [VarP initState] bodyE))
      |]

productDecoder :: RecordOptions -> Con -> Q Exp
productDecoder _ (NormalC _ []) = fail "Empty normal constructor is not supported"
productDecoder _ (NormalC con fs@(h : t)) =
  [|decodeNormalProductOf $(lift (length fs)) >> $(mkNormalDecoder con (h :| t))|]
productDecoder _ (RecC _ []) = fail "Empty record constructor is not supported"
productDecoder opt (RecC con fs) =
  mkRecordDecoder 'decodeRecordProduct opt con fs
productDecoder _ _ = fail "unsupported constructor"

sumDecoder :: SumOptions -> Name -> [Con] -> Q Exp
sumDecoder opt ty cs = do
  (unitCons, compoundCons) <-
    partitionEithers
      <$> traverse
        ( \case
            NormalC con [] ->
              let conOpt = getConOpt opt con
               in fmap
                    (\v -> Left (getEncodedName (conName conOpt), v))
                    [|pure $(conE con)|]
            NormalC con [_] ->
              let conOpt = getConOpt opt con
               in compoundSumM conOpt
                    <$> [|$(conE con) <$> decodeUnarySum|]
            NormalC con fs@(h : t) ->
              let conOpt = getConOpt opt con
               in compoundSumM conOpt
                    <$> [|
                      decodeNormalSumOf $(lift (length fs))
                        >> $(mkNormalDecoder con (h :| t))
                      |]
            RecC _ [] -> fail "Empty record constructor is not supported"
            RecC con fs ->
              let conOpt = getConOpt opt con
               in compoundSumM conOpt
                    <$> mkRecordDecoder 'decodeRecordSum (conRecordOptions conOpt) con fs
            _ -> fail "Unsupported constructor"
        )
        cs
  [|
    decodeSum $(lift (show ty))
      >>= $( lamCaseE
               [ newName "unitCon" >>= \unitVar ->
                   match
                     [p|SkUnit $(varP unitVar)|]
                     (normalB (failExpr unitVar >>= matchNfText unitVar unitCons))
                     [],
                 newName "compCon" >>= \compoundVar ->
                   match
                     [p|SkCompound $(varP compoundVar)|]
                     (normalB (failExpr compoundVar >>= matchNfText compoundVar compoundCons))
                     []
               ]
           )
    |]
  where
    compoundSumM conOpt expr = Right (getEncodedName (conName conOpt), expr)

    failExpr conVar =
      [|fail ($(lift (show ty ++ ": Unknown constructor ")) ++ show $(varE conVar))|]

mkFromGCborInstance :: TypeConInfo -> Exp -> DecQ
mkFromGCborInstance tci = mkInstance ''FromGCbor tci 'fromGCbor

mkProductFromGCborInst :: ProductOptions -> ProductDec -> Q Dec
mkProductFromGCborInst opt dec =
  productDecoder (productRecordOptions opt) (dpCon dec) >>= mkFromGCborInstance (dpTypeCon dec)

deriveProductFromGCbor :: ProductOptions -> Name -> Q [Dec]
deriveProductFromGCbor opt name =
  reifyProductDec name >>= fmap L.singleton . mkProductFromGCborInst opt

mkSumFromGCborInst :: SumOptions -> SumDec -> Q Dec
mkSumFromGCborInst opt dec =
  sumDecoder opt (dsName dec) (dsCons dec) >>= mkFromGCborInstance (dsTypeCon dec)

deriveSumFromGCbor :: SumOptions -> Name -> Q [Dec]
deriveSumFromGCbor opt name =
  reifySumDec name >>= fmap L.singleton . mkSumFromGCborInst opt

deriveProductGCbor :: ProductOptions -> Name -> Q [Dec]
deriveProductGCbor opt name =
  reifyProductDec name >>= \dec ->
    liftA2
      (\f t -> [f, t])
      (mkProductToGCborInst opt dec)
      (mkProductFromGCborInst opt dec)

deriveSumGCbor :: SumOptions -> Name -> Q [Dec]
deriveSumGCbor opt name =
  reifySumDec name >>= \dec ->
    liftA2
      (\f t -> [f, t])
      (mkSumToGCborInst opt dec)
      (mkSumFromGCborInst opt dec)