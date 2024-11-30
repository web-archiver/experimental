{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Webar.Text.Normalized
  ( NFText,
    fromAscii,
    toText,
    nfTxt,
  )
where

import Data.Char (isAscii)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- | Text in normal form, only allow ascii character currently
newtype NFText = NFText Text
  deriving newtype (Show, Eq, Ord)
  deriving stock (Lift)

fromAscii :: Text -> Maybe NFText
fromAscii t
  | T.isAscii t = Just (NFText t)
  | otherwise = Nothing

toText :: NFText -> Text
toText (NFText t) = t

nfTxt :: QuasiQuoter
nfTxt =
  QuasiQuoter
    { quoteDec = \_ -> fail "Declaration is not supported",
      quoteType = \_ -> fail "Type is not supported",
      quoteExp = \s ->
        case fromAscii (T.pack s) of
          Just nt -> lift nt
          Nothing -> fail "Only ascii character is allowed",
      quotePat = \s ->
        if all isAscii s
          then [p|NFText $(pure (LitP (StringL s)))|]
          else fail "Only ascii character is allowed"
    }