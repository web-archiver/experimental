mod unit {
    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    #[gcbor(rename_variants = "snake_case")]
    enum Unit {
        A,
        B,
        Var3,
    }

    #[test]
    fn a() {
        test_success(Unit::A, include_bytes!("./data/var_a.bin"))
    }
    #[test]
    fn b() {
        test_success(Unit::B, include_bytes!("./data/var_b.bin"))
    }
    #[test]
    fn var3() {
        test_success(Unit::Var3, include_bytes!("./data/var_var3.bin"))
    }
}

mod unary {
    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    #[gcbor(rename_variants = "snake_case")]
    enum Unary {
        Uv(bool),
        Uv2(Vec<u32>),
    }

    #[test]
    fn uv() {
        test_success(Unary::Uv(true), include_bytes!("./data/var_uv.bin"))
    }
    #[test]
    fn uv2() {
        test_success(
            Unary::Uv2(Vec::from([123, 456])),
            include_bytes!("./data/var_uv2.bin"),
        )
    }
}

mod normal {
    use webar_core::{nf_str, text::normalized::NFString};

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    #[gcbor(rename_variants = "snake_case")]
    enum Normal {
        Nv1(u8, i16),
        Nv2(bool, NFString),
    }

    #[test]
    fn nv1() {
        test_success(
            Normal::Nv1(123, -3200),
            include_bytes!("./data/var_nv1.bin"),
        )
    }
    #[test]
    fn nv2() {
        test_success(
            Normal::Nv2(false, nf_str!("123").to_nf_string()),
            include_bytes!("./data/var_nv2.bin"),
        )
    }
}

mod record {
    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    #[gcbor(rename_variants = "snake_case")]
    enum Record {
        Rv1 { a: u32, k: i16, ab: i8, c: bool },
        Rv2 { z: bool, full: bool, new: u32 },
    }

    #[test]
    fn rv1() {
        test_success(
            Record::Rv1 {
                a: 10,
                k: -128,
                ab: -10,
                c: false,
            },
            include_bytes!("./data/var_rv1.bin"),
        )
    }
    #[test]
    fn rv2() {
        test_success(
            Record::Rv2 {
                z: false,
                full: true,
                new: 0,
            },
            include_bytes!("./data/var_rv2.bin"),
        )
    }
}

mod mixed {
    use webar_core::{nf_str, text::normalized::NFString};

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    #[gcbor(rename_variants = "snake_case")]
    enum Mixed {
        MvUnit,
        MvU2,
        MvUnary(i8),
        MvNormal(bool, bool),
        MvRecord { c: i8, ac: bool, ab: NFString },
    }

    #[test]
    fn unit() {
        test_success(Mixed::MvUnit, include_bytes!("./data/var_mv_unit.bin"))
    }
    #[test]
    fn u2() {
        test_success(Mixed::MvU2, include_bytes!("./data/var_mv_u2.bin"))
    }
    #[test]
    fn unary() {
        test_success(
            Mixed::MvUnary(-10),
            include_bytes!("./data/var_mv_unary.bin"),
        )
    }
    #[test]
    fn normal() {
        test_success(
            Mixed::MvNormal(false, true),
            include_bytes!("./data/var_mv_normal.bin"),
        )
    }
    #[test]
    fn record() {
        test_success(
            Mixed::MvRecord {
                c: -1,
                ac: true,
                ab: nf_str!("a").to_nf_string(),
            },
            include_bytes!("./data/var_mv_record.bin"),
        )
    }
}
