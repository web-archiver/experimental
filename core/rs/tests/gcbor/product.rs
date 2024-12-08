mod tuple {
    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    struct Tuple(i32, Vec<i32>, Vec<u16>);

    #[test]
    fn t0() {
        test_success(
            Tuple(1, Vec::from([2, 3]), Vec::from([4, 5])),
            include_bytes!("./data/prod_normal_0.bin"),
        )
    }

    #[test]
    fn bound() {
        test_success(
            Tuple(i32::MIN, Vec::from([0, i32::MAX, 10]), Vec::from([1, 2, 3])),
            include_bytes!("./data/prod_normal_bound.bin"),
        )
    }
}

mod record {
    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    struct Record {
        a: u32,
        b: Vec<u64>,
    }

    #[test]
    fn t0() {
        test_success(
            Record {
                a: 1,
                b: Vec::from([2, 3]),
            },
            include_bytes!("./data/prod_record_0.bin"),
        )
    }
    #[test]
    fn bound() {
        test_success(
            Record {
                a: u32::MAX,
                b: Vec::from([0, 1, u64::MAX]),
            },
            include_bytes!("./data/prod_record_max.bin"),
        )
    }
}

mod sort {
    use webar_core::{nf_str, text::normalized::NFString};

    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    struct Sort {
        c: Vec<i32>,
        bac: NFString,
        ab: i32,
        a: u8,
    }

    #[test]
    fn t0() {
        test_success(
            Sort {
                c: Vec::from([-1, 0, 1]),
                bac: nf_str!("example").to_nf_string(),
                ab: 10,
                a: u8::MAX,
            },
            include_bytes!("./data/prod_sort_0.bin"),
        )
    }

    #[test]
    fn t1() {
        test_success(
            Sort {
                a: 1,
                c: Vec::new(),
                ab: 12,
                bac: nf_str!("sss").to_nf_string(),
            },
            include_bytes!("./data/prod_sort_1.bin"),
        )
    }
}

mod sort_nested {
    use crate::test_success;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    struct Inner {
        b: u16,
        aa: bool,
    }
    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
    struct SortNested {
        i: Inner,
        aa: u32,
        cab: i64,
    }

    #[test]
    fn t0() {
        test_success(
            SortNested {
                i: Inner { b: 10, aa: true },
                aa: 1000,
                cab: -6,
            },
            include_bytes!("./data/prod_sort_nested_0.bin"),
        )
    }
    #[test]
    fn bound() {
        test_success(
            SortNested {
                i: Inner { b: 10, aa: true },
                cab: -255,
                aa: 1000,
            },
            include_bytes!("./data/prod_sort_nested_1.bin"),
        )
    }
}

mod omit {
    macro_rules! file_field_name {
        () => {
            "full"
        };
        ($($v:literal),+) => {
            concat!($("o", stringify!($v)),+ )
        };
    }
    macro_rules! value_expr {
        ($ty:ident, ($($f:literal),*)) => {
            paste::paste! {
                $ty {
                    $([< f $f >]: None,)+
                    .. $ty::FULL
                }
            }
        };
    }
    macro_rules! omit_test {
        ($ty:ident) => {
            #[test]
            fn full() {
                crate::test_success(
                    $ty::FULL,
                    include_bytes!("./data/prod_omit_full.bin")
                )
            }
        };
        ($ty:ident, ($($f:literal),+)) => {
            paste::paste! {
                #[test]
                fn [< $(o $f)+ >] () {
                    crate::test_success(
                        value_expr!($ty, ($($f),+)),
                        include_bytes!(concat!("./data/prod_omit_", file_field_name!($($f),+), ".bin"))
                    )
                }
            }
        };
    }

    mod r2o1r2 {
        #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
        struct R2O1R2 {
            f0: u8,
            f1: u8,
            #[gcbor(omissible)]
            f2: Option<u8>,
            f3: u8,
            f4: u8,
        }
        impl R2O1R2 {
            const FULL: Self = Self {
                f0: 0,
                f1: 1,
                f2: Some(2),
                f3: 3,
                f4: 4,
            };
        }

        omit_test!(R2O1R2);
        omit_test!(R2O1R2, (2));
    }

    mod r1o3r1 {
        #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
        struct R1O3R1 {
            f0: u8,
            #[gcbor(omissible)]
            f1: Option<u8>,
            #[gcbor(omissible)]
            f2: Option<u8>,
            #[gcbor(omissible)]
            f3: Option<u8>,
            f4: u8,
        }
        impl R1O3R1 {
            const FULL: Self = Self {
                f0: 0,
                f1: Some(1),
                f2: Some(2),
                f3: Some(3),
                f4: 4,
            };
        }

        omit_test!(R1O3R1);
        omit_test!(R1O3R1, (1));
        omit_test!(R1O3R1, (2));
        omit_test!(R1O3R1, (3));
        omit_test!(R1O3R1, (1, 2));
        omit_test!(R1O3R1, (2, 3));
        omit_test!(R1O3R1, (1, 3));
        omit_test!(R1O3R1, (1, 2, 3));
    }

    mod o3r2 {
        #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
        struct O3R2 {
            #[gcbor(omissible)]
            f0: Option<u8>,
            #[gcbor(omissible)]
            f1: Option<u8>,
            #[gcbor(omissible)]
            f2: Option<u8>,
            f3: u8,
            f4: u8,
        }
        impl O3R2 {
            const FULL: Self = Self {
                f0: Some(0),
                f1: Some(1),
                f2: Some(2),
                f3: 3,
                f4: 4,
            };
        }

        omit_test!(O3R2);
        omit_test!(O3R2, (0));
        omit_test!(O3R2, (1));
        omit_test!(O3R2, (2));
        omit_test!(O3R2, (0, 1));
        omit_test!(O3R2, (0, 2));
        omit_test!(O3R2, (1, 2));
        omit_test!(O3R2, (0, 1, 2));
    }

    mod r2o3 {
        #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec)]
        struct R2O3 {
            f0: u8,
            f1: u8,
            #[gcbor(omissible)]
            f2: Option<u8>,
            #[gcbor(omissible)]
            f3: Option<u8>,
            #[gcbor(omissible)]
            f4: Option<u8>,
        }
        impl R2O3 {
            const FULL: Self = Self {
                f0: 0,
                f1: 1,
                f2: Some(2),
                f3: Some(3),
                f4: Some(4),
            };
        }

        omit_test!(R2O3);
        omit_test!(R2O3, (2));
        omit_test!(R2O3, (3));
        omit_test!(R2O3, (4));
        omit_test!(R2O3, (2, 3));
        omit_test!(R2O3, (2, 4));
        omit_test!(R2O3, (3, 4));
        omit_test!(R2O3, (2, 3, 4));
    }
}
