use std::fmt::Debug;

use webar_core::codec::gcbor::{from_slice, to_vec, FromGCborSlice, ToGCbor};

fn test_success<T: Debug + Eq + ToGCbor + FromGCborSlice>(v: T, bin: &[u8]) {
    assert_eq!(to_vec(&v), bin, "encode");
    assert_eq!(from_slice::<T>(bin).unwrap(), v, "decode")
}
fn encode_test<T: ToGCbor + ?Sized>(v: &T, bin: &[u8]) {
    assert_eq!(to_vec(v), bin)
}

mod product;
mod sum;

#[test]
fn null() {
    test_success((), include_bytes!("./data/null.bin"))
}

mod integer {

    mod positive {
        macro_rules! test_val {
            ($t:ty, $v:literal, $p:literal) => {
                paste::paste! {
                    #[test]
                    fn [<$t _ $v>] () {
                        crate::test_success::<$t>($v, include_bytes!($p))
                    }
                }
            };
        }

        test_val!(u8, 0, "./data/zero.bin");
        test_val!(u64, 0, "./data/zero.bin");
        test_val!(u32, 1, "./data/one.bin");
        test_val!(i32, 10, "./data/ten.bin");
        test_val!(u8, 23, "./data/23.bin");
        test_val!(u64, 24, "./data/24.bin");
        test_val!(i8, 100, "./data/100.bin");
        test_val!(u16, 1000, "./data/1000.bin");
        test_val!(u32, 1_000_000, "./data/million.bin");
        test_val!(i64, 1_000_000_000_000, "./data/1e12.bin");
    }

    mod negative {
        macro_rules! test_val {
            ($t:ty, -$v:literal, $p:literal) => {
                paste::paste! {
                    #[test]
                    fn [<$t _minus_ $v>]() {
                        crate::test_success::<$t>(-$v, include_bytes!($p));
                    }
                }
            };
        }

        test_val!(i8, -1, "./data/minus_one.bin");
        test_val!(i16, -10, "./data/minus_ten.bin");
        test_val!(i32, -100, "./data/-100.bin");
        test_val!(i64, -1000, "./data/-1000.bin");
    }

    mod min_max {
        macro_rules! test_int {
            ($n:ident, $t:ty) => {
                mod $n {
                    #[test]
                    fn min() {
                        crate::test_success(
                            <$t>::MIN,
                            include_bytes!(concat!("./data/", stringify!($t), "_min.bin")),
                        );
                    }
                    #[test]
                    fn max() {
                        crate::test_success(
                            <$t>::MAX,
                            include_bytes!(concat!("./data/", stringify!($t), "_max.bin")),
                        );
                    }
                }
            };
        }
        macro_rules! test_word {
            ($n:ident, $t:ty) => {
                mod $n {
                    #[test]
                    fn max() {
                        crate::test_success(
                            <$t>::MAX,
                            include_bytes!(concat!("./data/", stringify!($t), "_max.bin")),
                        )
                    }
                }
            };
        }

        test_int!(i64, i64);
        test_word!(u64, u64);
        test_int!(i8, i8);
        test_word!(u8, u8);
        test_int!(i16, i16);
        test_word!(u16, u16);
        test_int!(i32, i32);
        test_word!(u32, u32);
    }
}

mod bool {
    use crate::test_success;

    #[test]
    fn t() {
        test_success(true, include_bytes!("./data/true.bin"))
    }
    #[test]
    fn f() {
        test_success(false, include_bytes!("./data/false.bin"))
    }
}

mod text {
    mod normalized {
        use webar_core::text::normalized::NFStr;

        struct TestCase(&'static NFStr, &'static [u8]);
        impl TestCase {
            fn test_string(self) {
                crate::test_success(self.0.to_nf_string(), self.1)
            }
            fn test_str(self) {
                crate::encode_test(self.0, self.1)
            }
        }

        macro_rules! mk_tests {
            ($(($n:ident, $c:ident, $t:expr)),+) => {
                $(const $c: TestCase = TestCase(
                    webar_core::text::normalized::nf_str!($t),
                    include_bytes!(concat!("./data/", stringify!($n), ".bin")),
                );)+

                mod string {
                    $(  #[test]
                        fn $n() {
                            super::$c.test_string()
                        }
                    )+
                }

                mod str {
                    $(
                        #[test]
                        fn $n() {
                            super::$c.test_str()
                        }
                    )+
                }
            };
        }

        mk_tests!(
            (empty_str, S_EMPTY, ""),
            (a, S_A, "a"),
            (ietf, S_IETF, "IETF"),
            (escape_str, S_ESCAPE, "\"\\"),
            (large_text, S_LARGE, include_str!("./data/large_text.txt"))
        );
    }

    mod raw_utf8 {
        use webar_core::text::raw_utf8::{RawUtf8StrRef, RawUtf8String};

        struct TestCase(RawUtf8StrRef<'static>, &'static [u8]);
        impl TestCase {
            fn test_string(self) {
                crate::test_success(RawUtf8String::from(self.0), self.1)
            }
            fn test_str(self) {
                crate::encode_test(&self.0, self.1)
            }
        }

        macro_rules! mk_tests {
            ($(($n:ident, $c:ident, $v:literal)),+) => {
                $(const $c: TestCase = TestCase(
                    webar_core::text::raw_utf8::raw_utf8_str_ref!($v),
                    include_bytes!(concat!("./data/raw_utf8_", stringify!($n), ".bin"))
                );)+

                mod string {
                    $(#[test]
                        fn $n() {
                            super::$c.test_string()
                        }
                    )+
                }

                mod str {
                    $(#[test]
                    fn $n() {
                        super::$c.test_str()
                    })+
                }
            };
        }

        mk_tests!(
            (empty, T_EMPTY, ""),
            (abc, T_ABC, "abc"),
            (unnormalized, T_A_0308, "a\u{0308}")
        );
    }
}

mod bytes {
    use webar_core::bytes::ByteBuf;

    use crate::test_success;

    #[test]
    fn empty() {
        test_success(
            ByteBuf(Vec::new()),
            include_bytes!("./data/empty_bytes.bin"),
        )
    }
    #[test]
    fn sample() {
        test_success(
            ByteBuf(Vec::from([0x01, 0x02, 0x03, 0x04])),
            include_bytes!("./data/sample_bytes.bin"),
        )
    }
}

mod array {
    mod vec {
        use crate::test_success;

        #[test]
        fn empty() {
            test_success(
                Vec::<u8>::from([]),
                include_bytes!("./data/empty_array.bin"),
            );
        }

        #[test]
        fn v_123() {
            test_success(
                Vec::from([1u32, 2, 3]),
                include_bytes!("./data/123_array.bin"),
            )
        }

        #[test]
        fn vec_25_elem() {
            test_success(
                (1u32..=25).collect::<Vec<_>>(),
                include_bytes!("./data/array_25.bin"),
            )
        }
    }

    mod array {
        use crate::test_success;

        #[test]
        fn empty() {
            test_success::<[u32; 0]>([], include_bytes!("./data/empty_array.bin"))
        }

        #[test]
        fn v_123() {
            test_success([1u32, 2, 3], include_bytes!("./data/123_array.bin"))
        }
    }

    mod slice {
        use webar_core::codec::gcbor::to_vec;

        #[test]
        fn empty() {
            assert_eq!(
                to_vec::<[u8]>(&[]),
                include_bytes!("./data/empty_array.bin")
            )
        }

        #[test]
        fn v_123() {
            assert_eq!(
                to_vec(&[1u32, 2, 3]),
                include_bytes!("./data/123_array.bin")
            )
        }
    }
}

mod time {
    use webar_core::time::{TimePeriod, Timestamp};

    use crate::test_success;

    mod timestamp {
        use webar_core::time::Timestamp;

        use crate::test_success;

        #[test]
        fn with_uncertainty() {
            test_success(
                Timestamp::new(1697724754, 873294000, Some((0, 1000))),
                include_bytes!("./data/timestamp_uncertainty.bin"),
            )
        }
        #[test]
        fn no_uncertainty() {
            test_success(
                Timestamp::new(1697724754, 873294000, None),
                include_bytes!("./data/timestamp_no_uncertainty.bin"),
            )
        }
    }

    #[test]
    fn period() {
        test_success(
            TimePeriod(
                Timestamp::new(1697724754, 873294000, Some((0, 1))),
                Timestamp::new(1697724755, 0, Some((0, 1))),
            ),
            include_bytes!("./data/period_sample.bin"),
        )
    }
}

mod uuid {
    use uuid::{uuid, Uuid};

    use crate::test_success;

    #[test]
    fn uil() {
        test_success(uuid::Uuid::nil(), include_bytes!("./data/uuid_nil.bin"))
    }
    #[test]
    fn sample() {
        test_success(
            uuid!("c2cc10e1-57d6-4b6f-9899-38d972112d8c"),
            include_bytes!("./data/uuid_1.bin"),
        )
    }

    #[test]
    fn v4() {
        // from rfc9562
        test_success(
            uuid!("919108f7-52d1-3320-5bac-f847db4148a8"),
            include_bytes!("./data/uuid_v4.bin"),
        )
    }

    #[test]
    fn v7() {
        // from rfc9562
        test_success(
            uuid!("017f22e2-79b0-7cc3-98c4-dc0c0c07398f"),
            include_bytes!("./data/uuid_v7.bin"),
        )
    }

    #[test]
    fn max() {
        test_success(Uuid::max(), include_bytes!("./data/uuid_max.bin"))
    }
}

mod gcbor_set {
    use webar_core::codec::gcbor::set::GCborSet;

    use crate::test_success;

    #[test]
    fn empty() {
        test_success(
            GCborSet::<i8>::new(),
            include_bytes!("./data/set_empty.bin"),
        )
    }

    #[test]
    fn s123() {
        test_success(
            GCborSet::from([-3, -2, -1, 0, 1, 2, 3]),
            include_bytes!("./data/set_123.bin"),
        )
    }
}

mod gcbor_map {
    use webar_core::codec::gcbor::map::GCborMap;

    use crate::test_success;

    #[test]
    fn empty() {
        test_success(
            GCborMap::<i8, u8>::new(),
            include_bytes!("./data/map_empty.bin"),
        )
    }

    #[test]
    fn m123() {
        test_success(
            GCborMap::from([
                (-3i8, 6u8),
                (-2, 5),
                (-1, 4),
                (0, 0),
                (1, 1),
                (2, 2),
                (3, 3),
            ]),
            include_bytes!("./data/map_123.bin"),
        )
    }
}

mod digest {
    use webar_core::digest::{Digest, Sha256};

    use crate::test_success;

    /// sha256 of string abc
    const SHA256_ABC: Sha256 = Sha256(hex_literal::hex!(
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    ));

    #[test]
    fn sha256_abc() {
        test_success(SHA256_ABC, include_bytes!("./data/sha256_abc.bin"))
    }

    #[test]
    fn digest_sha256_abc() {
        test_success(
            Digest::Sha256(SHA256_ABC),
            include_bytes!("./data/sha256_abc.bin"),
        )
    }
}
