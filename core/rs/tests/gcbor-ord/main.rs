use proptest::prelude::TestCaseError;

use webar_core::codec::gcbor::{to_vec, GCborOrd, ToGCbor};

fn test_prop<T: ToGCbor + GCborOrd + ?Sized>(v1: &T, v2: &T) -> Result<(), TestCaseError> {
    proptest::prop_assert_eq!(v1.cmp_gcbor(v2), to_vec(v1).cmp(&to_vec(v2)));
    Ok(())
}
fn test_fixed<T: ToGCbor + GCborOrd + ?Sized>(v1: &T, v2: &T) {
    assert_eq!(v1.cmp_gcbor(v2), to_vec(v1).cmp(&to_vec(v2)))
}
macro_rules! simple_prop {
    ($n:ident, $t: ty) => {
        proptest::proptest! {
            #[test]
            fn $n(v1: $t, v2: $t) {
                crate::test_prop(&v1, &v2).unwrap();
            }
        }
    };
}

mod text {
    use webar_core::text::normalized::NFStr;

    proptest::proptest! {
        #[test]
        fn nf_str(s1 in "[[:ascii:]]*", s2 in "[[:ascii:]]*") {
            crate::test_prop(NFStr::new(&s1).unwrap(), NFStr::new(&s2).unwrap()).unwrap();
        }
    }

    mod std_string {

        use crate::test_fixed;

        const S_ABC: &str = "abc";
        const S_BA: &str = "ba";
        const S_EMPTY: &str = "";
        const S_A_0308: &str = "a\u{0308}";
        const S_E_0323_0302: &str = "e\u{0323}\u{0302}";

        #[test]
        fn nn_abc_empty() {
            test_fixed(S_ABC, S_EMPTY)
        }
        #[test]
        fn nn_abc_ba() {
            test_fixed(S_ABC, S_BA)
        }

        #[test]
        fn un_abc_a() {
            test_fixed(S_ABC, S_A_0308)
        }

        #[test]
        fn nu_e_ba() {
            test_fixed(S_E_0323_0302, S_BA)
        }

        #[test]
        fn uu_a_e() {
            test_fixed(S_A_0308, S_E_0323_0302)
        }
    }
}

mod integer {
    simple_prop!(u8, u8);
    simple_prop!(u16, u16);
    simple_prop!(u32, u32);
    simple_prop!(u64, u64);

    simple_prop!(i8, i8);
    #[test]
    fn negative_overflow() {
        crate::test_fixed(&-128i8, &-1i8)
    }
    simple_prop!(i16, i16);
    simple_prop!(i32, i32);
    simple_prop!(i64, i64);
}

proptest::proptest! {
    #[test]
    fn uuid_(v1: u128, v2: u128) {
        crate::test_prop(&uuid::Uuid::from_u128(v1), &uuid::Uuid::from_u128(v2)).unwrap();
    }
}

mod bool {
    use crate::test_fixed;

    #[test]
    fn false_false() {
        test_fixed(&false, &false)
    }
    #[test]
    fn false_true() {
        test_fixed(&false, &true)
    }
    #[test]
    fn true_false() {
        test_fixed(&true, &false)
    }
    #[test]
    fn true_true() {
        test_fixed(&true, &true)
    }
}

#[test]
fn unit() {
    test_fixed(&(), &())
}

mod transparent_derive {
    use crate::test_fixed;

    #[derive(Debug, PartialEq, Eq, webar_core_macros::GCborCodec, webar_core_macros::GCborOrd)]
    #[gcbor(transparent)]
    struct Normal(i8);

    #[test]
    fn sample() {
        test_fixed(&Normal(-1), &Normal(2))
    }
}

mod digest {
    use webar_core::digest::{Digest, Sha256};

    use crate::test_prop;

    proptest::proptest! {
        #[test]
        fn sha256(s1: [u8; 32], s2: [u8; 32]) {
            test_prop(&Sha256(s1), &Sha256(s2)).unwrap()
        }

        #[test]
        fn digest(s1: [u8; 32], s2: [u8; 32]) {
            test_prop(&Digest::Sha256(Sha256(s1)), &Digest::Sha256(Sha256(s2))).unwrap()
        }
    }
}
