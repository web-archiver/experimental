use proptest::prelude::TestCaseError;

use webar_core::{
    codec::gcbor::{to_vec, GCborOrd, ToGCbor},
    text::normalized::NFStr,
};

fn test_ord<T: ToGCbor + GCborOrd + ?Sized>(v1: &T, v2: &T) -> Result<(), TestCaseError> {
    proptest::prop_assert_eq!(v1.cmp_gcbor(v2), to_vec(v1).cmp(&to_vec(v2)));
    Ok(())
}

proptest::proptest! {
    #[test]
    fn nf_str(s1 in "[[:ascii:]]*", s2 in "[[:ascii:]]*") {
        test_ord(NFStr::from_str(&s1).unwrap(), NFStr::from_str(&s2).unwrap()).unwrap();
    }
}
