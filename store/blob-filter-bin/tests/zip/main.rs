mod testcases;

macro_rules! mk_test {
    ($i:ident) => {
        #[test]
        fn $i() {
            testcases::$i().test()
        }
    };
}

mk_test!(dir_plain);
mk_test!(dir_with_comment);
mk_test!(dir_with_extra_data);

mk_test!(symlink_plain);
mk_test!(symlink_with_comment);
mk_test!(symlink_multi);

mk_test!(regular_keep0);
mk_test!(regular_keep_with_comment);

mk_test!(regular_replace0);
mk_test!(regular_replaced_with_comment);
mk_test!(regular_replaced_in_dir);

mk_test!(file_multi);
