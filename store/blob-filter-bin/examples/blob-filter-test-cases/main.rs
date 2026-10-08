#[path = "../../tests/zip/testcases.rs"]
mod zip_tests;

fn write_zip_test(test: &zip_tests::TestCase, input_name: &str, output_name: &str) {
    std::fs::write(input_name, &test.input).unwrap();
    std::fs::write(output_name, &test.output).unwrap();
}
macro_rules! write_zip_case {
    ($i:ident) => {
        write_zip_test(
            &(zip_tests::$i()),
            std::concat!(std::stringify!($i), ".in.zip"),
            std::concat!(std::stringify!($i), ".exp.zip")
        )
    };
}

fn main() {
    let dir = {
        let mut args = std::env::args_os();
        args.next().unwrap();
        args.next()
    };
    if let Some(dir) = dir {
        std::env::set_current_dir(dir).unwrap();
    }
    write_zip_case!(dir_plain);
    write_zip_case!(dir_with_comment);
    write_zip_case!(dir_with_extra_data);
    write_zip_case!(symlink_plain);
    write_zip_case!(symlink_with_comment);
    write_zip_case!(symlink_multi);
    write_zip_case!(regular_keep0);
    write_zip_case!(regular_keep_with_comment);
    write_zip_case!(regular_replace0);
    write_zip_case!(regular_replaced_with_comment);
    write_zip_case!(regular_replaced_in_dir);
    write_zip_case!(file_multi);
}
