use crate::FilePath;

pub const SSL_KEYLOG_BIN: FilePath = FilePath::new_throw(c"sslkeylog.bin");
pub const SSL_KEYLOG_TXT: FilePath = FilePath::new_throw(c"sslkeylog.txt");
