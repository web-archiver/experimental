use crate::FilePath;

pub const SSL_KEYLOG_BIN: FilePath = FilePath::new_throw(c"sslkeylog.bin");
pub const SSL_KEYLOG_TXT: FilePath = FilePath::new_throw(c"sslkeylog.txt");
pub const WIRESHARK_DATA_FILE: FilePath = FilePath::new_throw(c"traffic.pcapng");
pub const WIRESHARK_LOG_FILE: FilePath = FilePath::new_throw(c"dumpcap.log");
