use crate::FilePath;

pub const SSL_KEYLOG_BIN: FilePath = FilePath::new_throw(c"sslkeylog.bin");
pub const SSL_KEYLOG_TXT: FilePath = FilePath::new_throw(c"sslkeylog.txt");

pub const WIRESHARK_DATA_FILE: FilePath = FilePath::new_throw(c"traffic.pcapng");
pub const WIRESHARK_LOG_FILE: FilePath = FilePath::new_throw(c"dumpcap.log");

pub const TRACING_DIR: FilePath = FilePath::new_throw(c"tracing");
pub const TRACING_LOG_GCBOR: FilePath = FilePath::new_throw(c"tracing/gcbor.log.bin");
pub const TRACING_LOG_CBOR: FilePath = FilePath::new_throw(c"tracing/serde.log.bin");
pub const TRACING_LOG_PRETTY_TXT: FilePath = FilePath::new_throw(c"tracing/text_pretty.log.txt");
pub const TRACING_LOG_FULL_TXT: FilePath = FilePath::new_throw(c"tracing/text_full.log.txt");
pub const TRACING_LOG_JSON: FilePath = FilePath::new_throw(c"tracing/json.log.json");
