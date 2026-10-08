use std::ffi::CStr;

use crate::FilePath;
pub struct TracingFilePaths {
    pub dir: &'static CStr,
    pub log_full_txt: &'static CStr,
    pub log_pretty_txt: &'static CStr,
    pub log_json: &'static CStr,
    pub log_cbor: &'static CStr,
    pub log_gcbor: &'static CStr,
}

pub mod connection {
    use std::ffi::CStr;

    use crate::FilePath;

    #[derive(Debug)]
    pub struct CaptureFilePaths {
        pub event_path: &'static CStr,
        pub tx_path: &'static CStr,
        pub rx_path: &'static CStr,
    }

    pub const METADATA: FilePath = FilePath::new_throw(c"meta.gcbor");

    pub const TCP_LOG_FILES: CaptureFilePaths = CaptureFilePaths {
        event_path: c"tcp_events.gcborseq",
        tx_path: c"stream_tx.data.bin",
        rx_path: c"stream_rx.data.bin",
    };

    pub const TLS_LOG_FILES: CaptureFilePaths = CaptureFilePaths {
        event_path: c"tls_io_events.gcborseq",
        tx_path: c"tls_tx.data.bin",
        rx_path: c"tls_rx.data.bin",
    };
    pub const TLS_INFO_FILE: FilePath = FilePath::new_throw(c"tls_info.gcbor");

    pub const PROXY_LOG_FILES: CaptureFilePaths = CaptureFilePaths {
        event_path: c"stream_io_events.gcborseq",
        tx_path: c"stream_tx.data.bin",
        rx_path: c"stream_rx.data.bin",
    };
}

pub mod connector {
    use crate::FilePath;

    pub const CONNECTION_DIR: FilePath = FilePath::new_throw(c"connection");

    pub const SSL_KEYLOG_CBOR: FilePath = FilePath::new_throw(c"sslkeylog.gcbor");
    pub const SSL_KEYLOG_TXT: FilePath = FilePath::new_throw(c"sslkeylog.txt");

    pub const HTTP_REQUESTS: FilePath = FilePath::new_throw(c"http_messages.gcbor");

    pub const DUMPCAP_DIR: FilePath = FilePath::new_throw(c"dumpcap");
}

pub const TRACING_MAIN: TracingFilePaths = TracingFilePaths {
    dir: c"tracing-main",
    log_gcbor: c"tracing-main/log.gcborseq",
    log_pretty_txt: c"tracing-main/text_pretty.log.txt",
    log_full_txt: c"tracing-main/text_full.log.txt",
    log_cbor: c"tracing-main/log-serde.cborseq",
    log_json: c"tracing-main/log.json",
};
pub const TRACING_CONNECTOR: TracingFilePaths = TracingFilePaths {
    dir: c"tracing-connector",
    log_gcbor: c"tracing-connector/log.gcborseq",
    log_pretty_txt: c"tracing-connector/text_pretty.log.txt",
    log_full_txt: c"tracing-connector/text_full.log.txt",
    log_cbor: c"tracing-connector/log-serde.cborseq",
    log_json: c"tracing-connector/log.json",
};

pub const CONNECTORS_DIR: FilePath = FilePath::new_throw(c"connector");

pub const FETCH_INFO: FilePath = FilePath::new_throw(c"info.cbor");

pub const BLOB_INCREMENTAL_STORE: FilePath = FilePath::new_throw(c"blob/incremental");
pub const BLOB_FULL_STORE: FilePath = FilePath::new_throw(c"blob/full");
pub const BLOB_INCREMENTAL_INFO_FILE: FilePath = FilePath::new_throw(c"blob/incremental.bin");

pub const HTTP_DATA: FilePath = FilePath::new_throw(c"http.tar");
