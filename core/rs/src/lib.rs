#![deny(clippy::unicode_not_nfc)]
pub mod bytes;

pub mod digest;

pub mod codec {
    pub mod gcbor;
}

pub mod text {
    pub mod normalized;
}

pub mod time;
