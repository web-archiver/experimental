/*! String in normal form

Currently only ascii character is allowed.
*/
use std::{fmt::Display, mem::transmute};

pub use crate::nf_str;

#[derive(Debug, thiserror::Error)]
#[error("Str contains non-ascii character")]
pub struct NFStrError();

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NFStr(str);
impl Display for NFStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}
impl NFStr {
    pub const fn from_str(s: &str) -> Result<&Self, NFStrError> {
        if s.is_ascii() {
            Ok(unsafe { transmute(s) })
        } else {
            Err(NFStrError())
        }
    }
    pub const fn as_str(&self) -> &str {
        &self.0
    }
    pub fn to_nf_string(&self) -> NFString {
        NFString(self.0.to_owned())
    }
}

#[doc(hidden)]
pub mod internal {
    pub use super::NFStr;
    pub use std::result::Result;
}

#[macro_export]
macro_rules! nf_str {
    ($l:expr) => {{
        use $crate::text::normalized::{internal, NFStr};
        const S: &'static NFStr = match NFStr::from_str($l) {
            internal::Result::Ok(v) => v,
            internal::Result::Err(_) => panic!("Invalid string"),
        };
        S
    }};
}

#[derive(Debug, thiserror::Error)]
#[error("String contains non-ascii character")]
pub struct NFStringError();
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NFString(String);
impl Display for NFString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}
impl NFString {
    pub fn from_string(s: String) -> Result<Self, NFStringError> {
        if s.is_ascii() {
            Ok(Self(s))
        } else {
            Err(NFStringError())
        }
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
    pub fn as_nf_str(&self) -> &NFStr {
        unsafe { transmute(self.0.as_str()) }
    }
}
