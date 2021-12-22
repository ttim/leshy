use crate::lang::common::Bytes;

#[derive(Clone)]
pub enum Const {
    Literal { bytes: Bytes },
    Stack { from_offset: Bytes, length: Bytes },
    Symbol { name: String },
}

impl ToString for Const {
    fn to_string(&self) -> String {
        match self {
            Const::Literal { bytes } =>
                bytes.to_string(),
            Const::Stack { from_offset, length } =>
                String::from("${") + &from_offset.to_string() + ", " + &length.to_string() + "}",
            Const::Symbol { name } =>
                String::from(":") + name,
        }
    }
}

#[test]
fn test_to_string() {
    assert_eq!(String::from("4"), Const::Literal { bytes: Bytes::from_i32(4) }.to_string());
    assert_eq!(String::from("${4, 16_L}"), Const::Stack { from_offset: Bytes::from_i32(4), length: Bytes::from_i64(16) }.to_string());
    assert_eq!(String::from(":symbol"), Const::Symbol { name: String::from("symbol") }.to_string());
}
