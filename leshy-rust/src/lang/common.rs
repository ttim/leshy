#[derive(Clone, Debug)]
pub struct Bytes {
    content: Vec<u8>,
}

impl Bytes {
    pub fn from_i32(value: i32) -> Bytes {
        Bytes { content: value.to_le_bytes().to_vec() }
    }

    pub fn from_i64(value: i64) -> Bytes {
        Bytes { content: value.to_le_bytes().to_vec() }
    }

    pub fn from_bytes(value: Vec<u8>) -> Bytes {
        Bytes { content: value }
    }

    pub fn from_string(value: String) -> Bytes {
        Bytes { content: value.into_bytes() }
    }

    pub fn as_string(&self) -> Option<String> {
        match std::str::from_utf8(&self.content) {
            Ok(str) =>
                if str.chars().all(|ch| { char::is_ascii_alphanumeric(&ch) || char::is_ascii_whitespace(&ch)}) {
                    Option::Some(String::from(str))
                } else {
                    Option::None
                }
            Err(_) =>
                Option::None,
        }
    }

    pub fn as_i32(&self) -> Option<i32> {
        if self.content.len() == 4 {
            // todo: doesn't seem to be efficient?
            self.content[0..4].try_into().map(i32::from_le_bytes).ok()
        } else {
            Option::None
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        if self.content.len() == 8 {
            // todo: doesn't seem to be efficient
            self.content[0..8].try_into().map(i64::from_le_bytes).ok()
        } else {
            Option::None
        }
    }

    pub fn as_base64(&self) -> String {
        base64::encode(&self.content)
    }
}

impl ToString for Bytes {
    fn to_string(&self) -> String {
        if let Some(s) = self.as_string() {
            return s;
        }
        if let Some(i32) = self.as_i32() {
            return i32.to_string();
        }
        if let Some(i64) = self.as_i64() {
            return i64.to_string() + "_L";
        }
        return self.as_base64();
    }
}

#[test]
fn decode_encode_i32() {
    let bytes = Bytes::from_i32(4);
    assert_eq!(Some(4), bytes.as_i32());
    assert_eq!(None, bytes.as_i64());
    assert_eq!(None, bytes.as_string());
    assert_eq!(String::from("BAAAAA=="), bytes.as_base64());
}

#[test]
fn decode_encode_i64() {
    let bytes = Bytes::from_i64(16);
    assert_eq!(None, bytes.as_i32());
    assert_eq!(Some(16), bytes.as_i64());
    assert_eq!(None, bytes.as_string());
    assert_eq!(String::from("EAAAAAAAAAA="), bytes.as_base64());
}

#[test]
fn decode_encode_string() {
    let bytes = Bytes::from_string(String::from("hello"));
    assert_eq!(None, bytes.as_i32());
    assert_eq!(None, bytes.as_i64());
    assert_eq!(Some(String::from("hello")), bytes.as_string());
    assert_eq!(String::from("aGVsbG8="), bytes.as_base64());
}
