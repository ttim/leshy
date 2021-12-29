pub fn cmp_eq(len: usize, r1: &[u8], r2: &[u8]) -> bool {
    match len {
        4 => { get_i32(r1) == get_i32(r2) }
        _ => todo!()
    }
}

pub fn cmp_le(len: usize, r1: &[u8], r2: &[u8]) -> bool {
    match len {
        4 => { get_i32(r1) < get_i32(r2) }
        _ => todo!()
    }
}

pub fn cmp_gt(len: usize, r1: &[u8], r2: &[u8]) -> bool {
    match len {
        4 => { get_i32(r1) > get_i32(r2) }
        _ => todo!()
    }
}

pub fn bytes_as_i32(bytes: &[u8]) -> Option<i32> {
    if bytes.len() != 4 { return None }
    get_i32(bytes)
}

// // Should it be DeRef or AsRef? Or Borrow/BorrowMut? Or AsRef/AsMut?
// pub fn add(length: usize, op1: *[u8], op2: *[u8], dst: *mut [u8]) {
//     match length {
//         4 => {
//             put_i32(dst.as_mut(), get_i32(op1.as_ref()) + get_i32(op2.as_ref()))
//         }
//         _ => { todo!() }
//     }
// }

pub fn get_i32(bytes: &[u8]) -> Option<i32> {
    bytes[0..4].try_into().map(i32::from_le_bytes).ok()
}
pub fn put_i32(bytes: &mut [u8], value: i32) {
    bytes[0..4].copy_from_slice(value.to_le_bytes().as_slice())
}

pub fn bytes_as_i64(bytes: &[u8]) -> Option<i64> {
    if bytes.len() != 8 { return None }
    get_i64(bytes)
}

pub fn get_i64(bytes: &[u8]) -> Option<i64> {
    bytes[0..8].try_into().map(i64::from_le_bytes).ok()
}

pub fn bytes_from_i32(value: i32) -> Vec<u8> {
    value.to_le_bytes().to_vec()
}

pub fn bytes_from_i64(value: i64) -> Vec<u8> {
    value.to_le_bytes().to_vec()
}

pub fn bytes_as_base64(bytes: &[u8]) -> String {
    base64::encode(&bytes)
}

pub fn bytes_as_string(bytes: &[u8]) -> Option<String> {
    match std::str::from_utf8(bytes) {
        Ok(str) => {
            if str
                .chars()
                .all(|ch| char::is_ascii_alphanumeric(&ch) || char::is_ascii_whitespace(&ch))
            {
                Option::Some(String::from(str))
            } else {
                Option::None
            }
        }
        Err(_) => Option::None,
    }
}

pub fn bytes_to_string(bytes: &[u8]) -> String {
    if let Some(s) = bytes_as_string(bytes) {
        return s;
    }
    if let Some(i32) = bytes_as_i32(bytes) {
        return i32.to_string();
    }
    if let Some(i64) = bytes_as_i64(bytes) {
        return i64.to_string() + "_L";
    }
    bytes_as_base64(bytes)
}

#[test]
fn decode_encode_i32() {
    let bytes = bytes_from_i32(4);
    assert_eq!(Some(4), bytes_as_i32(&bytes));
    assert_eq!(None, bytes_as_i64(&bytes));
    assert_eq!(None, bytes_as_string(&bytes));
    assert_eq!(String::from("BAAAAA=="), bytes_as_base64(&bytes));
}

//#[test]
//fn decode_encode_i64() {
//    let bytes = Bytes::from_i64(16);
//    assert_eq!(None, bytes.as_i32());
//    assert_eq!(Some(16), bytes.as_i64());
//    assert_eq!(None, bytes.as_string());
//    assert_eq!(String::from("EAAAAAAAAAA="), bytes.as_base64());
//}
//
//#[test]
//fn decode_encode_string() {
//    let bytes = Bytes::from_string(String::from("hello"));
//    assert_eq!(None, bytes.as_i32());
//    assert_eq!(None, bytes.as_i64());
//    assert_eq!(Some(String::from("hello")), bytes.as_string());
//    assert_eq!(String::from("aGVsbG8="), bytes.as_base64());
//}
