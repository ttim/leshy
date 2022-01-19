use std::io::{Read, Seek};

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error(Box<dyn std::error::Error>);

impl<T: std::error::Error + 'static> From<T> for Error {
    fn from(error: T) -> Self {
        Error(Box::from(error))
    }
}

pub fn read_u32(src: &mut (impl Read + Seek)) -> Result<u32> {
    Ok(u32::try_from(leb128::read::unsigned(src)?)?)
}

pub fn read_i32(src: &mut (impl Read + Seek)) -> Result<i32> {
    Ok(i32::try_from(leb128::read::signed(src)?)?)
}

pub fn read_u8(src: &mut (impl Read + Seek)) -> Result<u8> {
    let mut buf: [u8; 1] = [0; 1];
    src.read_exact(&mut buf)?;
    Ok(buf[0])
}

pub fn read_vector<T: Read + Seek, V, F: Fn(&mut T) -> Result<V>>(src: &mut T, parse_once: F) -> Result<Vec<V>> {
    let len = read_u32(src)?;
    let mut result = Vec::with_capacity(len as usize);
    for _ in 0..len {
        result.push(parse_once(src)?)
    }
    Ok(result)
}

pub fn read_string(src: &mut (impl Read + Seek)) -> Result<String> {
    Ok(String::from_utf8(read_vector(src, |src| read_u8(src))?)?)
}