

pub fn is_letter(ch: u8) -> bool {
    return b'a' <= ch && ch <= b'z' || b'A' <= ch && b'Z' <= ch;
}

pub fn is_digit(ch: u8) -> bool {
    return b'0' <= ch && ch <= b'9';
}
