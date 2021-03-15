/// A 8-bit signed integer
pub type CHAR = i8;
/// A 8-bit unsigned integer
pub type BYTE = u8;
/// A 16-bit unsigned integer
pub type WORD = u16;
/// A 16-bit signed integer
pub type SHORT = i16;
/// A 32-bit unsigned integer
pub type DWORD = u32;
/// A 32-bit signed integer
pub type LONG = i32;

pub mod dis;
pub mod isa;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        assert_eq!(std::mem::size_of::<(isa::AddrReg, isa::DataReg)>(), 1);
    }
}
