pub enum M68KI {
    OR,
    ANDI,
    SUBI,
    ADDI,
    EORI,
}

pub fn test(first: u8) -> M68KI {
    match first {
        0b_0000_0000 => M68KI::OR,
        0b_0000_0001 | 0b_0000_0011 | 0b_0000_0101 | 0b_0000_0111 | 0b_0000_1001 => {
            // BTST, BCHG, BSET (dynamic) or MOVEP
            todo!()
        }
        0b_0000_0010 => M68KI::ANDI,
        0b_0000_0100 => M68KI::SUBI,
        0b_0000_0110 => M68KI::ADDI,
        0b_0000_1000 => {
            // BTST, BCHG, BCLR, BSET (static)
            todo!()
        }
        0b_0000_1010 => M68KI::EORI,
        _ => panic!(),
    }
}
