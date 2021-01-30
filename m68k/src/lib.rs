use std::{fmt, hint::unreachable_unchecked};

pub enum M68KI {
    OR,
    ANDI,
    SUBI,
    ADDI,
    EORI,
}

#[derive(Debug, Clone, Copy)]
pub enum AddrReg {
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    SP,
}

impl AddrReg {
    /// Interprets the last 3 bits as an address register
    pub fn from_bits(byte: u8) -> Self {
        match byte & 0b0000_0111 {
            0 => AddrReg::A0,
            1 => AddrReg::A1,
            2 => AddrReg::A2,
            3 => AddrReg::A3,
            4 => AddrReg::A4,
            5 => AddrReg::A5,
            6 => AddrReg::A6,
            7 => AddrReg::SP,
            _ => unsafe { unreachable_unchecked() },
        }
    }
}

impl fmt::Display for AddrReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

#[repr(u8)]
#[derive(Debug)]
pub enum DataReg {
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
}

impl DataReg {
    /// Interprets the last 3 bits as an address register
    pub fn from_bits(byte: u8) -> Self {
        match byte & 0b0000_0111 {
            0 => DataReg::D0,
            1 => DataReg::D1,
            2 => DataReg::D2,
            3 => DataReg::D3,
            4 => DataReg::D4,
            5 => DataReg::D5,
            6 => DataReg::D6,
            7 => DataReg::D7,
            _ => unsafe { unreachable_unchecked() },
        }
    }
}

impl fmt::Display for DataReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

#[derive(Debug)]
pub enum EffectiveAddr {
    /// Address Register Direct `An`
    AddrRegDirect(AddrReg),
    /// Address Register Direct `Dn`
    DataRegDirect(DataReg),
    /// Address Register Indirect `010`
    AddrRegIndirect(AddrReg),
    /// Address Register Indirect with Predecrement
    AddrRegIndirectWPredec(AddrReg),
    /// Address Register Indirect with Displacement `(u16, An)`
    AddrRegIndirectWDispl(i16, AddrReg),
    /// Absolute Short Data `<address>.w`
    AbsShortData(i16),
    /// Absolute Long Data `<address>.l`
    AbsLongData(u32),
    /// Immediate Data `#<data>`
    ImmediateData(u32),
}

impl fmt::Display for EffectiveAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AddrRegDirect(reg) => write!(f, "{}", reg),
            Self::DataRegDirect(reg) => write!(f, "{}", reg),
            Self::AddrRegIndirect(reg) => write!(f, "({})", reg),
            Self::AddrRegIndirectWPredec(reg) => write!(f, "-({})", reg),
            Self::AddrRegIndirectWDispl(disp, reg) => write!(f, "{:#x}({})", disp, reg),
            Self::AbsShortData(addr) => write!(f, "{}.w", addr),
            Self::AbsLongData(addr) => write!(f, "{}.l", addr),
            Self::ImmediateData(data) => write!(f, "#{}", data),
        }
    }
}

#[derive(Debug)]
pub enum Ins {
    /// MOVEA
    MoveAddress {
        size: Size,
        src: EffectiveAddr,
        dest: AddrReg,
    },
    /// MOVE
    Move {
        size: Size,
        src: EffectiveAddr,
        dest: EffectiveAddr,
    },
    /// CLR
    Clear(EffectiveAddr),
    /// MOVEM
    MoveMultiple {
        dir: MoveMultipleDir,
        size: Size,
        ea: EffectiveAddr,
        mask: u16,
    },
    /// TST
    Test { size: Size, ea: EffectiveAddr },
    /// TRAP
    Trap(u8),
    /// LINK
    LinkAndAllocate { areg: AddrReg, displacement: i16 },
    /// UNLK
    Unlink(AddrReg),
    /// RTS
    ReturnFromSubroutine,
    /// JSR
    JumpToSubroutine(EffectiveAddr),
    /// BRA
    BranchAlways(i16),
    /// BSR
    BranchToSubroutine(i16),
    /// Bcc
    Branch(Condition, i16),
    /// MOVEQ
    MoveQuick(i8, DataReg),
    /// ADDQ
    AddQuick {
        size: Size,
        ea: EffectiveAddr,
        data: u8,
    },
    /// SUBQ
    SubQuick {
        size: Size,
        ea: EffectiveAddr,
        data: u8,
    },
    /// AND
    And {
        size: Size,
        dreg: DataReg,
        ea: EffectiveAddr,
        flipped: bool,
    },
    /// ADD
    Add {
        size: Size,
        dreg: DataReg,
        ea: EffectiveAddr,
        flipped: bool,
    },
    /// ADDA
    AddAddress {
        size: Size,
        dest: AddrReg,
        src: EffectiveAddr,
    },
}

impl fmt::Display for Ins {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MoveAddress { size, src, dest } => {
                write!(f, "MOVEA.{} {},{}", size, src, dest)
            }
            Self::Move { size, src, dest } => {
                write!(f, "MOVE.{} {},{}", size, src, dest)
            }
            Self::Clear(ea) => write!(f, "CLR {}", ea),
            Self::MoveMultiple { dir, size, ea, mask} => match dir {
                MoveMultipleDir::RegToMem => write!(f, "MOVEM.{} {:016b} {}", size, mask, ea),
                MoveMultipleDir::MemToReg => write!(f, "MOVEM.{} {} {:016b}", size, ea, mask),
            }
            Self::Test { size, ea } => write!(f, "TST.{} {}", size, ea),
            Self::Trap(vector) => {
                write!(f, "TRAP #{}", vector)
            }
            Self::LinkAndAllocate { areg, displacement } => {
                write!(f, "LINK {},#{}", areg, displacement)
            }
            Self::Unlink(areg) => write!(f, "UNLK {}", areg),
            Self::ReturnFromSubroutine => write!(f, "RTS"),
            Self::JumpToSubroutine(ea) => write!(f, "JSR {}", ea),
            Self::BranchAlways(disp) => write!(f, "BRA @{}", disp),
            Self::BranchToSubroutine(disp) => write!(f, "BSR @{}", disp),
            Self::Branch(cond, disp) => write!(f, "B{} {}", cond, disp),
            Self::MoveQuick(data, reg) => write!(f, "MOVEQ.l #{},{}", data, reg),
            Self::AddQuick { size, data, ea } => {
                write!(f, "ADDQ.{} #{},{}", size, data, ea)
            }
            Self::SubQuick { size, data, ea } => {
                write!(f, "SUBQ.{} #{},{}", size, data, ea)
            }
            Self::And {
                size,
                dreg,
                ea,
                flipped,
            } => {
                if *flipped {
                    write!(f, "AND.{} {},{}", size, dreg, ea)
                } else {
                    write!(f, "AND.{} {},{}", size, ea, dreg)
                }
            }
            Self::Add {
                size,
                dreg,
                ea,
                flipped,
            } => {
                if *flipped {
                    write!(f, "ADD.{} {},{}", size, dreg, ea)
                } else {
                    write!(f, "ADD.{} {},{}", size, ea, dreg)
                }
            }
            Self::AddAddress { size, src, dest } => {
                write!(f, "ADDA.{} {},{}", size, src, dest)
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Condition {
    /// RA
    Always = 0b0000,
    /// SR
    ToSubroutine = 0b0001,
    /// HI
    High = 0b0010,
    /// LS
    LowOrSame = 0b0011,
    
    /// CC
    CarryClear = 0b0100,
    /// CS
    CarrySet = 0b0101,
    /// NE
    NotEqual = 0b0110,
    /// EQ
    Equal = 0b0111,
    
    /// VC
    OverflowClear = 0b1000,
    /// VS
    OverflowSet = 0b1001,
    /// PL
    Plus = 0b1010,
    /// MI
    Minus = 0b1011,

    /// GE
    GreaterOrEqual = 0b1100,
    /// LT
    LessThan = 0b1101,
    /// GT
    GreaterThan = 0b1110,
    /// LE
    LessOrEqual = 0b1111,
}

impl Condition {
    fn code(&self) -> &'static str {
        match self {
            Self::Always => "RA",
            Self::ToSubroutine => "SR",
            Self::High => "HI",
            Self::LowOrSame => "LS",
            
            Self::CarryClear => "CC",
            Self::CarrySet => "CS",
            Self::NotEqual => "NE",
            Self::Equal => "EQ",
            
            Self::OverflowClear => "VC",
            Self::OverflowSet => "VS",
            Self::Plus => "PL",
            Self::Minus => "MI",
        
            Self::GreaterOrEqual => "GE",
            Self::LessThan => "LT",
            Self::GreaterThan => "GT",
            Self::LessOrEqual => "LE",
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code())
    }
}

impl Condition {
    pub fn from_bits(byte: u8) -> Self {
        match byte & 0b1111 {
            0b0000 => Self::Always,
            0b0001 => Self::ToSubroutine,
            0b0010 => Self::High,
            0b0011 => Self::LowOrSame,
            
            0b0100 => Self::CarryClear,
            0b0101 => Self::CarrySet,
            0b0110 => Self::NotEqual,
            0b0111 => Self::Equal,
            
            0b1000 => Self::OverflowClear,
            0b1001 => Self::OverflowSet,
            0b1010 => Self::Plus,
            0b1011 => Self::Minus,

            0b1100 => Self::GreaterOrEqual,
            0b1101 => Self::LessThan,
            0b1110 => Self::GreaterThan,
            0b1111 => Self::LessOrEqual,
            _ => unsafe { unreachable_unchecked() }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MoveMultipleDir {
    RegToMem,
    MemToReg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Size {
    Byte,
    Word,
    Long,
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Byte => write!(f, "b"),
            Self::Word => write!(f, "w"),
            Self::Long => write!(f, "l"),
        }
    }
}

pub struct Decoder<'a> {
    offset: usize,
    idata: &'a [u8],
    inner: std::slice::Iter<'a, u8>,
}

fn size_high(second: u8) -> Option<Size> {
    match second {
        0b_0000_0000..=0b_0011_1111 => Some(Size::Byte),
        0b_0100_0000..=0b_0111_1111 => Some(Size::Word),
        0b_1000_0000..=0b_1011_1111 => Some(Size::Long),
        0b_1100_0000..=0b_1111_1111 => None,
    }
}

impl<'a> Decoder<'a> {
    pub fn new(idata: &'a [u8]) -> Self {
        Self {
            offset: 0,
            idata,
            inner: idata.iter(),
        }
    }

    fn try_next_i16(&mut self) -> Option<i16> {
        let a = *self.inner.next()?;
        let b = *self.inner.next()?;
        Some(i16::from_be_bytes([a, b]))
    }

    fn try_next_u16(&mut self) -> Option<u16> {
        let a = *self.inner.next()?;
        let b = *self.inner.next()?;
        Some(u16::from_be_bytes([a, b]))
    }

    fn try_next_u32(&mut self) -> Option<u32> {
        let a = *self.inner.next()?;
        let b = *self.inner.next()?;
        let c = *self.inner.next()?;
        let d = *self.inner.next()?;
        Some(u32::from_be_bytes([a, b, c, d]))
    }

    fn next_i16(&mut self) -> i16 {
        self.try_next_i16().unwrap()
    }

    fn next_u16(&mut self) -> u16 {
        self.try_next_u16().unwrap()
    }

    fn next_u32(&mut self) -> u32 {
        self.try_next_u32().unwrap()
    }

    fn lea(&mut self, byte: u8, size: Size) -> EffectiveAddr {
        match byte & 0b_0011_1111 {
            0b_0000_0000..=0b_0000_0111 => EffectiveAddr::DataRegDirect(DataReg::from_bits(byte)),
            0b_0000_1000..=0b_0000_1111 => EffectiveAddr::AddrRegDirect(AddrReg::from_bits(byte)),
            0b_0001_0000..=0b_0001_0111 => EffectiveAddr::AddrRegIndirect(AddrReg::from_bits(byte)),
            0b_0010_0000..=0b_0010_0111 => EffectiveAddr::AddrRegIndirectWPredec(AddrReg::from_bits(byte)),
            0b_0010_1000..=0b_0010_1111 => {
                let disp: i16 = self.next_i16();
                EffectiveAddr::AddrRegIndirectWDispl(disp, AddrReg::from_bits(byte))
            }
            0b_0011_1100 => match size {
                Size::Byte | Size::Word => {
                    let val: u32 = self.next_u16().into();
                    EffectiveAddr::ImmediateData(val)
                }
                Size::Long => {
                    let val: u32 = self.next_u32();
                    EffectiveAddr::ImmediateData(val)
                }
            },
            0b_0011_1000 => EffectiveAddr::AbsShortData(self.next_i16()),
            0b_0011_1001 => EffectiveAddr::AbsLongData(self.next_u32()),
            _ => panic!("lea: {:06b}", byte & 0b_0011_1111),
        }
    }

    fn next_ins(&mut self) -> Option<Ins> {
        if let Some(&first) = self.inner.next() {
            let &second = self.inner.next().unwrap();
            match first {
                0b_0001_0000..=0b0011_1111 => {
                    let size = match first >> 4 & 0b11 {
                        0b10 => Size::Long,
                        0b11 => Size::Word,
                        _ => Size::Byte, // 00 is impossible
                    };
                    // MOVE
                    if first % 2 == 0 {
                        match second {
                            0b_0100_0000..=0b_0111_1111 => {
                                assert_ne!(size, Size::Byte);
                                Some(Ins::MoveAddress {
                                    size,
                                    src: self.lea(second, size),
                                    dest: AddrReg::from_bits(first >> 1),
                                })
                            },
                            0b_0000_0000..=0b_0011_1111 => Some(Ins::Move {
                                size,
                                src: self.lea(second, size),
                                dest: EffectiveAddr::DataRegDirect(DataReg::from_bits(first >> 1)),
                            }),
                            0b_1000_0000..=0b_1011_1111 => Some(Ins::Move {
                                size,
                                src: self.lea(second, size),
                                dest: EffectiveAddr::AddrRegIndirect(AddrReg::from_bits(first >> 1)),
                            }),
                            _ => panic!("Unknown Opcode {:08b} {:08b}", first, second),
                        }
                    } else {
                        match second {
                            0b_0000_0000..=0b_0011_1111 => {
                                let reg = AddrReg::from_bits(first >> 1);
                                let src = self.lea(second, size);
                                Some(Ins::Move {
                                    size,
                                    src,
                                    dest: EffectiveAddr::AddrRegIndirectWPredec(reg),
                                })
                            }
                            0b_1100_0000..=0b_1111_1111 => {
                                let dest_bits = (first >> 1) & 0b111;
                                let src = self.lea(second, size);
                                let dest: EffectiveAddr = match dest_bits {
                                    0b000 => EffectiveAddr::AbsShortData(self.next_i16()),
                                    0b001 => EffectiveAddr::AbsLongData(self.next_u32()),
                                    _ => panic!("MOVE dest is mode {:03b}", dest_bits),
                                };
                                Some(Ins::Move { size, src, dest })
                            }
                            _ => panic!("Unknown Opcode {:08b} {:08b}", first, second),
                        }
                    }

                    //input = rest;
                    //continue;
                }
                0b_0100_0010 => {
                    if let Some(size) = size_high(second) {
                        let ea = self.lea(second, size);
                        Some(Ins::Clear(ea))
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0100_1000 | 0b_0100_1100 => {
                    if second & 0b_1000_0000 > 0 {
                        let dir = if first & 0b100 != 0 {
                            MoveMultipleDir::MemToReg
                        } else {
                            MoveMultipleDir::RegToMem
                        };
                        let size = if second & 0b_1000_0000 != 0 {
                            Size::Long
                        } else {
                            Size::Word
                        };
                        let ea = self.lea(second, size); // TODO: check if order is correct
                        let mask = self.next_u16();
                        Some(Ins::MoveMultiple {
                            dir,
                            size,
                            ea,
                            mask,
                        })
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0100_1010 => {
                    if let Some(size) = size_high(second) {
                        // TST
                        let ea = self.lea(second, size);
                        Some(Ins::Test { size, ea })
                    } else {
                        panic!("TAS {:08b} {:08b}", first, second);
                    }
                }
                // 0b_0100_1100 => MOVEM
                0b_0100_1110 => {
                    // Special (TRAP, ...)
                    match second {
                        0b_0100_0000..=0b_0100_1111 => Some(Ins::Trap(second & 0b1111)),
                        0b_0101_0000..=0b_0101_0111 => {
                            let areg = AddrReg::from_bits(second);
                            let displacement = self.next_i16();
                            Some(Ins::LinkAndAllocate { areg, displacement })
                        }
                        0b_0101_1000..=0b_0101_1111 => {
                            let areg = AddrReg::from_bits(second);
                            Some(Ins::Unlink(areg))
                        }
                        0b_0111_0101 => Some(Ins::ReturnFromSubroutine),
                        0b_1000_0000..=0b1011_1111 => {
                            let ea = self.lea(second, Size::Byte); // TODO: restrict, i.e. size shouldn't be needed here
                            Some(Ins::JumpToSubroutine(ea))
                        }

                        _ => panic!("SPECIAL: {:08b}", second),
                    }
                }
                0b_0101_0000..=0b_0101_1111 => {
                    if let Some(size) = size_high(second) {
                        let is_sub = first & 1 > 0;
                        let data = (first >> 1) & 0b111;
                        let data = if data == 0 { 8 } else { data };
                        let ea = self.lea(second, size);
                        if is_sub {
                            Some(Ins::SubQuick { size, data, ea })
                        } else {
                            Some(Ins::AddQuick { size, data, ea })
                        }
                    } else {
                        panic!("DBcc, Scc: {:08b} {:08b}", first, second);
                    }
                }
                0b_0110_0000..=0b_0110_1111 => {
                    let cond = Condition::from_bits(first);
                    let displacement: i16 = if second == 0 {
                        self.next_i16()
                    } else {
                        i8::from_be_bytes([second]).into()
                    };
                    match cond {
                        Condition::Always => {
                            Some(Ins::BranchAlways(displacement))
                        }
                        Condition::ToSubroutine => {
                            Some(Ins::BranchToSubroutine(displacement))
                        }
                        _ => Some(Ins::Branch(cond, displacement)),
                    }
                    
                },
                0b_0111_0000..=0b_0111_1111 => {
                    if first & 1 == 0 {
                        let reg = DataReg::from_bits(first >> 1);
                        let data = i8::from_be_bytes([second]);
                        Some(Ins::MoveQuick(data, reg))
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_1100_0000..=0b_1100_1111 => {
                    if let Some(size) = size_high(second) {
                        let flipped = first & 1 > 0;
                        let dreg = DataReg::from_bits(first >> 1);
                        let ea = self.lea(second, size);
                        Some(Ins::And {
                            size,
                            dreg,
                            ea,
                            flipped,
                        })
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_1101_0000..=0b_1101_1111 => {
                    if let Some(size) = size_high(second) {
                        let flipped = first & 1 > 0;
                        let dreg = DataReg::from_bits(first >> 1);
                        let ea = self.lea(second, size);
                        Some(Ins::Add {
                            size,
                            dreg,
                            ea,
                            flipped,
                        })
                    } else {
                        // ADDA
                        let size = if first & 1 == 1 {
                            Size::Long
                        } else {
                            Size::Word
                        };
                        let dest = AddrReg::from_bits(first >> 1);
                        let src = self.lea(second, size);
                        Some(Ins::AddAddress { size, src, dest })
                    }
                }
                _ => panic!("{:08b} {:08b}", first, second),
            }
        } else {
            None
        }
    }
}

impl<'a> Iterator for Decoder<'a> {
    type Item = (usize, &'a [u8], Ins);
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ins) = self.next_ins() {
            let off = self.offset;
            let slice = self.inner.as_slice();
            let len = self.idata.len() - slice.len();
            self.offset += len;
            let (ins_bytes, rest) = self.idata.split_at(len);
            self.idata = rest;
            Some((off, ins_bytes, ins))
        } else {
            None
        }
    }
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
