use std::{fmt, hint::unreachable_unchecked};

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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    /// Address Register Indirect with Postincrement `(An)+`
    AddrRegIndirectWPostincr(AddrReg),
    /// Address Register Indirect with Predecrement `-(An)`
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
            Self::AddrRegIndirectWPostincr(reg) => write!(f, "({})+", reg),
            Self::AddrRegIndirectWPredec(reg) => write!(f, "-({})", reg),
            Self::AddrRegIndirectWDispl(disp, reg) => write!(f, "{}({})", disp, reg),
            Self::AbsShortData(addr) => write!(f, "{}.w", addr),
            Self::AbsLongData(addr) => write!(f, "{}.l", addr),
            Self::ImmediateData(data) => write!(f, "#{}", data),
        }
    }
}

#[derive(Debug)]
pub enum Ins {
    /// ADDI
    AddImmediate {
        size: Size,
        data: u32,
        ea: EffectiveAddr,
    },
    /// CMPI
    CompareImmediate {
        size: Size,
        data: u32,
        ea: EffectiveAddr,
    },
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
    /// EXT
    SignExtend(Size, DataReg),
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
    /// OR
    Or {
        size: Size,
        dreg: DataReg,
        ea: EffectiveAddr,
        flipped: bool,
    },
    /// CMP
    Compare {
        size: Size,
        reg: DataReg,
        ea: EffectiveAddr,
    },
    /// CMPA
    CompareAddr {
        size: Size,
        reg: AddrReg,
        ea: EffectiveAddr,
    },
    /// CMPM
    CompareMultiple {
        size: Size,
        areg_x: AddrReg,
        areg_y: AddrReg,
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
    /// ASd
    ArithmeticShift {
        dir: ShiftDir,
        size: Size,
        count: ShiftCount,
        reg: DataReg,
    },
}

impl fmt::Display for Ins {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AddImmediate { size, data, ea } => {
                write!(f, "ADDI.{} #{},{}", size, data, ea)
            }
            Self::CompareImmediate { size, data, ea } => {
                write!(f, "CMPI.{} #{},{}", size, data, ea)
            }
            Self::MoveAddress { size, src, dest } => {
                write!(f, "MOVEA.{} {},{}", size, src, dest)
            }
            Self::Move { size, src, dest } => {
                write!(f, "MOVE.{} {},{}", size, src, dest)
            }
            Self::Clear(ea) => write!(f, "CLR {}", ea),
            Self::SignExtend(size, reg) => write!(f, "EXT.{} {}", size, reg),
            Self::MoveMultiple {
                dir,
                size,
                ea,
                mask,
            } => match dir {
                MoveMultipleDir::RegToMem => write!(f, "MOVEM.{} {:016b} {}", size, mask, ea),
                MoveMultipleDir::MemToReg => write!(f, "MOVEM.{} {} {:016b}", size, ea, mask),
            },
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
            Self::Or {
                size,
                dreg,
                ea,
                flipped,
            } => {
                if *flipped {
                    write!(f, "OR.{} {},{}", size, dreg, ea)
                } else {
                    write!(f, "OR.{} {},{}", size, ea, dreg)
                }
            }
            Self::Compare { size, reg, ea } => write!(f, "CMP.{} {},{}", size, ea, reg),
            Self::CompareAddr { size, reg, ea } => write!(f, "CMPA.{} {},{}", size, ea, reg),
            Self::CompareMultiple {
                size,
                areg_y,
                areg_x,
            } => write!(f, "CMPM.{} ({})+,({})+", size, areg_y, areg_x),
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
            Self::ArithmeticShift {
                dir,
                size,
                count,
                reg,
            } => {
                write!(f, "AS{}.{} {},{}", dir, size, count, reg)
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
            _ => unsafe { unreachable_unchecked() },
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftDir {
    Right,
    Left,
}

impl fmt::Display for ShiftDir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Right => write!(f, "R"),
            Self::Left => write!(f, "L"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftCount {
    Immediate(u8),
    Reg(DataReg),
}

impl fmt::Display for ShiftCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Immediate(count) => write!(f, "#{}", count),
            Self::Reg(reg) => write!(f, "{}", reg),
        }
    }
}
