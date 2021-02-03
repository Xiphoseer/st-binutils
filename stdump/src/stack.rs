use std::fmt;

use m68k::isa::{EffectiveAddr, Size};

#[derive(Debug, Clone, Copy)]
pub enum Word {
    Unknown,
    Immediate(i16),
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unknown => write!(f, "???"),
            Self::Immediate(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Long {
    Unknown,
    Reloc(u32),
    Immediate(i32),
}

impl Long {
    fn high(&self) -> Word {
        match self {
            Self::Unknown => Word::Unknown,
            Self::Reloc(_) => Word::Unknown,
            Self::Immediate(v) => {
                let b = v.to_be_bytes();
                Word::Immediate(i16::from_be_bytes([b[0], b[1]]))
            }
        }
    }

    fn mid(&self) -> Word {
        match self {
            Self::Unknown => Word::Unknown,
            Self::Reloc(_) => Word::Unknown,
            Self::Immediate(v) => {
                let b = v.to_be_bytes();
                Word::Immediate(i16::from_be_bytes([b[1], b[2]]))
            }
        }
    }

    fn low(&self) -> Word {
        match self {
            Self::Unknown => Word::Unknown,
            Self::Reloc(_) => Word::Unknown,
            Self::Immediate(v) => {
                let b = v.to_be_bytes();
                Word::Immediate(i16::from_be_bytes([b[2], b[3]]))
            }
        }
    }
}

impl fmt::Display for Long {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unknown => write!(f, "???"),
            Self::Reloc(v) => write!(f, "{}.lr", v),
            Self::Immediate(v) => write!(f, "#{}", v),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Byte {
    Unknown,
    Immediate(i8),
}

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unknown => write!(f, "???"),
            Self::Immediate(v) => write!(f, "#{}", v),
        }
    }
}

#[derive(Debug)]
pub struct Stack<'a> {
    reloc: &'a [u32],
    base: Address,
    inner: Vec<StackOp>,
}

impl<'a> fmt::Display for Stack<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.base)?;
        for line in &self.inner {
            writeln!(f, "- {:?}", line)?;
        }
        Ok(())
    }
}

impl<'a> Stack<'a> {
    pub fn set_base(&mut self, new: Address) {
        self.base = new;
        self.inner.clear();
    }

    pub fn discard(&mut self, mut cnt: u32) {
        while cnt > 0 {
            match self.inner.pop() {
                Some(StackOp::Long(_)) if cnt >= 4 => {
                    cnt -= 4;
                }
                Some(StackOp::Word(_)) if cnt >= 2 => {
                    cnt -= 2;
                }
                None => return,
                v => todo!("{:?}", v),
            }
        }
    }

    pub fn new(reloc: &'a [u32]) -> Self {
        Self {
            reloc,
            base: Address::Initial,
            inner: Vec::new(),
        }
    }

    pub fn word_at(&self, mut offset: usize) -> Word {
        let mut iter = self.inner.iter();
        while let Some(op) = iter.next_back() {
            match op {
                StackOp::Word(w) if offset == 0 => {
                    return *w;
                }
                StackOp::Word(_) if offset == 1 => {
                    panic!("{:?}", self)
                }
                StackOp::Word(_) => {
                    offset -= 2;
                }
                StackOp::Long(l) if offset == 0 => {
                    return l.high();
                }
                StackOp::Long(l) if offset == 1 => {
                    return l.mid();
                }
                StackOp::Long(l) if offset == 2 => {
                    return l.low();
                }
                StackOp::Long(_) if offset == 3 => {
                    panic!("{:?}", self)
                }
                StackOp::Long(_) => {
                    offset -= 4;
                }
                _ => panic!("{:?}", self),
            }
        }
        Word::Unknown
    }

    pub fn long_at(&self, mut offset: usize) -> Long {
        let mut iter = self.inner.iter();
        while let Some(op) = iter.next_back() {
            match op {
                StackOp::Long(l) if offset == 0 => {
                    return *l;
                }
                StackOp::Long(_) if offset >= 4 => {
                    offset -= 4;
                }
                StackOp::Word(_) if offset >= 2 => {
                    offset -= 2;
                }
                _ => panic!("{:?}", op),
            }
        }
        Long::Unknown
    }

    pub fn push(&mut self, size: Size, ea: EffectiveAddr) {
        match ea {
            EffectiveAddr::DataRegDirect(_) | EffectiveAddr::AddrRegDirect(_) => match size {
                Size::Byte => self.inner.push(StackOp::Byte(Byte::Unknown)),
                Size::Word => self.inner.push(StackOp::Word(Word::Unknown)),
                Size::Long => self.inner.push(StackOp::Long(Long::Unknown)),
            },
            EffectiveAddr::ImmediateData(v) => {
                let b = v.to_be_bytes();
                match size {
                    Size::Byte => self
                        .inner
                        .push(StackOp::Byte(Byte::Immediate(i8::from_be_bytes([b[3]])))),
                    Size::Word => {
                        self.inner
                            .push(StackOp::Word(Word::Immediate(i16::from_be_bytes([
                                b[2], b[3],
                            ]))))
                    }
                    Size::Long => {
                        self.inner
                            .push(StackOp::Long(Long::Immediate(i32::from_be_bytes([
                                b[0], b[1], b[2], b[3],
                            ]))))
                    }
                }
            }
            EffectiveAddr::AbsLongData { addr, relocated } => {
                assert_eq!(size, Size::Long);
                self.inner.push(StackOp::Long(if relocated {
                    Long::Reloc(addr)
                } else {
                    Long::Immediate(i32::from_be_bytes(addr.to_be_bytes()))
                }));
            }
            EffectiveAddr::AddrRegIndirectWDispl(_, _) | EffectiveAddr::AddrRegIndirect(_) => {
                self.inner.push(match size {
                    Size::Long => StackOp::Long(Long::Unknown),
                    Size::Word => StackOp::Word(Word::Unknown),
                    Size::Byte => StackOp::Byte(Byte::Unknown),
                });
            }
            _ => todo!("{:?}: {}", size, ea),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Address {
    Unknown,
    Initial,
    Fixed(u32),
}

impl Default for Address {
    fn default() -> Self {
        Self::Unknown
    }
}

#[derive(Debug, Copy, Clone)]
pub enum StackOp {
    Word(Word),
    Long(Long),
    Byte(Byte),
}
