use bitflags::bitflags;
use ins::{
    ALUOp, AddrReg, Condition, DataReg, EffectiveAddr, Ins, MoveMultipleDir, Reg, ShiftCount,
    ShiftDir, ShiftKind, Size,
};

pub mod ins;

bitflags! {
    struct EAFlags: u32 {
        /// Data Register Direct
        const DN = 0b1;
        /// Address Register Direct
        const AN = 0b10;
        const ANI = 0b100;
        const ANIPI = 0b1000;
        const ANIPD = 0b10000;
        const ANID = 0b100000;
        const ANII = 0b1000000;
        const ASD = 0b10000000;
        const ALD = 0b100000000;
        const PCRD = 0b1000000000;
        const PCRI = 0b10000000000;
        const IMM = 0b100000000000;

        const AD = Self::ALD.bits | Self::ASD.bits;
        /// addr register indirect - pre/post dec/inc
        const ANIP = Self::ANIPI.bits | Self::ANIPD.bits;
        /// addr register indirect - relative
        const ANIR = Self::ANID.bits | Self::ANII.bits;
        /// addr register indirect - all
        const ANIX = Self::ANI.bits | Self::ANIP.bits | Self::ANIR.bits;
        /// PC relative
        const PCR = Self::PCRD.bits | Self::PCRI.bits;

        /// memory alterable addressing modes
        const MEM_ALT = Self::ANIX.bits | Self::AD.bits;
        /// data alterable addressing modes
        const DATA_ALT = Self::MEM_ALT.bits | Self::DN.bits;
        /// data addressing modes
        const DATA_ADDR = Self::DATA_ALT.bits | Self::PCR.bits | Self::IMM.bits;
        /// control addressing modes
        const CTRL_ALT = Self::ANI.bits | Self::ANIR.bits | Self::AD.bits;
        /// control alterable addressing modes
        const CTRL_ADDR = Self::CTRL_ALT.bits | Self::PCR.bits;

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

    fn try_next_u8_i8(&mut self) -> Option<(u8, i8)> {
        let a = *self.inner.next()?;
        let b = *self.inner.next()?;
        Some((a, i8::from_be_bytes([b])))
    }

    fn try_next_i8(&mut self) -> Option<i8> {
        self.inner.next()?;
        let b = *self.inner.next()?;
        Some(i8::from_be_bytes([b]))
    }

    fn try_next_u8(&mut self) -> Option<u8> {
        self.inner.next()?;
        let b = *self.inner.next()?;
        Some(b)
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

    fn try_next_i32(&mut self) -> Option<i32> {
        let a = *self.inner.next()?;
        let b = *self.inner.next()?;
        let c = *self.inner.next()?;
        let d = *self.inner.next()?;
        Some(i32::from_be_bytes([a, b, c, d]))
    }

    fn try_next_u32(&mut self) -> Option<u32> {
        let a = *self.inner.next()?;
        let b = *self.inner.next()?;
        let c = *self.inner.next()?;
        let d = *self.inner.next()?;
        Some(u32::from_be_bytes([a, b, c, d]))
    }

    fn next_i8(&mut self) -> i8 {
        self.try_next_i8().unwrap()
    }

    #[allow(unused)]
    fn next_u8(&mut self) -> u8 {
        self.try_next_u8().unwrap()
    }

    fn next_u8_i8(&mut self) -> (u8, i8) {
        self.try_next_u8_i8().unwrap()
    }

    fn next_i16(&mut self) -> i16 {
        self.try_next_i16().unwrap()
    }

    fn next_u16(&mut self) -> u16 {
        self.try_next_u16().unwrap()
    }

    fn next_i32(&mut self) -> i32 {
        self.try_next_i32().unwrap()
    }

    fn next_u32(&mut self) -> u32 {
        self.try_next_u32().unwrap()
    }

    fn lea(&mut self, byte: u8, size: Size, flags: EAFlags) -> EffectiveAddr {
        match byte & 0b_0011_1111 {
            0b_0000_0000..=0b_0000_0111 => {
                assert!(flags.contains(EAFlags::DN));
                EffectiveAddr::DataRegDirect(DataReg::from_bits(byte))
            }
            0b_0000_1000..=0b_0000_1111 => {
                assert!(flags.contains(EAFlags::AN));
                EffectiveAddr::AddrRegDirect(AddrReg::from_bits(byte))
            }
            0b_0001_0000..=0b_0001_0111 => {
                assert!(flags.contains(EAFlags::ANI));
                EffectiveAddr::AddrRegIndirect(AddrReg::from_bits(byte))
            }
            0b_0001_1000..=0b_0001_1111 => {
                assert!(flags.contains(EAFlags::ANIPI));
                EffectiveAddr::AddrRegIndirectWPostincr(AddrReg::from_bits(byte))
            }
            0b_0010_0000..=0b_0010_0111 => {
                assert!(flags.contains(EAFlags::ANIPD));
                EffectiveAddr::AddrRegIndirectWPredec(AddrReg::from_bits(byte))
            }
            0b_0010_1000..=0b_0010_1111 => {
                assert!(flags.contains(EAFlags::ANID));
                let disp: i16 = self.next_i16();
                EffectiveAddr::AddrRegIndirectWDispl(disp, AddrReg::from_bits(byte))
            }
            0b_0011_0000..=0b_0011_0111 => {
                assert!(flags.contains(EAFlags::ANII));
                let (mode, disp) = self.next_u8_i8();
                let areg = AddrReg::from_bits(byte);
                // FIXME: is this the correct mode decoding?
                let xreg = if mode & 0b_1000_0000 == 0 {
                    Reg::Data(DataReg::from_bits(mode >> 4))
                } else {
                    Reg::Addr(AddrReg::from_bits(mode >> 4))
                };
                let size = if mode & 0b_1000 == 0 {
                    Size::Word
                } else {
                    Size::Long
                };
                EffectiveAddr::AddrRegIndirectWIndex(disp, areg, xreg, size)
            }
            0b_0011_1000 => {
                assert!(flags.contains(EAFlags::ASD));
                EffectiveAddr::AbsShortData(self.next_i16())
            }
            0b_0011_1001 => {
                assert!(flags.contains(EAFlags::ALD));
                EffectiveAddr::AbsLongData(self.next_u32())
            }
            0b_0011_1100 => {
                assert!(flags.contains(EAFlags::IMM));
                match size {
                    Size::Byte | Size::Word => {
                        let val: u32 = self.next_u16().into();
                        EffectiveAddr::ImmediateData(val)
                    }
                    Size::Long => {
                        let val: u32 = self.next_u32();
                        EffectiveAddr::ImmediateData(val)
                    }
                }
            }
            _ => panic!("lea: {:06b}", byte & 0b_0011_1111),
        }
    }

    fn next_ins(&mut self) -> Option<Ins> {
        if let Some(&first) = self.inner.next() {
            let &second = self.inner.next().unwrap();
            match first {
                0b_0000_0000 => {
                    // ORI
                    if let Some(size) = size_high(second) {
                        let data = match size {
                            Size::Byte => self.next_i8().into(),
                            Size::Word => self.next_i16().into(),
                            Size::Long => self.next_i32(),
                        };
                        /*if second & 0b_0011_1000 == 0b_0000_1000 {
                            panic!("Unknown ORI {:08b} {:08b}", first, second);
                        }*/
                        let ea = self.lea(second, size, EAFlags::DATA_ALT | EAFlags::AN); // FIXME: An not allowed by spec
                        Some(Ins::ALUImmediate {
                            op: ALUOp::Or,
                            size,
                            data,
                            ea,
                        })
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0000_0001..=0b_0000_1111 if first & 1 == 1 => {
                    // BTST, BCHG, BCLR, BSET
                    match second {
                        0b_0000_0000..=0b_0011_1111 => panic!("BTST {:08b} {:08b}", first, second),
                        0b_0100_0000..=0b_0111_1111 => panic!("BCHG {:08b} {:08b}", first, second),
                        0b_1000_0000..=0b_1011_1111 => panic!("BCLR {:08b} {:08b}", first, second),
                        0b_1100_0000..=0b_1111_1111 => panic!("BSET {:08b} {:08b}", first, second),
                    }
                }
                0b_0000_0010 => {
                    // ANDI
                    if let Some(size) = size_high(second) {
                        let data = match size {
                            Size::Byte => self.next_i8().into(),
                            Size::Word => self.next_i16().into(),
                            Size::Long => self.next_i32(),
                        };
                        let ea = self.lea(second, size, EAFlags::DATA_ALT);
                        Some(Ins::ALUImmediate {
                            op: ALUOp::And,
                            size,
                            data,
                            ea,
                        })
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0000_0100 => {
                    // SUBI
                    if let Some(size) = size_high(second) {
                        let data = match size {
                            Size::Byte => self.next_i8().into(),
                            Size::Word => self.next_i16().into(),
                            Size::Long => self.next_i32(),
                        };
                        let ea = self.lea(second, size, EAFlags::DATA_ALT);
                        Some(Ins::ALUImmediate {
                            op: ALUOp::Sub,
                            size,
                            data,
                            ea,
                        })
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0000_0110 => {
                    // ADDI
                    if let Some(size) = size_high(second) {
                        let data = match size {
                            Size::Byte => self.next_i8().into(),
                            Size::Word => self.next_i16().into(),
                            Size::Long => self.next_i32(),
                        };
                        let ea = self.lea(second, size, EAFlags::DATA_ALT);
                        Some(Ins::ALUImmediate {
                            op: ALUOp::Add,
                            size,
                            data,
                            ea,
                        })
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0000_1000 => {
                    // BTST, BCHG, BCLR, BSET (static)
                    match second {
                        0b_0000_0000..=0b_0011_1111 => {
                            let bit = self.next_u8();
                            let ea = self.lea(second, Size::Long, EAFlags::DATA_ADDR);
                            Some(Ins::BitTest(ShiftCount::Immediate(bit), ea))
                        }
                        0b_0100_0000..=0b_0111_1111 => panic!("BCHG (static) {:08b}", second),
                        0b_1000_0000..=0b_1011_1111 => {
                            let bit = self.next_u8();
                            let ea = self.lea(second, Size::Long, EAFlags::DATA_ALT);
                            Some(Ins::BitClear(ShiftCount::Immediate(bit), ea))
                        }
                        0b_1100_0000..=0b_1111_1111 => {
                            let bit = self.next_u8();
                            let ea = self.lea(second, Size::Long, EAFlags::DATA_ALT);
                            Some(Ins::BitSet(ShiftCount::Immediate(bit), ea))
                        }
                    }
                }
                0b_0000_1100 => {
                    // CMPI
                    if let Some(size) = size_high(second) {
                        let data = match size {
                            Size::Byte => (self.next_u16() & 0b_1111_1111).into(),
                            Size::Word => self.next_u16().into(),
                            Size::Long => self.next_u32(),
                        };
                        let ea = self.lea(second, size, EAFlags::DATA_ALT);
                        Some(Ins::CompareImmediate { size, data, ea })
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0001_0000..=0b0011_1111 => {
                    let size = match first >> 4 & 0b11 {
                        0b10 => Size::Long,
                        0b11 => Size::Word,
                        _ => Size::Byte, // 00 is impossible
                    };
                    // MOVE
                    if first % 2 == 0 {
                        match second {
                            0b_0000_0000..=0b_0011_1111 => Some(Ins::Move {
                                size,
                                src: self.lea(second, size, EAFlags::all()),
                                dest: EffectiveAddr::DataRegDirect(DataReg::from_bits(first >> 1)),
                            }),
                            0b_0100_0000..=0b_0111_1111 => {
                                assert_ne!(size, Size::Byte, "{:08b} {:08b}", first, second);
                                Some(Ins::MoveAddress {
                                    size,
                                    src: self.lea(second, size, EAFlags::all()),
                                    dest: AddrReg::from_bits(first >> 1),
                                })
                            }
                            0b_1000_0000..=0b_1011_1111 => Some(Ins::Move {
                                size,
                                src: self.lea(second, size, EAFlags::all()),
                                dest: EffectiveAddr::AddrRegIndirect(AddrReg::from_bits(
                                    first >> 1,
                                )),
                            }),
                            0b_1100_0000..=0b_1111_1111 => {
                                let dest = EffectiveAddr::AddrRegIndirectWPostincr(
                                    AddrReg::from_bits(first >> 1),
                                );
                                Some(Ins::Move {
                                    size,
                                    src: self.lea(second, size, EAFlags::all()),
                                    dest,
                                })
                            }
                        }
                    } else {
                        match second {
                            0b_0000_0000..=0b_0011_1111 => {
                                let reg = AddrReg::from_bits(first >> 1);
                                let src = self.lea(second, size, EAFlags::all());
                                Some(Ins::Move {
                                    size,
                                    src,
                                    dest: EffectiveAddr::AddrRegIndirectWPredec(reg),
                                })
                            }
                            0b_0100_0000..=0b_0111_1111 => {
                                let reg = AddrReg::from_bits(first >> 1);
                                let src = self.lea(second, size, EAFlags::all());
                                let disp = self.next_i16();
                                let dest = EffectiveAddr::AddrRegIndirectWDispl(disp, reg);
                                Some(Ins::Move { size, src, dest })
                            }
                            0b_1100_0000..=0b_1111_1111 => {
                                let dest_bits = (first >> 1) & 0b111;
                                let src = self.lea(second, size, EAFlags::all());
                                let dest: EffectiveAddr = match dest_bits {
                                    0b000 => EffectiveAddr::AbsShortData(self.next_i16()),
                                    0b001 => EffectiveAddr::AbsLongData(self.next_u32()),
                                    _ => panic!("MOVE dest is mode 111 {:03b}", dest_bits),
                                };
                                Some(Ins::Move { size, src, dest })
                            }
                            _ => panic!("Unknown MOVE.{} {:08b} {:08b}", size, first, second),
                        }
                    }

                    //input = rest;
                    //continue;
                }
                0b_0100_0000..=0b_0100_1111
                    if first & 1 == 1 && second & 0b_1100_0000 == 0b_1100_0000 =>
                {
                    let reg = AddrReg::from_bits(first >> 1);
                    let ea = self.lea(second, Size::Byte, EAFlags::CTRL_ADDR);
                    Some(Ins::LoadEffectiveAddr(ea, reg))
                }
                0b_0100_0010 => {
                    if let Some(size) = size_high(second) {
                        let ea = self.lea(second, size, EAFlags::DATA_ADDR);
                        Some(Ins::Clear(size, ea))
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0100_0100 => {
                    // NEG
                    if let Some(size) = size_high(second) {
                        let ea = self.lea(second, size, EAFlags::DATA_ALT);
                        Some(Ins::Negate(size, ea))
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0100_0110 => {
                    // NOT
                    if let Some(size) = size_high(second) {
                        let ea = self.lea(second, size, EAFlags::DATA_ALT);
                        Some(Ins::Not(size, ea))
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                //0b_0100_1000 01000 => SWAP
                0b_0100_1000 | 0b_0100_1100 => {
                    if second & 0b_1000_0000 != 0 {
                        if first & 0b100 == 0 && second & 0b111000 == 0 {
                            let size = if second & 0b_0100_0000 != 0 {
                                Size::Long
                            } else {
                                Size::Word
                            };
                            let reg = DataReg::from_bits(second);
                            return Some(Ins::SignExtend(size, reg));
                        }
                        let (dir, flags) = if first & 0b100 != 0 {
                            (
                                MoveMultipleDir::MemToReg,
                                EAFlags::CTRL_ADDR | EAFlags::ANIPI,
                            )
                        } else {
                            (
                                MoveMultipleDir::RegToMem,
                                EAFlags::CTRL_ALT | EAFlags::ANIPD,
                            )
                        };
                        let size = if second & 0b_1000_0000 != 0 {
                            Size::Long
                        } else {
                            Size::Word
                        };
                        let ea = self.lea(second, size, flags); // TODO: check if order is correct
                        let mask = self.next_u16();
                        Some(Ins::MoveMultiple {
                            dir,
                            size,
                            ea,
                            mask,
                        })
                    } else if first & 0b100 == 0 && second & 0b0111_1000 == 0b_0100_0000 {
                        let reg = DataReg::from_bits(second);
                        Some(Ins::Swap(reg))
                    } else if first & 0b100 == 0 && second & 0b0100_0000 == 0b_0000_0000 {
                        let ea = self.lea(second, Size::Long, EAFlags::DATA_ALT);
                        Some(Ins::NegateBCD(ea))
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_0100_1010 => {
                    if let Some(size) = size_high(second) {
                        // TST
                        let ea = self.lea(second, size, EAFlags::DN | EAFlags::ANIX | EAFlags::AD);
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
                            let ea = self.lea(second, Size::Byte, EAFlags::CTRL_ADDR);
                            Some(Ins::JumpToSubroutine(ea))
                        }
                        0b_1100_0000..=0b_1111_1111 => {
                            let ea = self.lea(second, Size::Long, EAFlags::CTRL_ADDR);
                            Some(Ins::Jump(ea))
                        }
                        _ => panic!("SPECIAL: {:08b}", second),
                    }
                }
                0b_0101_0000..=0b_0101_1111 => {
                    if let Some(size) = size_high(second) {
                        let is_sub = first & 1 > 0;
                        let data = (first >> 1) & 0b111;
                        let data = if data == 0 { 8 } else { data };
                        let mut flags = EAFlags::DATA_ADDR;
                        if size != Size::Byte {
                            flags |= EAFlags::AN;
                        }
                        let ea = self.lea(second, size, flags);
                        if is_sub {
                            Some(Ins::SubQuick { size, data, ea })
                        } else {
                            Some(Ins::AddQuick { size, data, ea })
                        }
                    } else {
                        match second {
                            0b_1100_1000..=0b_1100_1111 => {
                                let cond = Condition::from_bits(first);
                                let reg = DataReg::from_bits(second);
                                let disp = self.next_i16();
                                Some(Ins::TestDecrementBranch(cond, reg, disp))
                            }
                            _ => panic!("Scc ???: {:08b} {:08b}", first, second),
                        }
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
                        Condition::True => Some(Ins::BranchAlways(displacement)),
                        Condition::False => Some(Ins::BranchToSubroutine(displacement)),
                        _ => Some(Ins::Branch(cond, displacement)),
                    }
                }
                0b_0111_0000..=0b_0111_1111 => {
                    if first & 1 == 0 {
                        let reg = DataReg::from_bits(first >> 1);
                        let data = i8::from_be_bytes([second]);
                        Some(Ins::MoveQuick(data, reg))
                    } else {
                        panic!("Unknown Opcode {:08b} {:08b}", first, second);
                    }
                }
                0b_1000_0000..=0b_1000_1111 => {
                    // ORs
                    if let Some(size) = size_high(second) {
                        let flipped = first & 1 != 0;
                        let dreg = DataReg::from_bits(first >> 1);
                        let mut f = EAFlags::all() - EAFlags::AN;
                        if flipped {
                            f -= EAFlags::DN;
                        }
                        let ea = self.lea(second, size, f);
                        Some(Ins::ALU {
                            op: ALUOp::Or,
                            size,
                            dreg,
                            ea,
                            flipped,
                        })
                    } else {
                        let reg = DataReg::from_bits(first >> 1);
                        let signed = first & 1 != 0;
                        let ea = self.lea(second, Size::Word, EAFlags::DATA_ADDR);
                        Some(Ins::Divide { signed, reg, ea })
                    }
                }
                0b_1001_0000..=0b_1001_1111 => {
                    // SUBs
                    if let Some(size) = size_high(second) {
                        let flipped = first & 1 != 0;
                        let dreg = DataReg::from_bits(first >> 1);
                        let flags = if flipped {
                            EAFlags::MEM_ALT
                        } else if size == Size::Byte {
                            EAFlags::all() - EAFlags::AN
                        } else {
                            EAFlags::all()
                        };
                        let ea = self.lea(second, size, flags);
                        Some(Ins::ALU {
                            op: ALUOp::Sub,
                            size,
                            dreg,
                            ea,
                            flipped,
                        })
                    } else {
                        let size = if first & 1 == 0 {
                            Size::Word
                        } else {
                            Size::Long
                        };
                        let ea = self.lea(second, size, EAFlags::all());
                        let reg = AddrReg::from_bits(first >> 1);
                        Some(Ins::SubAddr { size, ea, reg })
                    }
                }
                0b_1011_0000..=0b_1011_1111 => {
                    if let Some(size) = size_high(second) {
                        if first & 1 == 0 {
                            let reg = DataReg::from_bits(first >> 1);
                            let ea = self.lea(second, size, EAFlags::all());
                            Some(Ins::Compare { size, reg, ea })
                        } else if second & 0b_0011_1000 == 0b0000_1000 {
                            let areg_x = AddrReg::from_bits(first >> 1);
                            let areg_y = AddrReg::from_bits(second);
                            Some(Ins::CompareMultiple {
                                size,
                                areg_x,
                                areg_y,
                            })
                        } else {
                            let reg = DataReg::from_bits(first >> 1);
                            let ea = self.lea(second, size, EAFlags::DATA_ALT);
                            Some(Ins::ExclusiveOr { size, reg, ea })
                        }
                    } else {
                        let size = if first & 1 == 0 {
                            Size::Word
                        } else {
                            Size::Long
                        };
                        let reg = AddrReg::from_bits(first >> 1);
                        let ea = self.lea(second, size, EAFlags::all());
                        Some(Ins::CompareAddr { size, reg, ea })
                    }
                }
                0b_1100_0000..=0b_1100_1111 => {
                    if let Some(size) = size_high(second) {
                        let flipped = first & 1 > 0;
                        let flags = if flipped {
                            EAFlags::MEM_ALT
                        } else {
                            EAFlags::DATA_ADDR
                        };
                        let dreg = DataReg::from_bits(first >> 1);
                        let ea = self.lea(second, size, flags);
                        Some(Ins::ALU {
                            op: ALUOp::And,
                            size,
                            dreg,
                            ea,
                            flipped,
                        })
                    } else {
                        let signed = first & 1 != 0;
                        let reg = DataReg::from_bits(first >> 1);
                        let ea = self.lea(second, Size::Word, EAFlags::DATA_ADDR);
                        Some(Ins::Multiply { signed, reg, ea })
                    }
                }
                0b_1101_0000..=0b_1101_1111 => {
                    if let Some(size) = size_high(second) {
                        let flipped = first & 1 > 0;
                        let flags = if flipped {
                            EAFlags::MEM_ALT
                        } else {
                            EAFlags::all()
                        };
                        let dreg = DataReg::from_bits(first >> 1);
                        let ea = self.lea(second, size, flags);
                        Some(Ins::ALU {
                            op: ALUOp::Add,
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
                        let src = self.lea(second, size, EAFlags::all());
                        Some(Ins::AddAddress { size, src, dest })
                    }
                }
                0b_1110_0000..=0b_1110_1111 => {
                    let dir = if first & 1 == 0 {
                        ShiftDir::Right
                    } else {
                        ShiftDir::Left
                    };
                    // The shifts
                    if let Some(size) = size_high(second) {
                        let count = if second & 0b_0010_0000 == 0 {
                            let val = first >> 1 & 0b111;
                            ShiftCount::Immediate(if val == 0 { 8 } else { val })
                        } else {
                            ShiftCount::Reg(DataReg::from_bits(first >> 1))
                        };
                        let reg = DataReg::from_bits(second);
                        if second & 0b1_0000 == 0 {
                            let kind = if second & 0b1000 == 0 {
                                ShiftKind::Arithmetic
                            } else {
                                ShiftKind::Logical
                            };

                            Some(Ins::Shift {
                                kind,
                                dir,
                                size,
                                count,
                                reg,
                            })
                        } else {
                            let with_extend = second & 0b1000 == 0;
                            Some(Ins::Rotate {
                                with_extend,
                                dir,
                                size,
                                count,
                                reg,
                            })
                        }
                    } else if first & 0b1110 == 0 {
                        let ea = self.lea(second, Size::Long, EAFlags::MEM_ALT);
                        Some(Ins::ArithmeticShiftMem { dir, ea })
                    } else {
                        panic!("Memory shifts {:08b} {:08b}", first, second);
                    }
                }
                _ => panic!("??? {:08b} {:08b}", first, second),
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
