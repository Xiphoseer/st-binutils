use std::{collections::BTreeMap, path::PathBuf};

use m68k::{
    dis::Decoder,
    isa::{AddrReg, EffectiveAddr, Ins, Size},
};
use nom::{error::context, Finish};
use nom_supreme::{
    error::ErrorTree,
    final_parser::{ByteOffset, ExtractContext},
};
use parser::{p_prog, Program};
use stack::{Address, Long, Stack, Word};
use structopt::StructOpt;

mod parser;
mod stack;

#[derive(Debug, StructOpt)]
struct Options {
    /// The PRG file to analyze
    file: PathBuf,

    /// Prints the content of the DATA section
    #[structopt(long, short)]
    data: bool,

    /// Prints the content of the TEXT section
    #[structopt(long, short)]
    text: bool,

    /// Sets the offset to start disassembling from
    #[structopt(long, short)]
    offset: Option<usize>,

    /// Sets the maximum number of decoded functions
    #[structopt(long, short)]
    max: Option<usize>,
}

fn print_ascii(buf: &[u8]) {
    for &byte in buf {
        if byte.is_ascii() && byte > 31 {
            print!("{}", unsafe {
                std::char::from_u32_unchecked(u32::from(byte))
            });
        } else {
            print!(".");
        }
    }
}

fn print_buf(buf: &[u8]) {
    let mut iter = buf.chunks_exact(16);
    let mut line = 0;
    for chk in &mut iter {
        print!("{:08x}:", line);
        for byte in chk {
            print!(" {:02x}", byte);
        }
        print!(" | ");
        print_ascii(chk);
        println!();
        line += 16;
    }
    let rem = iter.remainder();
    if !rem.is_empty() {
        print!("{:08x}:", line);
        for byte in rem {
            print!(" {:02x}", byte);
        }
        for _ in rem.len()..16 {
            print!(" __");
        }
        print!(" | ");
        print_ascii(rem);
        println!();
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub struct TextInfo {
    op: Option<(usize, Ins)>,
    sr_call: Option<usize>,
    sr_start: bool,
}

fn disassemble(prog: &Program, offset: usize, ti_map: &mut BTreeMap<usize, TextInfo>) {
    let mut stack = Stack::new(&prog.reloc);

    let mut dec = Decoder::new(&prog.text, offset, &prog.reloc);
    while let Some((off, bytes, ins)) = dec.next() {
        print!("{:010} |", off);
        let mut cnt = 0;
        for &byte in bytes {
            let base = off + cnt;
            if prog.reloc.binary_search(&(base as u32)).is_ok() {
                print!("→");
            } else {
                print!(" ");
            }
            cnt += 1;
            print!("{:02x}", byte);
        }
        for _ in cnt..=8 {
            print!("   ");
        }
        print!(" | {}", &ins);
        let info = ti_map.entry(off).or_default();
        info.op = Some((off, ins));
        match ins {
            Ins::ReturnFromSubroutine => {
                break;
            }
            Ins::BranchToSubroutine(offset) => {
                let target: usize = (off + 2).wrapping_add(offset as usize);
                info.sr_call = Some(target);
                ti_map.entry(target).or_default().sr_start = true;
                print!("                sr{}()", target);
            }
            Ins::AddAddress { size, src, dest } => {
                if dest == AddrReg::SP {
                    assert_eq!(size, Size::Long);
                    match src {
                        EffectiveAddr::ImmediateData(off) => stack.discard(off),
                        _ => print!(" <--??-->"),
                    }
                }
            }
            Ins::MoveAddress { size, src, dest } => {
                if dest == AddrReg::SP {
                    assert_eq!(size, Size::Long);
                    match src {
                        EffectiveAddr::ImmediateData(off) => stack.set_base(Address::Fixed(off)),
                        _ => panic!("{:?}", src),
                    }
                } else if src == EffectiveAddr::AddrRegDirect(AddrReg::SP) {
                }
            }
            Ins::Move { size, src, dest } => {
                if let EffectiveAddr::AddrRegIndirectWPredec(AddrReg::SP) = dest {
                    stack.push(size, src);
                }
            }
            Ins::Jump(ea) => {
                if let EffectiveAddr::AbsLongData { addr, relocated } = ea {
                    if relocated {
                        println!();
                        dec.reset(addr as usize);
                    } else {
                        print!(" <---STOP--->");
                        break;
                    }
                } else {
                    print!(" <---STOP--->");
                    break;
                }
            }
            Ins::JumpToSubroutine(ea) => {
                if let EffectiveAddr::AbsLongData { addr, relocated } = ea {
                    if relocated {
                        let target = addr as usize;
                        info.sr_call = Some(target);
                        ti_map.entry(target).or_default().sr_start = true;
                    } else {
                        print!(" <---??r??--->");
                    }
                } else {
                    print!(" <---????--->");
                }
            }
            Ins::AddQuick { size: _, ea, data } => {
                if ea == EffectiveAddr::AddrRegDirect(AddrReg::SP) {
                    stack.discard(data as u32);
                }
            }
            Ins::Trap(trap) => match trap {
                1 => {
                    let arg = stack.word_at(0);
                    if let Word::Immediate(val) = arg {
                        match val {
                            9 => {
                                let buf = stack.long_at(2);
                                print!("               Cconws({}) <<GEMDOS>>", buf);
                            }
                            49 => {
                                let keepcnt = stack.long_at(2);
                                let retcode = stack.word_at(6);
                                print!(
                                    "               Ptermres({},{}) <<GEMDOS>>",
                                    keepcnt, retcode
                                );
                                stack.discard(8);
                                break;
                            }
                            74 => {
                                let _ = stack.word_at(2);
                                let block = stack.long_at(4);
                                let newsiz = stack.long_at(8);
                                print!("               Mshrink({},{}) <<GEMDOS>>", block, newsiz);
                            }
                            _ => {
                                println!("               GEMDOS #{}", val);
                                break;
                            }
                        }
                    }
                }
                14 => {
                    let val = stack.word_at(0);
                    match val {
                        Word::Immediate(38) => {
                            let func = stack.long_at(2);
                            print!("               Supexec({})", func);

                            if let Long::Immediate(off) = func {
                                let target = off as usize;
                                info.sr_call = Some(target);
                                ti_map.entry(target).or_default().sr_start = true;
                            } else {
                                print!(" <TODO>");
                            }
                        }
                        _ => {
                            print!("               XBIOS #{}", val);
                        }
                    }
                }
                _ => print!("               <<???>>"),
            },
            _ => {
                // TODO
            }
        }
        println!();
    }
    println!();

    println!();
    println!("Stack: {}", stack);
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    let opts: Options = Options::from_args();
    let buf = std::fs::read(&opts.file)?;

    let (_rest, prog): (&[u8], Program) =
        context("prog", p_prog)(&buf)
            .finish()
            .map_err(|t: ErrorTree<&[u8]>| {
                let t2: ErrorTree<ByteOffset> = t.extract_context(&buf[..]);
                let t3: ErrorTree<usize> = t2.map_locations(|o| o.0);
                t3
            })?;

    println!("{:#?}", &prog.header);

    if opts.text {
        let mut ti_map: BTreeMap<usize, TextInfo> = BTreeMap::new();

        let offset = opts.offset.unwrap_or(0);
        ti_map.entry(offset).or_default().sr_start = true;
        println!();
        println!("## Text:");
        println!();

        let mut cnt = 0;
        while let Some(offset) = ti_map
            .iter()
            .find(|(_, &ti)| ti.op.is_none() && ti.sr_start)
            .map(|(&at, _)| at)
        {
            println!("Next SR: {}", offset);
            disassemble(&prog, offset, &mut ti_map);

            cnt += 1;
            if let Some(max) = opts.max {
                if cnt >= max {
                    break;
                }
            }
        }

        println!("Branches:");
        for (&at, ti) in &ti_map {
            if let Some(target) = ti.sr_call {
                println!("- {} ==> {}", at, target);
            }
        }

        //print_buf(&prog.text);
    }

    if opts.data {
        println!();
        println!("## Misc");
        println!();
        print_buf(&prog.data);
    }

    println!();
    println!("## Misc");
    println!();

    println!("#relocations: {}", &prog.reloc.len());
    println!("remaining: {:?} bytes", _rest);

    Ok(())
}
