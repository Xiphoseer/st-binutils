use std::path::PathBuf;

use nom::{
    bytes::complete::take,
    error::context,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    Finish, IResult,
};
use nom_supreme::{
    error::{BaseErrorKind, ErrorTree, Expectation},
    final_parser::{ByteOffset, ExtractContext},
};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Options {
    /// The PRG file to analyze
    file: PathBuf,

    /// Prints the content of the data section
    #[structopt(long, short)]
    data: bool,
}

type WORD = u16;
type LONG = u32;

#[derive(Debug)]
struct PH {
    /// Branch to start of the program
    ///
    /// (must be 0x601a!)
    ph_branch: WORD,

    /// Length of the TEXT segment
    ph_tlen: LONG,
    /// Length of the DATA segment
    ph_dlen: LONG,
    /// Length of the BSS segment
    ph_blen: LONG,
    /// Length of the symbol table
    ph_slen: LONG,
    /// Reserved, should be 0
    /// Required by PureC
    ph_res1: LONG,
    /// Program flags
    ph_prgflags: LONG,
    /// 0 = Relocation info present
    ph_absflag: WORD,
}

fn p_ph(input: &[u8]) -> IResult<&[u8], PH, ErrorTree<&[u8]>> {
    let (rest, (ph_branch, ph_tlen, ph_dlen, ph_blen, ph_slen, ph_res1, ph_prgflags, ph_absflag)) =
        tuple((
            be_u16, be_u32, be_u32, be_u32, be_u32, be_u32, be_u32, be_u16,
        ))(input)?;
    Ok((
        rest,
        PH {
            ph_branch,
            ph_tlen,
            ph_dlen,
            ph_blen,
            ph_slen,
            ph_res1,
            ph_prgflags,
            ph_absflag,
        },
    ))
}

#[derive(Debug)]
struct Program<'a> {
    header: PH,
    text: &'a [u8],
    data: &'a [u8],
    symb: &'a [u8],
    reloc: Vec<u32>,
}

fn p_prog(input: &[u8]) -> IResult<&[u8], Program, ErrorTree<&[u8]>> {
    let (input, header) = context("header", p_ph)(input)?;
    let (input, (text, data, symb, reloc)) = tuple((
        context("text", take(header.ph_tlen)),
        context("data", take(header.ph_dlen)),
        context("symb", take(header.ph_slen)),
        context("reloc", p_reloc),
    ))(input)?;
    Ok((
        input,
        Program {
            header,
            text,
            data,
            symb,
            reloc,
        },
    ))
}

fn p_reloc(input: &[u8]) -> IResult<&[u8], Vec<u32>, ErrorTree<&[u8]>> {
    let (mut input, mut offset) = be_u32(input)?;
    let mut table = Vec::with_capacity(input.len());
    table.push(offset);
    while let Some((&first, rest)) = input.split_first() {
        match first {
            0 => return Ok((rest, table)),
            1 => offset += 254, // or 254?
            _ => {
                offset += u32::from(first);
                table.push(offset);
            }
        }
        input = rest;
    }
    Err(nom::Err::Error(ErrorTree::Base {
        location: input,
        kind: BaseErrorKind::Expected(Expectation::Eof),
    }))
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

    if opts.data {
        println!("DATA:");
        print_buf(&prog.data);
    }

    println!("#relocations: {}", &prog.reloc.len());
    println!("remaining: {:?} bytes", _rest);

    Ok(())
}
