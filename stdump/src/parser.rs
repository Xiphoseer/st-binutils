use nom::{
    bytes::complete::take,
    error::context,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};

type WORD = u16;
type LONG = u32;

#[derive(Debug)]
pub struct PH {
    /// Branch to start of the program
    ///
    /// (must be 0x601a!)
    pub ph_branch: WORD,

    /// Length of the TEXT segment
    pub ph_tlen: LONG,
    /// Length of the DATA segment
    pub ph_dlen: LONG,
    /// Length of the BSS segment
    pub ph_blen: LONG,
    /// Length of the symbol table
    pub ph_slen: LONG,
    /// Reserved, should be 0
    /// Required by PureC
    pub ph_res1: LONG,
    /// Program flags
    pub ph_prgflags: LONG,
    /// 0 = Relocation info present
    pub ph_absflag: WORD,
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
pub struct Program<'a> {
    pub header: PH,
    pub text: &'a [u8],
    pub data: &'a [u8],
    pub symb: &'a [u8],
    pub reloc: Vec<u32>,
}

pub fn p_prog(input: &[u8]) -> IResult<&[u8], Program, ErrorTree<&[u8]>> {
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
