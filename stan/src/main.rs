//! An ST analyzer
#![warn(missing_docs)]

use std::{ffi::CStr, fmt, os::raw::c_char, path::PathBuf};

use libtos::aes::{I8Ptr, OBJECT, RSHDR, TEDINFO};
use nom::{IResult, Offset, bytes::complete::tag, combinator::map, error::ParseError, multi::count, number::complete::{be_i16, be_u16, be_u32}, sequence::{preceded, tuple}};
use nom_supreme::{
    error::ErrorTree,
    final_parser::{final_parser, RecreateContext},
};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Options {
    file: PathBuf,
}

/// Parse a RSC header
fn p_rshdr<'i, E>(input: &'i [u8]) -> IResult<&'i [u8], RSHDR, E>
where
    E: ParseError<&'i [u8]>,
{
    map(
        preceded(
            tag(&[0u8, 0u8]),
            tuple((
                be_u16, be_u16, be_u16, be_u16, be_u16, be_u16, be_u16, be_u16, be_u16, be_u16,
                be_u16, be_u16, be_u16, be_u16, be_u16, be_u16, be_u16,
            )),
        ),
        |(
            rsh_object,
            rsh_tedinfo,
            rsh_iconblk,
            rsh_bitblk,
            rsh_frstr,
            rsh_string,
            rsh_imdata,
            rsh_frimg,
            rsh_trindex,
            rsh_nobs,
            rsh_ntree,
            rsh_nted,
            rsh_nib,
            rsh_nbb,
            rsh_nstring,
            rsh_nimages,
            rsh_rssize,
        )| RSHDR {
            rsh_vrsn: 0,
            rsh_object,
            rsh_tedinfo,
            rsh_iconblk,
            rsh_bitblk,
            rsh_frstr,
            rsh_string,
            rsh_imdata,
            rsh_frimg,
            rsh_trindex,
            rsh_nobs,
            rsh_ntree,
            rsh_nted,
            rsh_nib,
            rsh_nbb,
            rsh_nstring,
            rsh_nimages,
            rsh_rssize,
        },
    )(input)
}

/// Parse a RSC header
fn p_object<'i, E>(input: &'i [u8]) -> IResult<&'i [u8], OBJECT, E>
where
    E: ParseError<&'i [u8]>,
{
    map(
        tuple((
            be_i16, be_i16, be_i16, be_u16, be_u16, be_u16, be_u32, be_i16, be_i16, be_i16, be_i16,
        )),
        |(
            ob_next,
            ob_head,
            ob_tail,
            ob_type,
            ob_flags,
            ob_state,
            ob_spec,
            ob_x,
            ob_y,
            ob_width,
            ob_height,
        )| OBJECT {
            ob_next,
            ob_head,
            ob_tail,
            ob_type,
            ob_flags,
            ob_state,
            ob_spec,
            ob_x,
            ob_y,
            ob_width,
            ob_height,
        },
    )(input)
}

/// Parse a TEDINFO structure
fn p_tedinfo<'i, E>(input: &'i [u8]) -> IResult<&'i [u8], TEDINFO<I8Ptr>, E>
where
    E: ParseError<&'i [u8]>,
{
    map(
        tuple((
            be_u32, be_u32, be_u32, be_i16, be_i16, be_i16, be_i16, be_i16, be_i16, be_i16, be_i16,
        )),
        |(
            te_ptext,
            te_ptmplt,
            te_pvalid,
            te_font,
            te_fontid,
            te_just,
            te_color,
            te_fontsize,
            te_thickness,
            te_txtlen,
            te_tmplen,
        )| TEDINFO {
            te_ptext,
            te_ptmplt,
            te_pvalid,
            te_font,
            te_fontid,
            te_just,
            te_color,
            te_fontsize,
            te_thickness,
            te_txtlen,
            te_tmplen,
        },
    )(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MyByteOffset(pub usize);

impl fmt::Display for MyByteOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<I: Offset> RecreateContext<I> for MyByteOffset {
    fn recreate_context(original_input: I, tail: I) -> Self {
        MyByteOffset(original_input.offset(&tail))
    }
}

fn p_res<'i>(input: &'i [u8]) -> Result<RSHDR, ErrorTree<MyByteOffset>> {
    final_parser(p_rshdr::<ErrorTree<&'i [u8]>>)(&input[..36])
}

fn p_objects<'i>(input: &'i [u8], cnt: usize) -> Result<Vec<OBJECT>, ErrorTree<MyByteOffset>> {
    final_parser(count(p_object::<ErrorTree<&'i [u8]>>, cnt))(&input[..(cnt * 24)])
}

fn p_tedinfos<'i>(input: &'i [u8], cnt: usize) -> Result<Vec<TEDINFO<I8Ptr>>, ErrorTree<MyByteOffset>> {
    final_parser(count(p_tedinfo::<ErrorTree<&'i [u8]>>, cnt))(&input[..(cnt * 28)])
}

fn lpctstr(input: &[u8], off: u32) -> &CStr {
    let ptr: *const c_char = unsafe { input.as_ptr().offset(off as isize) as *const c_char};
    unsafe { CStr::from_ptr(ptr) }
}

/// Main function
fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    let opts = Options::from_args();
    let data = std::fs::read(&opts.file)?;

    let rshdr: RSHDR = p_res(&data)?;
    println!("{:#?}", rshdr);
    let objects: Vec<OBJECT> = p_objects(
        &data[(rshdr.rsh_object as usize)..],
        rshdr.rsh_nobs as usize,
    )?;
    for o in objects {
        println!("{:?}", o);
    }
    let tedinfos: Vec<TEDINFO<I8Ptr>> = p_tedinfos(
        &data[(rshdr.rsh_tedinfo as usize)..],
        rshdr.rsh_nted as usize,
    )?;
    for te in tedinfos {
        println!("{:?}", te);
        println!("{:?}", lpctstr(&data, te.te_ptext).to_string_lossy());
        println!("{:?}", lpctstr(&data, te.te_ptmplt).to_string_lossy());
        println!("{:?}", lpctstr(&data, te.te_pvalid).to_string_lossy());
    }

    Ok(())
}
