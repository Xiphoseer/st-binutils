//! # Application Environment Services

/// Header of a RSC (Resource) file
/// 
/// Source: <https://freemint.github.io/tos.hyp/en/aes_structures.html#RSHDR>
#[repr(C)]
#[derive(Debug)]
pub struct RSHDR {
   /// Null
   pub rsh_vrsn: u16,
   /// Position of the object field
   pub rsh_object: u16,
   /// Position of the TEDINFO structs
   pub rsh_tedinfo: u16,
   /// Position of the ICONBLK structs
   pub rsh_iconblk: u16,
   /// Position of the BITBLK structs
   pub rsh_bitblk: u16,
   /// Position of the free strings
   pub rsh_frstr: u16,
   /// Unused
   pub rsh_string: u16,
   /// Position of image data
   pub rsh_imdata: u16,
   /// Position of the free images
   pub rsh_frimg: u16,
   /// Position of the object tree table
   pub rsh_trindex: u16,
   /// Total number of objects
   pub rsh_nobs: u16,
   /// Total number of trees
   pub rsh_ntree: u16,
   /// Total number of TEDINFO structs
   pub rsh_nted: u16,
   /// Total number of ICONBLK structs
   pub rsh_nib: u16,
   /// Total number of BITBLK structs
   pub rsh_nbb: u16,
   /// Total number of strings
   pub rsh_nstring: u16,
   /// Total number of images
   pub rsh_nimages: u16,
   /// Total bytes in resource
   pub rsh_rssize: u16,
}

/// The object structure
///
/// Source: <https://freemint.github.io/tos.hyp/en/aes_structures.html#OBJECT>
#[repr(C)]
#[derive(Debug)]
pub struct OBJECT {
   /// The next object
   pub ob_next: i16,
   /// First child
   pub ob_head: i16,
   /// Last child
   pub ob_tail: i16,
   /// Object type
   pub ob_type: u16,
   /// Manipulation flags
   pub ob_flags: u16,
   /// Object status
   pub ob_state: u16,
   /// More under object type
   pub ob_spec: u32,
   /// X-coordinate of the object
   pub ob_x: i16,
   /// Y-coordinate of the object
   pub ob_y: i16,
   /// Width of the object
   pub ob_width: i16,
   /// Height of the object
   pub ob_height: i16,
}

/// Pointer to a string
pub type I8Ptr = u32;

/// Text definition information
///
/// Source: <https://freemint.github.io/tos.hyp/en/aes_structures.html#TEDINFO>
#[repr(C)]
#[derive(Debug)]
pub struct TEDINFO<LPSTR> {
    /// Pointer to a string
    pub te_ptext: LPSTR,
    /// Pointer to the string template
    pub te_ptmplt: LPSTR,
    /// Pointer to the validation string
    pub te_pvalid: LPSTR,
    /// Font type
    pub te_font: i16,
    /// GDOS Font ID
    pub te_fontid: i16,
    /// Text alignment
    ///
    /// 0 = Ranged left  
    /// 1 = Ranged right  
    /// 2 = Centred
    pub te_just: i16,
    /// Colour
    pub te_color: i16,
    /// GDOS font size in points
    pub te_fontsize: i16,
    /// Border width
    pub te_thickness: i16,
    /// Maximum length of the text
    pub te_txtlen: i16,
    /// Length of the string template
    pub te_tmplen: i16,
}