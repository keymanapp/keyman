/*
  Copyright:        Copyright (C) 2022 SIL International.
  Authors:          srl295, mcdurdin
  This file provides constants for the KMX Plus (LDML support) binary format,
  to be shared between TypeScript and C++ via the generator (below)
*/

// NOTICE!
//
// If you update this file, you *must* be sure to re-run
//
//  core/tools/ldml-const-builder/build.sh clean build run
//
// To update keyman_core_ldml.h, and commit the result.
//
// It is not updated automatically.


/**
 * Defines the section identifiers and ensures that we include each and every
 * one of them in the `sections` block and gives us a type which we can iterate
 * through.
 */
export type SectionIdent =
// Keep this sorted, but with `sect` as the first entry.
  'sect' |
  'bksp' |
  'disp' |
  'elem' |
  'keys' |
  'layr' |
  'list' |
  'loca' |
  'meta' |
  'strs' |
  'tran' |
  'uset' |
  'vars';


type SectionMap = {
  [id in SectionIdent]: SectionIdent;
}

export enum KMXPlusVersion {
  Version17 = 0x1100, // == KMXFile.VERSION_170,
  Version19 = 0x1300, // == KMXFile.VERSION_190,
};

// TODO-LDML: namespace com.keyman.core.ldml {
/**
 * Constants for the KMXPlus data format
 * These are shared between the data access layer and the compiler.
 * Note that the section IDs (section_keys etc.) are 32 bit hex
 * values that are designed to appear as text when written in little endian
 * format, so 0x7379656b = 'keys'
 */
class Constants {
  /**
   * The version of the LDML processor
   */
  readonly version = '1.0';
  /**
   * The current CLDR version
   */
  readonly cldr_version_latest = '46';
  /**
   * The version for testdata files
   */
  readonly cldr_test_version_latest = 'techpreview';
  /**
   * import base
   */
  readonly cldr_import_base = 'cldr';
  /**
   * implied keys file
   */
  readonly cldr_implied_keys_import = `${this.cldr_version_latest}/keys-Latn-implied.xml`;
  /**
   * implied scancodes file
   */
  readonly cldr_implied_forms_import = `${this.cldr_version_latest}/scanCodes-implied.xml`;
  /**
   * Length of a raw section header, in bytes
   */
  readonly length_header_17 = 8;
  /**
   * Length of a raw section header, in bytes
   */
  readonly length_header_19 = 12;

  /**
   * Version number 17 for KMX+ file format, initial release version,
   * corresponds to Keyman 17.0
   */
  readonly kmxplus_version_17: KMXPlusVersion = KMXPlusVersion.Version17;

  /**
   * Version number 19 for KMX+ file format, new SEC2 section and version
   * header, corresponds to Keyman 19.0
   */
  readonly kmxplus_version_19: KMXPlusVersion = KMXPlusVersion.Version19;

  /* ------------------------------------------------------------------
   * sect section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'sect' section, not including entries
   */
  readonly length_sect = 16;
  /**
   *  Length of each item in the 'sect' section variable part
   */
  readonly length_sect_item = 8;

  /* ------------------------------------------------------------------
   * bksp section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'bksp' section, not including entries
   */
  readonly length_bksp = 12;
  /**
   *  Length of each item in the 'bksp' section variable part
   */
  readonly length_bksp_item = 16;
  /**
   * bitwise or value for error="fail" in transform
   */
  readonly bksp_flags_error = 0x0001;

  /* ------------------------------------------------------------------
   * disp section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'disp' section, not including entries
   */
  readonly length_disp = 16;
  /**
   *  Length of each entry in the 'disp' variable part; note size is same in v17 and v19
   */
  readonly length_disp_item = 12;


  /**
   * in disp.item.flags, mask for flag bits (v19+)
   */
  readonly disp_item_flags_mask_flags         = 0x00000FFF;

  /**
   * in disp.item.flags, if set, then item toId value is an id. (v19+)
   */
  readonly disp_item_flags_is_id              = 0x00000001;
  /**
   * in disp.item.flags, if set, then item display string is an SVG file. (v19+)
   */
  readonly disp_item_flags_is_svg             = 0x00000002;
  /**
   * in disp.item.flags, if set, then draw the key as a frame key (v19+)
   */
  readonly disp_item_flags_is_frame_key       = 0x00000004;
  /**
   * in disp.item.flags, if set, then draw the key as highlighted (v19+)
   */
  readonly disp_item_flags_is_highlighted     = 0x00000008;
  /**
   * in disp.item.flags, if set, then draw the key as a deadkey (v19+)
   */
  readonly disp_item_flags_is_deadkey         = 0x00000010;
  /**
   * in disp.item.flags, if set, then the key is blank and non-interactive (v19+)
   */
  readonly disp_item_flags_is_blank_key       = 0x00000020;

  // reserved bits 6-11 for future flags

  /**
   * in disp.item.flags, mask for hint position (v19+)
   */
  readonly disp_item_flags_mask_hint          = 0x0000F000;

  /**
   * in disp.item.flags, right shift value for masked hint position (v19+)
   */
  readonly disp_item_flags_shift_hint         = 12;

  /** hint position for primary key cap (v19+) */
  readonly disp_item_hint_primary  = 0;
  /** hint position for north-west (top left) hint (v19+) */
  readonly disp_item_hint_nw       = 1;
  /** hint position for north (top) hint (v19+) */
  readonly disp_item_hint_n        = 2;
  /** hint position for north-east (top right) hint (v19+) */
  readonly disp_item_hint_ne       = 3;
  /** hint position for west (left) hint (v19+) */
  readonly disp_item_hint_w        = 4;
  /** hint position for east (right) hint (v19+) */
  readonly disp_item_hint_e        = 5;
  /** hint position for south west (bottom left) hint (v19+) */
  readonly disp_item_hint_sw       = 6;
  /** hint position for south (bottom) hint (v19+) */
  readonly disp_item_hint_s        = 7;
  /** hint position for south east (bottom right) hint (v19+) */
  readonly disp_item_hint_se       = 8;

  /**
   * in disp.item.flags, mask for key cap type. (v19+)
   */
  readonly disp_item_flags_mask_key_cap_type  = 0xFFFF0000;

  /**
   * in disp.item.flags, right shift value for key cap type.
   */
  readonly disp_item_flags_shift_key_cap_type = 16;

  // The following values match web/.../specialCharacters.ts; see
  // developer/src/kmc-kmn/test/kmw/constants.tests.ts for more information.

  /**
   * dis2.item.flags key cap type, sync with specialCharacters.ts (v19+)
   */
  readonly disp_key_cap_shift =    8;
  readonly disp_key_cap_enter =    5;
  readonly disp_key_cap_tab =      6;
  readonly disp_key_cap_bksp =     4;
  readonly disp_key_cap_menu =     11;
  readonly disp_key_cap_hide =     10;
  readonly disp_key_cap_alt =      25;
  readonly disp_key_cap_ctrl =     1;
  readonly disp_key_cap_caps =     3;
  readonly disp_key_cap_abc_upper = 16; // differentiate '*ABC*' and '*abc*'
  readonly disp_key_cap_abc_lower = 17; // differentiate '*ABC*' and '*abc*'
  readonly disp_key_cap_123 =      19;
  readonly disp_key_cap_symbol =   21;
  readonly disp_key_cap_currency = 20;
  readonly disp_key_cap_shifted =  9;
  readonly disp_key_cap_altgr =    2;
  readonly disp_key_cap_tableft =  7;
  readonly disp_key_cap_lalt =     0x56;
  readonly disp_key_cap_ralt =     0x57;
  readonly disp_key_cap_lctrl =    0x58;
  readonly disp_key_cap_rctrl =    0x59;
  readonly disp_key_cap_laltctrl =       0x60;
  readonly disp_key_cap_raltctrl =       0x61;
  readonly disp_key_cap_laltctrlshift =  0x62;
  readonly disp_key_cap_raltctrlshift =  0x63;
  readonly disp_key_cap_altshift =       0x64;
  readonly disp_key_cap_ctrlshift =      0x65;
  readonly disp_key_cap_altctrlshift =   0x66;
  readonly disp_key_cap_laltshift =      0x67;
  readonly disp_key_cap_raltshift =      0x68;
  readonly disp_key_cap_lctrlshift =     0x69;
  readonly disp_key_cap_rctrlshift =     0x70;
  // Added in Keyman 14.0.
  readonly disp_key_cap_ltrenter =       0x05; // Default alias of '*Enter*'.
  readonly disp_key_cap_ltrbksp =        0x04; // Default alias of '*BkSp*'.
  readonly disp_key_cap_rtlenter =       0x71;
  readonly disp_key_cap_rtlbksp =        0x72;
  readonly disp_key_cap_shiftlock =      0x73;
  readonly disp_key_cap_shiftedlock =    0x74;
  readonly disp_key_cap_zwnj =           0x75; // If this one is specified, auto-detection will kick in.
  readonly disp_key_cap_zwnjios =        0x75; // The iOS version will be used by default, but the
  readonly disp_key_cap_zwnjandroid =    0x76; // Android platform has its own default glyph.
  // Added in Keyman 17.0.
  // Reference: https://github.com/silnrsi/font-symchar/blob/v4.000/documentation/encoding.md
  readonly disp_key_cap_zwnjgeneric =    0x79; // Generic version of ZWNJ (no override)
  readonly disp_key_cap_sp =             0x80; // Space
  readonly disp_key_cap_nbsp =           0x82; // No-break Space
  readonly disp_key_cap_narnbsp =        0x83; // Narrow No-break Space
  readonly disp_key_cap_enq =            0x84; // En Quad
  readonly disp_key_cap_emq =            0x85; // Em Quad
  readonly disp_key_cap_ensp =           0x86; // En Space
  readonly disp_key_cap_emsp =           0x87; // Em Space
  // TODO: Skipping #-per-em-space
  readonly disp_key_cap_punctsp =        0x8c; // Punctuation Space
  readonly disp_key_cap_thsp =           0x8d; // Thin Space
  readonly disp_key_cap_hsp =            0x8e; // Hair Space
  readonly disp_key_cap_zwsp =           0x81; // Zero Width Space
  readonly disp_key_cap_zwj =            0x77; // Zero Width Joiner
  readonly disp_key_cap_wj =             0x78; // Word Joiner
  readonly disp_key_cap_cgj =            0x7a; // Combining Grapheme Joiner
  readonly disp_key_cap_ltrm =           0x90; // Left-to-right Mark
  readonly disp_key_cap_rtlm =           0x91; // Right-to-left Mark
  readonly disp_key_cap_sh =             0xa1; // Soft Hyphen
  readonly disp_key_cap_htab =           0xa2; // Horizontal Tabulation


  /* ------------------------------------------------------------------
   * elem section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'elem' section, not including entries
   */
  readonly length_elem = 12;
  /**
   *  Length of each elem string in the 'elem' section variable part
   */
  readonly length_elem_item = 8;
  /**
   * Length of each element in an elem string
   */
  readonly length_elem_item_element = 8;

  /**
   * bitwise or value for type in elem[elemstr][element].flags.
   * If bits are 00b, then 'element' is a UTF-32LE codepoint.
   * If bits are 01b, then 'element' is a string index.
   * If bits are 10b (2), then 'element' is a uset index.
   *
   * `type = flags & elem_flags_type`
   */
  readonly elem_flags_type      = 0x00000003;
  readonly elem_flags_type_char = 0x00000000;
  readonly elem_flags_type_str  = 0x00000001;
  readonly elem_flags_type_uset = 0x00000002;

  /**
   * bitwise or value for tertiary_base in elem[elemstr][element].flags.
   * If bit is 1, then tertiary_base is true.
   * If bit is 0, then tertiary_base is false.
   *
   * Used only for `ordr`-type element strings.
   *
   * `tertiary_base = flags & elem_flags_tertiary_base`
   */
  readonly elem_flags_tertiary_base = 0x00000004;

  /**
   * bitwise or value for tertiary_base in elem[elemstr][element].flags.
   * If bit is 1, then prebase is true.
   * If bit is 0, then prebase is false.
   *
   * Used only for `ordr`-type element strings.
   *
   * `prebase = flags & elem_flags_prebase`
   */
  readonly elem_flags_prebase = 0x00000008;

  /**
   * bitwise mask for order in elem[elemstr][element].flags.
   *
   * Used only for `ordr`-type element strings. 1 byte signed integer.
   *
   * `order = (flags & elem_flags_order_mask) >> elem_flags_order_bitshift`
   */
  readonly elem_flags_order_mask = 0x00FF0000;

  /**
   * bit shift for order in elem[elemstr][element].flags.
   *
   * Used only for `ordr`-type element strings.
   *
   * `order = (flags & elem_flags_order_mask) >> elem_flags_order_bitshift`
   */
  readonly elem_flags_order_bitshift = 16;

  /**
   * bitwise mask for tertiary sort in elem[elemstr][element].flags.
   *
   * Used only for `ordr`-type element strings. 1 byte signed integer.
   *
   * `tertiary = (flags & elem_flags_tertiary_mask) >> elem_flags_tertiary_bitshift`
   */
  readonly elem_flags_tertiary_mask = 0xFF000000;

  /**
   * bit shift for tertiary sort in elem[elemstr][element].flags.
   *
   * Used only for `ordr`-type element strings. 1 byte signed integer.
   *
   * `order = (flags & elem_flags_tertiary_mask) >> elem_flags_tertiary_bitshift`
   */
  readonly elem_flags_tertiary_bitshift = 24;

  /* ------------------------------------------------------------------
   * finl section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'finl' section, not including entries
   */
  readonly length_finl = 8;
  /**
   *  Length of each item in the 'finl' section variable part
   */
  readonly length_finl_item = 16;
  /**
   * bitwise or value for error="fail" in transform
   */
  readonly finl_flags_error = 0x0001;

  /* ------------------------------------------------------------------
   * keys section is now keys.kmap
   * ------------------------------------------------------------------ */

  /**
   * Constant for no modifiers
   */
  readonly keys_mod_none = 0x0000;
  /**
   * bitmask for Left Alt modifier key
   */
  readonly keys_mod_altL = 0x0004;
  /**
   * bitmask for Right Alt (AltGr) modifier key
   */
  readonly keys_mod_altR = 0x0008;
  /**
   * bitmask for either Alt (Windows) or Option (Apple) modifier keys
   */
  readonly keys_mod_alt  = 0x0040;
  /**
   * bitmask for Caps modifier key
   */
  readonly keys_mod_caps = 0x0100;
  /**
   * bitmask for Left control modifier key
   */
  readonly keys_mod_ctrlL = 0x0001;
  /**
   * bitmask for Right control modifier key
   */
  readonly keys_mod_ctrlR = 0x0002;
  /**
   * bitmask for either Control modifier key
   */
  readonly keys_mod_ctrl = 0x0020;
  /**
   * bitmask for either shift.
   */
  readonly keys_mod_shift = 0x0010;

  /**
   * bitmask for 'other'.
   */
  readonly keys_mod_other = 0x10000;

  /**
   * Convenience map for modifiers
   */
  readonly keys_mod_map: Map<string, number> = new Map(
    [
      ["none", this.keys_mod_none],
      ["alt", this.keys_mod_alt],
      ["altL", this.keys_mod_altL],
      ["altR", this.keys_mod_altR],
      ["caps", this.keys_mod_caps],
      ["ctrl", this.keys_mod_ctrl],
      ["ctrlL", this.keys_mod_ctrlL],
      ["ctrlR", this.keys_mod_ctrlR],
      ["shift", this.keys_mod_shift],
      ["other", this.keys_mod_other],
    ]
  );

  /**
   * a mask combining all valid modifier bits
   */
  readonly keys_mod_all: number = Array.from(this.keys_mod_map.values()).reduce((p, v) => (p | v), this.keys_mod_none);

  /* ------------------------------------------------------------------
   * keys section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'keys' section not including variable parts
   */
  readonly length_keys = 24;
  /**
   *  Length of each item in the 'keys' keys sub-table
   */
  readonly length_keys_key = 36;
  /**
   *  Length of each item in the 'keys' flick lists sub-table
   */
  readonly length_keys_flick_list = 12;
  /**
   *  Length of each item in the 'keys' flick elements sub-table
   */
  readonly length_keys_flick_element = 8;
  /**
   * Length of each item in the 'keys.kmap' key map subtable
   */
  readonly length_keys_kmap = 12;

  /**
   * 0 if to is a char, 1 if it is a string
   */
  readonly keys_key_flags_extend      = 0x00000001;

  /**
   * 1 if the key is a gap
   */
  readonly keys_key_flags_gap         = 0x00000002;

  /* ------------------------------------------------------------------
   * layr section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'layr' section not including variable parts
   */
  readonly length_layr = 24;
  /**
   *  Length of each layer form (renamed from 'list' in v19) in the 'layr' section variable part (v17)
   */
  readonly length_layr_form_v17 = 16;
  /**
   *  Length of each layer form in the 'layr' section variable part (v19+)
   */
  readonly length_layr_form_v19 = 32;

  /**
   * for the 'hardware' field indicating a touch keyboard, non-hardware
   */
  readonly layr_form_hardware_touch = 'touch';
  /**
   * for the 'hardware' field indicating a Brazilian 103 key ABNT2 layout (iso + extra key near right shift)
   */
  readonly layr_form_hardware_abnt2 = 'abnt2';
  /**
   * for the 'hardware' field indicating a European 102 key layout (extra key near left shift)
   */
  readonly layr_form_hardware_iso = 'iso';
  /**
   * for the 'hardware' field indicating a Japanese 109 key layout
   */
  readonly layr_form_hardware_jis = 'jis';
  /**
   * for the 'hardware' field indicating a Korean KS layout
   */
  readonly layr_form_hardware_ks = 'ks';
  /**
   * for the 'hardware' field indicating a US ANSI 101 key keyboard
   */
  readonly layr_form_hardware_us = 'us';

  /**
   * Length of each layer entry in the 'layr' section variable part
   */
  readonly length_layr_entry = 16;
  /**
   * Length of each row entry in the 'layr' section variable part
   */
  readonly length_layr_row = 8;
  /**
   * Length of each key entry in the 'layr' section variable part
   */
  readonly length_layr_key = 4;
  /**
   * in layr.form.flags, if set, then base layout key caps hints should be shown
   */
  readonly layr_form_flags_show_base_layout              = 0x00000001;
  /**
   * in layr.form.flags, if set, then left/right Ctrl and Alt keys function independently
   */
  readonly layr_form_flags_chiral_separate               = 0x00000002;

  /**
   * Minimum allowed minDeviceWidth for a layer form
   */
  readonly layr_min_minDeviceWidth = 1;

  /**
   * Maximum allowed minDeviceWidth for a layer form
   */
  readonly layr_max_minDeviceWidth = 999;

  /* ------------------------------------------------------------------
   * list section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'list' section not including variable parts
   */
  readonly length_list = 16;
  /**
   *  Length of each list item in the 'list' list section variable part
   */
  readonly length_list_item = 8;
  /**
   *  Length of each list item in the 'list' indices section variable part
   */
   readonly length_list_index = 4;

  /* ------------------------------------------------------------------
   * loca section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'loca' section not including variable parts
   */
  readonly length_loca = 12;
  /**
   *  Length of each item in the 'loca' section variable part
   */
  readonly length_loca_item = 4;

  /* ------------------------------------------------------------------
   * meta section
   ------------------------------------------------------------------ */

  /**
   * length of the 'meta' section
   */
  readonly length_meta = 36;
  /**
   * bitwise or value for normalization=disabled in meta.settings
   */
  readonly meta_settings_normalization_disabled = 1;

  /* ------------------------------------------------------------------
   * strs section
   ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'strs' section not including variable parts
   */
  readonly length_strs = 12;
  /**
   * Length of each item in the 'strs' section variable part
   */
  readonly length_strs_item = 8;

  /* ------------------------------------------------------------------
   * tran section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'tran' section, not including entries
   */
  readonly length_tran = 20;
  /**
   *  Length of each transform group item
   */
  readonly length_tran_group = 12;
  /**
   *  Length of each transform item
   */
  readonly length_tran_transform = 16;
  /**
   *  Length of each reorder subtable item
   */
  readonly length_tran_reorder = 8;

  /**
   * bitwise or value for error="fail" in transform
   */
  readonly tran_flags_error = 0x0001;

  /**
   * this group is full of transform items
   */
  readonly tran_group_type_transform = 0;
  /**
   * this group is full of reorder items
   */
  readonly tran_group_type_reorder = 1;

  /* ------------------------------------------------------------------
   * vars section
   * ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'vars' section not including variable parts
   */
  readonly length_vars = 16;
  /**
   *  Length of each item in the 'vars' section variable part
   */
  readonly length_vars_item = 16;

  /**
   * String variable
   */
  readonly vars_entry_type_string = 0;
  /**
   * Set variable
   */
  readonly vars_entry_type_set = 1;
  /**
   * unicodeSet variable
   */
  readonly vars_entry_type_unicodeSet = 2;

  /* ------------------------------------------------------------------
   * uset section
   * ------------------------------------------------------------------ */

  /*
   * Minimum length of the 'uset' section not including variable parts
   */
  readonly length_uset = 16;

  /**
   * Length of each entry in the uset.usets subtable
   */
  readonly length_uset_uset = 12;

  /**
   * Length of each entry in the uset.ranges subtable
   */
  readonly length_uset_range = 8;

  /**
   * All section IDs.
   */
  readonly section: SectionMap = {
  // keep this sorted
      bksp: 'bksp',
      disp: 'disp',
      elem: 'elem',
      keys: 'keys',
      layr: 'layr',
      list: 'list',
      loca: 'loca',
      meta: 'meta',
      sect: 'sect',
      strs: 'strs',
      tran: 'tran',
      uset: 'uset',
      vars: 'vars',
  };

  // v19+: special case for 'sect' override with 'sec2'
  readonly sectionname_sec2 = 'sec2';
  readonly sectionid_sec2 = 0x32636573;

  /**
   * Use to convert 4-char string into hex
   * @param id section id such as 'sect'
   * @returns hex ID such as 0x74636573
   */
  hex_section_id(id:string) {
      if(!id || typeof id !== 'string' || !id.match(/^[a-z0-9]{4}$/)) {
          throw Error(`hex_section_id(${id}) - need a 4-character alphanumeric lower-case string`);
      }
      let r = 0;
      for (let i = 3; i>=0; i--) {
          r = (r << 8 | id.charCodeAt(i));
      }
      return r;
  };

  /**
   * Use to convert hex into 4-char string
   * @param hex section ID such as 0x74636573
   * @returns string such as 'sect'
   */
  str_section_id(hex:number) : string {
    const chars : string[] = [];
    for (let i = 3; i>=0; i--) {
      chars.push(String.fromCharCode(hex & 0xFF));
      hex >>= 8;
    }
    return chars.join('');
  }

  // ---- marker stuff ----
  /** == kmx_file.UC_SENTINEL, always followed by `marker_code`, marker index 0x0001-0xfffe */
  readonly uc_sentinel         = 0xFFFF;
  /** == kmx_file.CODE_DEADKEY */
  readonly marker_code         = 0x0008;
  /** used to refer to no index */
  readonly marker_no_index     = 0x0000;
  /** minimum usable marker index */
  readonly marker_min_index    = 0x0001;
  /** index value referring to the 'any' marker match */
  readonly marker_any_index    = 0xD7FF;
  /** maximum marker index prior to the 'any' value */
  readonly marker_max_index    = this.marker_any_index - 1;
  /** maximum count of markers (not including 'any') */
  readonly marker_max_count    = this.marker_max_index - this.marker_min_index + 1;

  /**
   * List of versions to treat as if they were the 'latest' version.
   */
  treatAsLatest(version: string): boolean {
    return cldrTreatAsLatest.has(version);
  }

  /**
   * Difference in section header size from default v17 size
   */
  headerSizeDelta(version: KMXPlusVersion): number {
    if(version == KMXPlusVersion.Version17) {
      return 0;
    }

    return 4; /* KMXPlusVersion.Version19, additional version uint32le field */
  }
};

/** There's no data or DTD change in 45, 46, 46.1, 47 so map them all to 46 at present. */
const cldrTreatAsLatest: Set<string> = new Set(['45', '46.1', '47']);

export const constants = new Constants();

// }
