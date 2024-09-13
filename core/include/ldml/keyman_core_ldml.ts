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
   * The latest CLDR version
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
  readonly length_header = 8;

  /* ------------------------------------------------------------------
    * sect section
      ------------------------------------------------------------------ */

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
      ------------------------------------------------------------------ */

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
      ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'disp' section, not including entries
   */
   readonly length_disp = 16;
   /**
    *  Length of each entry in the 'disp' variable part
    */
   readonly length_disp_item = 12;

  /* ------------------------------------------------------------------
    * elem section
      ------------------------------------------------------------------ */

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
     ------------------------------------------------------------------ */

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
      ------------------------------------------------------------------ */

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
    ------------------------------------------------------------------ */

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
     ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'layr' section not including variable parts
   */
  readonly length_layr = 24;
  /**
   *  Length of each layer list in the 'layr' section variable part
   */
  readonly length_layr_list = 16;
  /**
   * for the 'hardware' field indicating a touch keyboard, non-hardware
   */
  readonly layr_list_hardware_touch = 'touch';
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

  /* ------------------------------------------------------------------
   * list section
      ------------------------------------------------------------------ */

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
   ------------------------------------------------------------------ */

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
      ------------------------------------------------------------------ */

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

};

export const constants = new Constants();

// }
