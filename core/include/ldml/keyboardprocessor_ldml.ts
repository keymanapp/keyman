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
// To update keyboardprocessor_ldml.h, and commit the result.
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
  'finl' |
  'key2' |
  'layr' |
  'list' |
  'loca' |
  'meta' |
  'name' |
  'ordr' |
  'strs' |
  'tran' |
  'vkey';


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
   * The techpreview CLDR version
   */
  readonly cldr_version_techpreview = 'techpreview';
  /**
   * The latest CLDR version
   */
  readonly cldr_version_latest = this.cldr_version_techpreview;
  /**
   * import base
   */
  readonly cldr_import_base = 'cldr';
  /**
   * implied keys file
   */
  readonly cldr_implied_keys_import = `${this.cldr_version_techpreview}/keys-Latn-implied.xml`;
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
   readonly length_disp_item = 8;

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
   * bitwise or value for unicode_set in elem[elemstr][element].flags.
   * If bit is 1, then 'element' is a UnicodeSet string.
   * If bit is 0, then 'element' is a UTF-32LE codepoint
   *
   * `unicode_set = flags & elem_flags_unicode_set`
   */
  readonly elem_flags_unicode_set = 0x00000001;

  /**
   * bitwise or value for tertiary_base in elem[elemstr][element].flags.
   * If bit is 1, then tertiary_base is true.
   * If bit is 0, then tertiary_base is false.
   *
   * Used only for `ordr`-type element strings.
   *
   * `tertiary_base = flags & elem_flags_tertiary_base`
   */
  readonly elem_flags_tertiary_base = 0x00000002;

  /**
   * bitwise or value for tertiary_base in elem[elemstr][element].flags.
   * If bit is 1, then prebase is true.
   * If bit is 0, then prebase is false.
   *
   * Used only for `ordr`-type element strings.
   *
   * `prebase = flags & elem_flags_prebase`
   */
  readonly elem_flags_prebase = 0x00000004;

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
    * keys section is now key2.jmap
      ------------------------------------------------------------------ */

  /**
   * Constant for no modifiers
   */
  readonly keys_mod_none = 0;
  /**
   * bitmask for Left Alt modifier key
   */
  readonly keys_mod_altL = 1 << 2;
  /**
   * bitmask for Right Alt (AltGr) modifier key
   */
  readonly keys_mod_altR = 1 << 3;
  /**
   * bitmask for either Alt (Windows) or Option (Apple) modifier keys
   */
  readonly keys_mod_alt  = this.keys_mod_altL | this.keys_mod_altR;
  /**
   * bitmask for Caps modifier key
   */
  readonly keys_mod_caps = 1 << 8;
  /**
   * bitmask for Left control modifier key
   */
  readonly keys_mod_ctrlL = 1 << 0;
  /**
   * bitmask for Right control modifier key
   */
  readonly keys_mod_ctrlR = 1 << 1;
  /**
   * bitmask for either Control modifier key
   */
  readonly keys_mod_ctrl = this.keys_mod_ctrlL | this.keys_mod_ctrlR;
  /**
   * bitmask for either shift.
   */
  readonly keys_mod_shift = 1 << 4;

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
    ]
  );

  /**
   * a mask combining all valid modifier bits
   */
  readonly keys_mod_all: number = Array.from(this.keys_mod_map.values()).reduce((p, v) => (p | v), this.keys_mod_none);

  /* ------------------------------------------------------------------
   * key2 section
    ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'key2' section not including variable parts
   */
  readonly length_key2 = 24;
  /**
   *  Length of each item in the 'key2' keys sub-table
   */
  readonly length_key2_key = 36;
  /**
   *  Length of each item in the 'key2' flick lists sub-table
   */
  readonly length_key2_flick_list = 12;
  /**
   *  Length of each item in the 'key2' flick elements sub-table
   */
  readonly length_key2_flick_element = 12;
  /**
   * Length of each item in the 'key2.kmap' key map subtable
   */
  readonly length_key2_kmap = 12;

  /**
   * 0 if to is a char, 1 if it is a string
   */
  readonly key2_key_flags_extend      = 0x00000001;

  /**
   * 1 if the key is a gap
   */
  readonly key2_key_flags_gap         = 0x00000002;

  /**
   * 1 if the key is transform=no
   */
  readonly key2_key_flags_notransform = 0x00000004;

  /**
   * 0 if to is a char, 1 if it is a string
   */
  readonly key2_flick_flags_extend      = 0x00000001;

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
  readonly layr_list_hardware_touch = 0;
  /**
   * for the 'hardware' field indicating an abnt2 layout
   */
  readonly layr_list_hardware_abnt2 = 1;
  /**
   * for the 'hardware' field indicating an iso layout
   */
  readonly layr_list_hardware_iso = 2;
  /**
   * for the 'hardware' field indicating a jis layout
   */
  readonly layr_list_hardware_jis = 3;
  /**
   * for the 'hardware' field indicating a us layout
   */
  readonly layr_list_hardware_us = 4;
  /**
   * Convenience map of layr_list_hardware field values
   */
  readonly layr_list_hardware_map: Map<String, number> = new Map(
    [
      ["touch", this.layr_list_hardware_touch],
      ["abnt2", this.layr_list_hardware_abnt2],
      ["iso", this.layr_list_hardware_iso],
      ["jis", this.layr_list_hardware_jis],
      ["us", this.layr_list_hardware_us],
    ]
  );
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
   * bitwise or value for fallback=omit in meta.settings
   */
  readonly meta_settings_fallback_omit = 1;
  /**
   * bitwise or value for transformFailure=omit in meta.settings
   */
  readonly meta_settings_transformFailure_omit = 2;
  /**
   * bitwise or value for transformPartial=hide in meta.settings
   */
  readonly meta_settings_transformPartial_hide = 4;

  /* ------------------------------------------------------------------
    * name section
      ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'name' section not including variable parts
   */
  readonly length_name = 12;
  /**
   *  Length of each item in the 'name' section variable part
   */
  readonly length_name_item = 4;

  /* ------------------------------------------------------------------
    * ordr section
      ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'ordr' section, not including entries
   */
  readonly length_ordr = 12;
  /**
   *  Length of each item in the 'ordr' section variable part
   */
  readonly length_ordr_item = 8;

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
  readonly length_tran = 12;
  /**
   *  Length of each item in the 'tran' section variable part
   */
  readonly length_tran_item = 16;
  /**
   * bitwise or value for error="fail" in transform
   */
  readonly tran_flags_error = 0x0001;

  /* ------------------------------------------------------------------
    * vkey section
      ------------------------------------------------------------------ */

  /**
   * Minimum length of the 'vkey' section not including variable parts
   */
  readonly length_vkey = 12;
  /**
   *  Length of each item in the 'vkey' section variable part
   */
  readonly length_vkey_item = 8;

  /**
   * All section IDs.
   */
  readonly section: SectionMap = {
  // keep this sorted
      bksp: 'bksp',
      disp: 'disp',
      elem: 'elem',
      finl: 'finl',
      key2: 'key2',
      layr: 'layr',
      list: 'list',
      loca: 'loca',
      meta: 'meta',
      name: 'name',
      ordr: 'ordr',
      sect: 'sect',
      strs: 'strs',
      tran: 'tran',
      vkey: 'vkey',
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
    let chars : string[] = [];
    for (let i = 3; i>=0; i--) {
      chars.push(String.fromCharCode(hex & 0xFF));
      hex >>= 8;
    }
    return chars.join('');
  }
};

export const constants = new Constants();

// }
