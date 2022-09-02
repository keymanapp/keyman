/*
  Copyright:        Copyright (C) 2022 SIL International.
  Authors:          srl295
  This file provides constants for the KMX Plus (LDML support) binary format,
  to be shared between TypeScript and C++ via the generator (below)
*/

// This defines the section identifiers and ensures that we include each and
// every one of them in the `sections` block and gives us a type which we can
// iterate through

export type SectionIdent =
  'keys' |
  'loca' |
  'meta' |
  'sect' |
  'strs' |
  'vkey';

type SectionMap = {
  [id in SectionIdent]: SectionIdent;
}

interface Constants_Section {
  // For now, only defining one property as other types
  // can be inferred
  section: SectionMap;
}

type Constants = Constants_Section & {[id:string]:any};

// Notice!
//
// If you update this file, you *must* be sure to re-run
//
//  core/tools/ldml-const-builder/build.sh clean build run
//
// To update keyboardprocessor_ldml.h, and commit the result.
//
// It is not updated automatically.

// TODO-LDML: namespace com.keyman.core.ldml {
/**
 * Constants for the KMXPlus data format
 * These are shared between the data access layer and the compiler.
 * Note that the section IDs (section_keys etc.) are 32 bit hex
 * values that are designed to appear as text when written in little endian
 * format, so 0x7379656b = 'keys'
 */
export const constants: Constants = {
    /**
     * The version of the LDML processor
     */
    version: '1.0',
    /**
     * Length of a raw section header, in bytes
     */
    length_header: 8,

    /**
     * Minimum length of the 'sect' section, not including entries
     */
    length_sect: 16,
    length_sect_item: 8,

    /**
     * Minimum length of the 'keys' section
     * not including variable parts
     */
    length_keys: 16,
    length_keys_item: 16,

    /**
     * Minimum length of the 'loca' section
     * not including variable parts
     */
    length_loca: 16,
    length_loca_item: 4,

    /**
     * length of the 'meta' section
     */
    length_meta: 40,
    /**
     * bitwise or value for fallback=omit in meta.settings
     */
    meta_settings_fallback_omit: 1,
    /**
     * bitwise or value for transformFailure=omit in meta.settings
     */
    meta_settings_transformFailure_omit: 2,
    /**
     * bitwise or value for transformPartial=hide in meta.settings
     */
    meta_settings_transformPartial_hide: 4,

    /**
     * Minimum length of the 'strs' section
     * not including variable parts
     */
    length_strs: 16,
    /**
     * Length of each item in the 'strs' section
     * variable part
     */
    length_strs_item: 8,

    /**
     * bitwise or value for extend in keys[key].flags.
     * If bit is 1, then 'to' is a string.
     * If bit is 0, then 'to' is a UTF-32LE codepoint
     *
     * `extend = flags & keys_flags_extend`
     */
    keys_flags_extend: 1,
    /**
     * Minimum length of the 'vkey' section
     * not including variable parts
     */
    length_vkey: 16,
    length_vkey_item: 8,

    /**
     * All section IDs.
     */
    section: {
        keys: 'keys',
        loca: 'loca',
        meta: 'meta',
        sect: 'sect',
        strs: 'strs',
        vkey: 'vkey',
    },

    /**
     * Use to convert 4-char string into hex
     * @param id section id such as 'sect'
     * @returns hex ID such as 0x74636573
     */
    hex_section_id: function(id:string) {
        if(!id || typeof id !== 'string' || !id.match(/[a-z][a-z][a-z][a-z]/)) {
            throw Error(`hex_section_id(${id}) - need a 4-character string`);
        }
        let r = 0;
        for (let i = 3; i>=0; i--) {
            r = (r << 8 | id.charCodeAt(i));
        }
        return r;
    },

};
// }
