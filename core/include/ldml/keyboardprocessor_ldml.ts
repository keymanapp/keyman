
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
 * These are shared between the data access layer and the compiler
 */
export const constants = {
    /**
     * The version of the LDML processor
     */
    version: '1.0',
    /**
     * Length of a raw section header, in bytes
     */
    length_header: 8,
    /**
     * Section ID for the keybag
     */
    section_keys: 'keys',
    /**
     * Minimum length of the 'keys' section
     * not including variable parts
     */
    length_keys: 16,
    /**
     * Section ID for the locale list
     */
    section_loca: 'loca',
    /**
     * Minimum length of the 'loca' section
     * not including variable parts
     */
    length_loca: 12,
    /**
     * Section ID for the metadata
     */
    section_meta: 'meta',
    /**
     * length of the 'meta' section
     */
    length_meta: 36,
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
     * Section ID for the section header
     */
    section_sect: 'sect',
    /**
     * Minimum length of the 'sect' section, not including entries
     */
    length_sect: 16,
    /**
     * Section ID for the string table
     */
    section_strs: 'strs',
    /**
     * Minimum length of the 'strs' section
     * not including variable parts
     */
    length_strs: 16,
    /**
     * bitwise or value for extend in keys[key].flags.
     * If bit is 1, then 'to' is a string.
     * If bit is 0, then 'to' is a UTF-32LE codepoint
     *
     * `extend = flags & keys_flags_extend`
     */
    keys_flags_extend: 1,
    /**
     * Section ID for the vkeys map
     */
    section_vkey: 'vkey',
    /**
     * Minimum length of the 'vkey' section
     * not including variable parts
     */
    length_vkey: 12,
};
// }
