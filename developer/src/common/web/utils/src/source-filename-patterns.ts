/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Developer source filename regular expressions
 */

/**
 * A keyboard package filename SHOULD adhere to this pattern (including file
 * extension), lower case alphanumeric and underscore only allowed (a-z, _ only
 * for first letter).
 */
export const KEYBOARD_ID_PATTERN_PACKAGE = /^[a-z_][a-z0-9_]*\.(kps|kmp)$/;

/**
 * A lexical model package filename SHOULD adhere to this pattern (including
 * file extension). There are three components to the filename: author, bcp47,
 * and uniq, separated by period. The filename ends in .model.kps or .model.kmp.
 * Each of the author, bcp47, and uniq sections may contain lowercase
 * alphanumeric, underscore characters, and the bcp47 section additionally may
 * contain hyphen. Digits are not permitted as first letter of each section.
 *
 * Despite including a bcp47 tag as part of the filename, it is informative only,
 * and is not regarded as part of the metadata for the lexical model.
 */
//                                 author           .bcp47             .uniq
export const MODEL_ID_PATTERN_PACKAGE = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.(kps|kmp)$/;

// const MODEL_ID_PATTERN_JS      = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.js$/;
// const MODEL_ID_PATTERN_TS      = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.ts$/;
// const MODEL_ID_PATTERN_PROJECT = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.kpj$/;

/**
 * Filenames of files contained in a package MAY adhere to this pattern for
 * optimum cross-platform compatibility. This is the basename portion of the
 * filename, and is case-insensitive.
 */
export const CONTENT_FILE_BASENAME_PATTERN = /^[a-z0-9_+.-]+$/i; // base names can be case insensitive

/**
 * Extensions of files contained in a package MAY adhere to this pattern for optimum
 * cross-platform compatibility. This is the extension portion of the filename,
 * and should be lower case, and may be empty.
 */
export const CONTENT_FILE_EXTENSION_PATTERN = /^(\.[a-z0-9_-]+)?$/; // extensions should be lower-case or empty

