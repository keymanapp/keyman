// The keyboard ID SHOULD adhere to this pattern:
export const KEYBOARD_ID_PATTERN_PACKAGE = /^[a-z_][a-z0-9_]*\.(kps|kmp)$/;
// The model ID SHOULD adhere to this pattern:
//                                 author           .bcp47             .uniq
export const MODEL_ID_PATTERN_PACKAGE = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.(kps|kmp)$/;
// const MODEL_ID_PATTERN_JS      = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.js$/;
// const MODEL_ID_PATTERN_TS      = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.ts$/;
// const MODEL_ID_PATTERN_PROJECT = /^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.kpj$/;
// "Content files" within the package should adhere to these pattern:
export const CONTENT_FILE_BASENAME_PATTERN = /^[a-z0-9_+.-]+$/i; // base names can be case insensitive

export const CONTENT_FILE_EXTENSION_PATTERN = /^(\.[a-z0-9_-]+)?$/; // extensions should be lower-case or empty

