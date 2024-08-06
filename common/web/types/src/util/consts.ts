// TODO-LDML: #7569 the below regex works, but captures more than it should
// (it would include \u{fffffffffffffffff } which
// is overlong and has a space at the end.) The second regex does not work yet.
export const MATCH_HEX_ESCAPE = /\\u{([0-9a-fA-F ]{1,})}/g;
// const MATCH_HEX_ESCAPE = /\\u{((?:(?:[0-9a-fA-F]{1,5})|(?:10[0-9a-fA-F]{4})(?: (?!}))?)+)}/g;

/** regex for single quad escape such as \u0127 or \U00000000 */
export const CONTAINS_QUAD_ESCAPE = /(?:\\u([0-9a-fA-F]{4})|\\U([0-9a-fA-F]{8}))/;

/** regex for single quad escape such as \u0127 */
export const MATCH_QUAD_ESCAPE = new RegExp(CONTAINS_QUAD_ESCAPE, 'g');

