/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */


/**
 * Verifies that an id passed in is valid as an identifier for a Keyman .kmn
 * keyboard: starts with alpha or underscore and remainder is alphanumeric
 * @param id
 * @returns
 */
export function isValidKeymanKeyboardId(id: string) {
  return /^[a-z_][a-z0-9_]*$/.test(id);
}

/**
 * Verifies that an id passed in is valid as an identifier for an LDML xml
 * keyboard: starts with alpha or underscore and remainder is alphanumeric.
 * Note that this test is not the same as the CLDR test for valid LDML xml
 * filename, as this test has a narrower validity policy to match the Keyman
 * implementation requirements.
 * @param id
 * @returns
 */
export function isValidLdmlKeyboardId(id: string) {
  return /^[a-z_][a-z0-9_]*$/.test(id);
}

/**
 * Verifies that an id passed in is valid as an identifier for a Keyman
 * lexical model: starts with alpha or underscore and remainder is alphanumeric,
 * in three components -- author.bcp47.uniq
 * @param id
 * @returns
 */
export function isValidLexicalModelId(id: string) {
  return /^[a-z_][a-z0-9_]*\.[a-z][a-z0-9_]+\.[a-z_][a-z0-9_]*$/.test(id);
}
