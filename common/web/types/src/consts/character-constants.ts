/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Common character codes
 */

export enum CharacterConstant {
  DOTTED_CIRCLE = 0x25CC,  // ◌
};

export const CharacterConstantString = {
  DOTTED_CIRCLE: String.fromCodePoint(CharacterConstant.DOTTED_CIRCLE),
};
