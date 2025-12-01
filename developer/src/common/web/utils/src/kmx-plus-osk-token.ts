/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

/**
 * In a KMX+ file, when we first build it, we reserve space for the font
 * facename to be rewritten, with a magic token that the package compiler will
 * search for; see kmp-compiler.ts and embed-osk.ts. This is the token that
 * marks the rewritten space; it is 32 characters long, to match the maximum
 * font facename length (in Windows).
 *
 * Background: this comes from a legacy design decision, where responsibility for
 * font selection is made in the package, referencing the relevant font .ttf
 * files. This forces a rewrite of font metadata in .kvk and .kmx files when they
 * are compiled into a .kmp file.
 */
export const oskFontMagicToken = "*OSK-FONT-MAGIC-TOKEN-OSK-FONT*";
