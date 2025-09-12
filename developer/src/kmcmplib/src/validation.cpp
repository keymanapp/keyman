/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-09-12
 *
 * Validation of various aspects of .kmn source files, such as identifiers
 */
#include "pch.h"

#include "km_u16.h"

#include "compfile.h"
#include "kmcmplib.h"

#include "xstring.h"
#include "validation.h"

KMX_BOOL Uni_IsSpaceCharacter(KMX_WCHAR ch);
KMX_BOOL Uni_IsValidCharacter(KMX_WCHAR *p);
KMX_BOOL Uni_IsControlCharacter(KMX_WCHAR ch);

/*
  Unicode version 16.0; GC=Zs.

  List of all space characters in Unicode 16.0, by General Category Zs:

  0020;SPACE;Zs;0;WS;;;;;N;;;;;
  00A0;NO-BREAK SPACE;Zs;0;CS;<noBreak> 0020;;;;N;NON-BREAKING SPACE;;;;
  1680;OGHAM SPACE MARK;Zs;0;WS;;;;;N;;;;;
  2000;EN QUAD;Zs;0;WS;2002;;;;N;;;;;
  2001;EM QUAD;Zs;0;WS;2003;;;;N;;;;;
  2002;EN SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
  2003;EM SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
  2004;THREE-PER-EM SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
  2005;FOUR-PER-EM SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
  2006;SIX-PER-EM SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
  2007;FIGURE SPACE;Zs;0;WS;<noBreak> 0020;;;;N;;;;;
  2008;PUNCTUATION SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
  2009;THIN SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
  200A;HAIR SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
  202F;NARROW NO-BREAK SPACE;Zs;0;CS;<noBreak> 0020;;;;N;;;;;
  205F;MEDIUM MATHEMATICAL SPACE;Zs;0;WS;<compat> 0020;;;;N;;;;;
  3000;IDEOGRAPHIC SPACE;Zs;0;WS;<wide> 0020;;;;N;;;;;

  TODO(lowpri): Use icu instead
*/

const KMX_WCHAR SpacingCharacters[] =
  u"\u0020\u00A0\u1680\u2000\u2001\u2002\u2003"
   "\u2004\u2005\u2006\u2007\u2008\u2009\u200A"
   "\u202F\u205F\u3000";

KMX_BOOL Uni_IsSpaceCharacter(KMX_WCHAR ch) {
  return !!u16chr(SpacingCharacters, ch);
}

KMX_BOOL Uni_IsValidCharacter(KMX_WCHAR const *p) {
  auto ch = Uni_UTF16ToUTF32(p);
  return
    // In range (although, not possible to be out of range with UTF-16)
    // Note: end-of-string zero-terminator is not considered a valid character
    ch > 0 && ch <= 0x10FFFF &&
    // invalid characters
    (ch & 0xFFFF) != 0xFFFF &&
    (ch & 0xFFFF) != 0xFFFE &&
    // reserved characters
    (ch < 0xFDD0 || ch > 0xFDEF) &&
    // Unpaired surrogate
    !Uni_IsSurrogate1(ch) &&
    !Uni_IsSurrogate2(ch);
}

KMX_BOOL Uni_IsControlCharacter(KMX_WCHAR ch) {
  return
    ch < 0x0020 ||
    (ch >= 0x007F && ch <= 0x009F);
}

KMX_BOOL Validation::ValidateIdentifier(KMX_WCHAR const *name, size_t maxLength) {
  // whitespace already trimmed from start and end

  // length of name > 0
  if(*name == 0) {
    this->compilerMessage.report(KmnCompilerMessages::ERROR_NameMustBeAtLeastOneCharLong);
    return FALSE;
  }

  // length of name within bounds, cater for terminating \0
  if(u16len(name) >= maxLength) {
    this->compilerMessage.report(KmnCompilerMessages::ERROR_NameMustBeAtMostNCharsLong, {std::to_string(maxLength - 1)});
    return FALSE;
  }

  // invalid characters - Unicode non-chars, any spacing characters, comma
  // close paren automatically impossible due to earlier phases in parser, but
  // including here for completeness.
  // TODO: incxstr should be `KMX_WCHAR const *`
  for(auto q = name; *q; q = incxstr(const_cast<KMX_WCHAR *>(q))) {
    if(!Uni_IsValidCharacter(q)) {
      this->compilerMessage.report(KmnCompilerMessages::ERROR_NameContainsInvalidCharacter);
      return FALSE;
    }
    if(Uni_IsControlCharacter(*q)) {
      this->compilerMessage.report(KmnCompilerMessages::ERROR_NameContainsInvalidCharacter);
      return FALSE;
    }

    if(Uni_IsSpaceCharacter(*q)) {
      this->compilerMessage.report(KmnCompilerMessages::ERROR_NameMustNotContainSpaces);
      return FALSE;
    }
    if(*q == ',') {
      this->compilerMessage.report(KmnCompilerMessages::ERROR_NameMustNotContainComma);
      return FALSE;
    }
    if(*q == '(' || *q == ')') {
      this->compilerMessage.report(KmnCompilerMessages::ERROR_NameMustNotContainParentheses);
      return FALSE;
    }
    if(*q == '[' || *q == ']') {
      this->compilerMessage.report(KmnCompilerMessages::ERROR_NameMustNotContainSquareBrackets);
      return FALSE;
    }
  }

  return TRUE;
}
