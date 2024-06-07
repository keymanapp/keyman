/*
  Copyright:    © SIL International.
  Description:  Core Regex Utilities - abstract out ICU dependencies
  Create Date:  5 Jun 2024
  Authors:      Steven R. Loomis
*/

#include "util_regex.hpp"

#include "core_icu.h"
#include "kmx/kmx_xstring.h"

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#include "utfcodec.hpp"
#include <assert.h>
// JS implementations


/**
 * RegexMatchLen(pattern, input)
 * @param pattern the string, sans trailing $, for the pattern
 * @param input the text to match against
 * @return the length, in code units, of the matched portion
 */
EM_JS(int, RegexMatchLen, (const char* pattern, const char *input), {
  const DEBUG_JS = false;
  if (!pattern) return -1;
  if (!input) return -1;
  const patternstr = Module.UTF8ToString(pattern) + '$';
  const inputstr = Module.UTF8ToString(input);
  const re = new RegExp(patternstr);
  const result = re.exec(inputstr);
  if (DEBUG_JS) console.dir({patternstr,inputstr,re,result});
  if (!result) return 0; // no match
  const index = result.index;
  // code unit indices
  const startIndex = index;
  const endIndex = inputstr.length;
  const matchedText = inputstr.substring(startIndex);
  const matchedCodepoints = [...matchedText];
  if (DEBUG_JS) console.dir({index, startIndex,endIndex,matchedText,matchedCodepoints});
  return matchedCodepoints.length;
});

/**
 * RegexGroup1(pattern, input)
 * @param pattern the string, sans trailing $, for the pattern
 * @param input the text to match against
 * @return string of group 1 match or null
 */
EM_JS(char*, RegexGroup1, (const char* pattern, const char *input), {
  const DEBUG_JS = false;
  if (!pattern) return 0;
  if (!input) return 0;
  const patternstr = Module.UTF8ToString(pattern) + '$';
  const inputstr = Module.UTF8ToString(input);
  const re = new RegExp(patternstr);
  const result = re.exec(inputstr);
  if (!result) return 0; // no match
  const g1 = result[1];
  if (DEBUG_JS) console.dir({patternstr,inputstr,re,result,g1});
  if (!g1) return 0;
  return stringToNewUTF8(g1);
});


/**
 * RegexSubstitute
 * @param pattern the string, sans trailing $, for the pattern
 * @param input the text to match against
 * @param to the replacement text
 * @return the entire updated output string
 */
EM_JS(char*, RegexSubstitute, (const char* pattern, const char *input, const char *to), {
  const DEBUG_JS = false;
  if (!pattern) return -1;
  if (!input) return -1;
  const patternstr = Module.UTF8ToString(pattern) + '$';
  const inputstr = Module.UTF8ToString(input);
  const tostr = Module.UTF8ToString(to);
  const re = new RegExp(patternstr);
  const output = inputstr.replace(re, tostr);
  return stringToNewUTF8(output);
});

// pull in the generated table
#include "util_normalize_table.h"

#endif


namespace km {
namespace core {
namespace util {

/** find the */
int32_t km_regex::findIndex(const std::u32string &match, const std::deque<std::u32string> &list) {
  int32_t index = 0;
  for(auto e = list.begin(); e < list.end(); e++, index++) {
    if (match == *e) {
      return index;
    }
  }
  return -1; // not found
}

km_regex::km_regex()
#if KMN_NO_ICU
#else
 : fPattern(nullptr)
#endif
{

}


km_regex::km_regex(const km_regex& other)
#if KMN_NO_ICU
 : fPattern(other.fPattern)
#else
 : fPattern(nullptr)
#endif
{
#if KMN_NO_ICU

#else
    if (other.fPattern) {
    // clone pattern
    fPattern.reset(other.fPattern->clone());
  }
#endif
}

km_regex::km_regex(const std::u32string &pattern)
#if KMN_NO_ICU
  : fPattern(pattern)
#else
  : fPattern(nullptr)
#endif
{
  init(pattern);
}

km_regex::~km_regex() {

}

bool km_regex::valid() const {
#if KMN_NO_ICU
  return (!fPattern.empty());
#else
  // valid if fPattern is present.
  return !!fPattern;
#endif
}

bool km_regex::init(const std::u32string &pattern) {
#if KMN_NO_ICU
// for now- new regex every time.
  assert(!pattern.empty());
  fPattern = pattern;
  return true; // TODO
#else
  if (pattern.empty()) {
    return false;
  }
  // TODO-LDML: if we have mapFrom, may need to do other processing.
  std::u16string patstr = km::core::kmx::u32string_to_u16string(pattern);
  UErrorCode status           = U_ZERO_ERROR;
  /* const */ icu::UnicodeString patustr = icu::UnicodeString(patstr.data(), (int32_t)patstr.length());
  // add '$' to match to end
  patustr.append(u'$'); // TODO-LDML: may need to escape some markers. Marker #91 will look like a `[` to the pattern
  fPattern.reset(icu::RegexPattern::compile(patustr, 0, status));
  return (UASSERT_SUCCESS(status));
#endif
}

size_t km_regex::apply(const std::u32string &input, std::u32string &output,
  const std::u32string &to,
  const std::deque<std::u32string> &fromList,
  const std::deque<std::u32string> &toList ) const {
#if KMN_NO_ICU
  // length in code points of match from end
  std::string patstr = convert<char32_t,char>(fPattern);
  std::string instr = convert<char32_t,char>(input);

  /** code units */
  const auto matchLen = RegexMatchLen(patstr.c_str(), instr.c_str());
  assert(matchLen != -1); // error
  if (matchLen == 0) {
    // TODO: not correct, just trying to quell unused arg.
    return 0;
  }
  std::u32string rustr; // replacement
  if (fromList.empty()) {
    rustr = to;
  } else {

    char *group1 = RegexGroup1(patstr.c_str(), instr.c_str());
    assert(group1 != nullptr);
    const std::string group1str(group1);
    const std::u32string match32 = convert<char, char32_t>(group1str);
    free(group1);
    auto matchIndex = findIndex(match32, fromList);
    assert(matchIndex != 1L);
    rustr = toList.at(matchIndex);
  }
  std::string rstr = convert<char32_t,char>(rustr);
  // now, perform substitution
  char *out = RegexSubstitute(patstr.c_str(), instr.c_str(), rstr.c_str());
  assert(out != nullptr);
  std::string outstr(out);
  free(out);
  output = convert<char, char32_t>(outstr);
  /** code units */
  const auto matchStart = input.length() - matchLen;
  // remove the unmatched prefix.
  output.erase(0, matchStart);

  return matchLen;
#else
  assert(fPattern);
  // TODO-LDML: Really? can't go from u32 to UnicodeString?
  // TODO-LDML: Also, we could cache the u16 string at the transformGroup level or higher.
  UErrorCode status = U_ZERO_ERROR;
  const std::u16string matchstr = km::core::kmx::u32string_to_u16string(input);
  icu::UnicodeString matchustr  = icu::UnicodeString(matchstr.data(), (int32_t)matchstr.length());
  // TODO-LDML: create a new Matcher every time. These could be cached and reset.
  std::unique_ptr<icu::RegexMatcher> matcher(fPattern->matcher(matchustr, status));
  if (!UASSERT_SUCCESS(status)) {
    return 0; // TODO-LDML: return error
  }

  if (!matcher->find(status)) { // i.e. matches somewhere, in this case at end of str
    return 0; // no match
  }

  // TODO-LDML: this is UTF-16 len, not UTF-32 len!!
  // TODO-LDML: if we had an underlying UText this would be simpler.
  int32_t matchStart = matcher->start(status);
  int32_t matchEnd   = matcher->end(status);
  if (!UASSERT_SUCCESS(status)) {
    return 0; // TODO-LDML: return error
  }
  // extract..
  const icu::UnicodeString substr = matchustr.tempSubStringBetween(matchStart, matchEnd);
  // preflight to UTF-32 to get length
  UErrorCode substrStatus = U_ZERO_ERROR; // throwaway status
  // we need the UTF-32 matchLen for our return.
  auto matchLen = substr.toUTF32(nullptr, 0, substrStatus);

  // should have matched something.
  assert(matchLen > 0);

  // now, do the replace.

  /** this is the 'to' or other replacement string.*/
  icu::UnicodeString rustr;
  if (fromList.empty()) {
    // Normal case: not a map.
    // This replace will apply $1, $2 etc.
    // Convert the fTo into u16 TODO-LDML (we could cache this?)
    const std::u16string rstr = km::core::kmx::u32string_to_u16string(to);
    rustr  = icu::UnicodeString(rstr.data(), (int32_t)rstr.length());
  } else {
    // Set map case: mapping from/to

    // we actually need the group(1) string here.
    // this is only the content in parenthesis ()
    icu::UnicodeString group1 = matcher->group(1, status);
    if (!UASSERT_SUCCESS(status)) {
      // TODO-LDML: could be a malformed from pattern
      return 0; // TODO-LDML: return error
    }
    // now, how long is group1 in UTF-32, hmm?
    UErrorCode preflightStatus = U_ZERO_ERROR; // throwaway status
    auto group1Len             = group1.toUTF32(nullptr, 0, preflightStatus);
    char32_t *s                = new char32_t[group1Len + 1];
    assert(s != nullptr); // TODO-LDML: OOM
    // convert
    group1.toUTF32((UChar32 *)s, group1Len + 1, status);
    if (!UASSERT_SUCCESS(status)) {
      return 0; // TODO-LDML: memory issue
    }
    std::u32string match32(s, group1Len); // taken from just group1
    // clean up buffer
    delete [] s;

    // Now we're ready to do the actual mapping.

    // 1., we need to find the index in the source set.
    auto matchIndex = findIndex(match32, fromList);
    assert(matchIndex != -1L); // This indicates that the regex and the fromList are out of sync.
    // we already asserted on load that the from and to sets have the same cardinality.

    // 2. get the target string, convert to utf-16
    // we use the same matchIndex that was just found
    const std::u16string rstr = km::core::kmx::u32string_to_u16string(toList.at(matchIndex));

    // 3. update the UnicodeString for replacement
    rustr  = icu::UnicodeString(rstr.data(), (int32_t)rstr.length());
    // and we return to the regular code flow.
  }
  // here we replace the match output. No normalization, yet.
  icu::UnicodeString entireOutput = matcher->replaceFirst(rustr, status);
  if (!UASSERT_SUCCESS(status)) {
    // TODO-LDML: could fail here due to bad input (syntax err)
    return 0;
  }
  // entireOutput includes all of 'input', but modified. Need to substring it.
  icu::UnicodeString outu = entireOutput.tempSubString(matchStart);

  // Special case if there's no output, save some allocs
  if (outu.length() == 0) {
    output.clear();
  } else {
    // TODO-LDML: All we are trying to do is to extract the output string. Probably too many steps.
    UErrorCode preflightStatus = U_ZERO_ERROR;
    // calculate how big the buffer is
    auto out32len              = outu.toUTF32(nullptr, 0, preflightStatus); // preflightStatus will be an err, because we know the buffer overruns zero bytes
    // allocate
    std::unique_ptr<char32_t[]> s(new char32_t[out32len + 1]);
    assert(s);
    if (!s) {
      return 0; // TODO-LDML: allocation failed
    }
    // convert
    outu.toUTF32((UChar32 *)(s.get()), out32len + 1, status);
    if (!UASSERT_SUCCESS(status)) {
      return 0; // TODO-LDML: memory issue
    }
    output.assign(s.get(), out32len);
  }
  return matchLen;

#endif
}


}
}
}
