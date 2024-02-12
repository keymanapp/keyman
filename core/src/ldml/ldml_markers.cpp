/*
  Copyright:    © SIL International.
  Description:  Implementation for ldml marker and normalization routines
  Create Date:  6 Jan 2024
  Authors:      Steven R. Loomis
*/

#include "ldml_markers.hpp"
#include "debuglog.h"
#include <algorithm>
#include <string>
#include "kmx/kmx_xstring.h"
#include <assert.h>

#include "ldml_utils.hpp"
#include <ldml/keyman_core_ldml.h>

namespace km {
namespace core {
namespace ldml {


// the 'prefix part' of a regex marker sequence, followed by RAW_PREFIX or REGEX_ANY_MATCH
const std::u32string REGEX_PREFIX       = U"\\uffff\\u0008";
const std::u32string RAW_PREFIX         = U"\uffff\u0008";
const std::u32string REGEX_ANY_MATCH    = U"[\\u0001-\\ud7fe]";
static_assert(LDML_MARKER_NO_INDEX < LDML_MARKER_MIN_INDEX, "LDML_MARKER_NO_INDEX must be < LDML_MARKER_MIN_INDEX");

// string manipulation

/**
 * Internal function to normalize with a specified mode.
 * Note: that this function _does_ assert failure, so it is not
 * required to assert its return code. The return is provided so
 * that callers can exit (such as making no change) if there was failure.
 *
 * Also note that "failure" here is something catastrophic: ICU not initialized,
 * or, more likely, some low memory situation. Does not fail on "bad" data.
 * @param n the ICU Normalizer to use
 * @param str input/output string
 * @param status error code, must be initialized on input
 * @return false if failure
 */
static bool normalize(const icu::Normalizer2 *n, std::u16string &str, UErrorCode &status) {
  UASSERT_SUCCESS(status);
  assert(n != nullptr);
  icu::UnicodeString dest;
  icu::UnicodeString src = icu::UnicodeString(str.data(), (int32_t)str.length());
  n->normalize(src, dest, status);
  // the next line here will assert
  if (UASSERT_SUCCESS(status)) {
    str.assign(dest.getBuffer(), dest.length());
  }
  return U_SUCCESS(status);
}

bool normalize_nfd(std::u32string &str) {
  std::u16string rstr = km::core::kmx::u32string_to_u16string(str);
  if(!normalize_nfd(rstr)) {
    return false;
  } else {
    str = km::core::kmx::u16string_to_u32string(rstr);
    return true;
  }
}

bool normalize_nfd(std::u16string &str) {
  UErrorCode status = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(status);
  UASSERT_SUCCESS(status);
  return normalize(nfd, str, status);
}

marker_entry::marker_entry(char32_t c) : ch(c), marker(LDML_MARKER_NO_INDEX), processed(false), end(true) {
}

marker_entry::marker_entry(char32_t c, marker_num n) : ch(c), marker(n), processed(false), end(false) {

}

size_t count_markers(const marker_map &map) {
  size_t m = 0;
  for (auto i = map.begin(); i < map.end(); i++) {
    // add all actual markers
    if (i->marker != LDML_MARKER_NO_INDEX) {
      m++;
    }
  }
  return m;
}

void add_back_markers(std::u32string &str, const std::u32string &src, marker_map &map, marker_encoding encoding) {
  if (map.empty()) {
    // quick check, nothing to do if no markers
    str = src;
    return;
  }
  // need to reconstitute.
  marker_map map2(map);  // make a copy of the map
  // clear the string
  str.clear();
  // iterator over the marker map
  auto marki = map2.rbegin();
  // number of markers left to processnfd
  size_t max_markers = count_markers(map);
  size_t processed_markers = 0;

  // add any end-of-text markers
  while(marki != map2.rend() && marki->ch == MARKER_BEFORE_EOT) {
    if (!marki->end) {
      prepend_marker(str, marki->marker, encoding);
      processed_markers++;
    }
    marki->processed = true;  // mark as done
    marki++;
  }

  // go from end to beginning of string
  for (auto p = src.rbegin(); p != src.rend(); p++) {
    const auto ch = *p;
    str.insert(0, 1, ch);  // prepend

    // remove all processed entries, outside of an iterator
    while (!map2.empty() && map2.back().processed) {
      map2.pop_back();
    }

    // now, add any out of order markers.
    for (auto i = map2.rbegin(); i != map2.rend(); i++) {
      if ((i->ch == ch) && !(i->processed)) {
        i->processed = true;
        if (i->end) {
          break;
        } else {
          prepend_marker(str, i->marker, encoding);
          processed_markers++;
        }
      }
    }
  }
  assert(max_markers == processed_markers);  // assert that we consumed all marks
}

bool normalize_nfd_markers_segment(std::u32string &str, marker_map &map, marker_encoding encoding) {
  /** original string, but no markers */
  std::u32string str_unmarked = remove_markers(str, map, encoding);
  /** original string, no markers, NFD */
  std::u32string str_unmarked_nfd = str_unmarked;
  if(!normalize_nfd(str_unmarked_nfd)) {
    return false; // normalize failed.
  } else if (str_unmarked_nfd == str_unmarked) {
    // Normalization produced no change when markers were removed.
    // So, we'll call this a no-op.
  } else {
    add_back_markers(str, str_unmarked_nfd, map, encoding);
  }
  return true; // all OK
}

/**
 * @param i iteration point. will always advance unless at end
 * @param end end of input
 * @returns marker number, or LDML_MARKER_NO_INDEX if no marker (and i will be unmoved)
 */
marker_num parse_next_marker(std::u32string::const_iterator &i, const std::u32string::const_iterator &end, marker_encoding encoding) {
  if (i == end) {
    return LDML_MARKER_NO_INDEX;
  }
  const auto &lookfor_str = (encoding == regex_sentinel) ? REGEX_PREFIX : RAW_PREFIX;
  std::u32string rest(i, end);
  if (rest.length() <= lookfor_str.length()) { // <= because we need at least 1 char for the rest of the marker payload
    // input too short
    i++;
    return LDML_MARKER_NO_INDEX;
  }
  if (0 != rest.compare(0, lookfor_str.length(), lookfor_str)) {
    // advance past initial char
    i++;
    return LDML_MARKER_NO_INDEX; // prefix mismatch
  }
  i += lookfor_str.length(); // matches, so advance

  // handle the plain_sentinel option here
  if (encoding == plain_sentinel) {
    marker_num marker_no = *(i++); // advance past marker no
    assert(marker_no >= LDML_MARKER_MIN_INDEX && marker_no <= LDML_MARKER_ANY_INDEX);
    return marker_no;
  }

  assert(encoding == regex_sentinel);

  // TODO-LDML: could change this from iterator to using 'rest'
  if (*i == U'\\') {
    // single marker
    if (++i == end) {
      return LDML_MARKER_NO_INDEX;
    }
    assert(*i == U'u');
    if (++i == end) {
      return LDML_MARKER_NO_INDEX;
    }
    km_core_usv markno[4];

    markno[0] = *(i++);
    if (i == end) {
      return LDML_MARKER_NO_INDEX;
    }
    markno[1] = *(i++);
    if (i == end) {
      return LDML_MARKER_NO_INDEX;
    }
    markno[2] = *(i++);
    if (i == end) {
      return LDML_MARKER_NO_INDEX;
    }
    markno[3] = *(i++);
    auto marker_no = parse_hex_quad(markno);
    assert(marker_no >= LDML_MARKER_MIN_INDEX && marker_no <= LDML_MARKER_MAX_INDEX);
    return marker_no;
  } else if (*i == REGEX_ANY_MATCH.at(0)) {  // '['
    std::u32string rest2(i, end); // TODO-LDML: could use 'rest' above
    if (rest2.length() < REGEX_ANY_MATCH.length()) {
      // not enough left so it can't match, so continue
      return LDML_MARKER_NO_INDEX;
    }
    i += REGEX_ANY_MATCH.length();
    return LDML_MARKER_ANY_INDEX;
  }
  return LDML_MARKER_NO_INDEX;
}

bool normalize_nfd_markers(std::u32string &str, marker_encoding encoding) {
  // quick check - don't bother if the string is empty
  if(str.empty()) return true;

  marker_map m;
  return normalize_nfd_markers_segment(str, m, encoding);
}

void
prepend_marker(std::u32string &str, marker_num marker, marker_encoding encoding) {
  if (encoding == plain_sentinel) {
    km_core_usv markstr[] = {LDML_UC_SENTINEL, LDML_MARKER_CODE, marker};
    str.insert(0, markstr, 3);
  } else {
    assert(encoding == regex_sentinel);
    if (marker == LDML_MARKER_ANY_INDEX) {
      // recreate the regex from back to front
      str.insert(0, REGEX_ANY_MATCH);
      str.insert(0, REGEX_PREFIX);
    } else {
      // add hex part
      prepend_hex_quad(str, marker);
      // add static part
      km_core_usv markstr[] = {u'\\', u'u'};
      str.insert(0, markstr, 2);
      str.insert(0, REGEX_PREFIX);
    }
  }
}

void
prepend_hex_quad(std::u32string &str, KMX_DWORD marker) {
  for (auto i = 0; i < 4; i++) {
    KMX_DWORD remainder = marker & 0xF; // get the last nibble
    char32_t ch;
    if (remainder < 0xA) {
      ch = U'0' + remainder;
    } else {
      ch = U'A' + (remainder - 0xA);
    }
    str.insert(0, 1, ch); // prepend
    marker >>= 4;
  }
}

/** @returns hex digit value for a UTF-32 codepoint */
inline int xdigitval(km_core_usv ch) {
  if (ch >= U'0' && ch <= U'9') {
    return (ch - U'0');
  } else if (ch >= U'a' && ch <= U'f') {
    return (0xA + ch - U'a');
  } else if (ch >= U'A' && ch <= U'F') {
    return (0xA + ch - U'A');
  } else {
    return -1;
  }
}

/** @returns 32-bit value for a 4-character buffer */
KMX_DWORD parse_hex_quad(const km_core_usv hex_str[]) {
  KMX_DWORD mark_no = 0;
  for(auto i = 0; i < 4; i++) {
    mark_no <<= 4;
    auto c = hex_str[i];
    auto n = xdigitval(c);
    if (n == -1) {
      return 0;
    }
    mark_no |= n;
  }
  return mark_no;
}

/**
 * Add any markers, if needed. Inlined because we need to run it twice.
 * @param markers marker map or nullptr
 * @param last the 'last' parameter past the prior parsing
 * @param end end of the input string
 */
inline void
add_pending_markers(
    marker_map *markers,
    marker_list &last_markers,
    const std::u32string::const_iterator &last,
    const std::u32string::const_iterator &end,
    const icu::Normalizer2 *nfd) {
  // quick check to see if there's no work to do.
  if(markers == nullptr) {
    return;
  }
  /** which character this marker is 'glued' to. */
  char32_t marker_ch;
  icu::UnicodeString decomposition;
  if (last == end) {
    // at end of text, so use a special value to indicate 'EOT'.
    marker_ch = MARKER_BEFORE_EOT;
  } else {
    auto ch = *last; // non-normalized character

    // if the character is composed, we need to use the first decomposed char
    // as the 'glue'.
    if(!nfd->getDecomposition(ch, decomposition)) {
      // char does not have a decomposition - so it may be used for the glue
      marker_ch = ch;
      decomposition.remove(); // no other entries needed
    } else {
      // 'glue' is the first codepoint of the decomposition.
      marker_ch = decomposition.char32At(0);
      if (decomposition.countChar32() == 1) {
        decomposition.remove(); // no other entries needed
      } // else: will add the remainder below
    }
  }
  markers->emplace_back(marker_ch);
  // now, update the map with these markers (in order) on this character.
  for (auto i = last_markers.begin(); i < last_markers.end(); i++) {
    // marker_ch is duplicate, but keeps the structure more shallow.
    markers->emplace_back(marker_ch, *i);
  }
  // add any further entries due to decomposition
  if (!decomposition.isEmpty()) {
    // We already added the base char above, add teh rest
    for (auto i=1; i<decomposition.countChar32(); i++) {
      markers->emplace_back(decomposition.char32At(i));
    }
  }
  // clear the list
  last_markers.clear();
}

std::u32string
remove_markers(const std::u32string &str, marker_map *markers, marker_encoding encoding) {
  std::u32string out;
  marker_list last_markers;
  UErrorCode status = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(status);
  UASSERT_SUCCESS(status);

  auto last = str.begin();  // points to the part of the string after the last matched marker
  for (auto i = str.begin(); i != str.end();) {
    auto marker_no = parse_next_marker(i, str.end(), encoding);
    if (marker_no == LDML_MARKER_NO_INDEX) {
      // add any markers found before this entry, but only if there is intervening
      // text. This prevents the sentinel or the '\u' from becoming the attachment char.
      if (i != last) {
        add_pending_markers(markers, last_markers, last, str.end(), nfd);
        out.append(last, i); // append any non-marker text since the end of the last marker
        last = i; // advance over text we've already appended
      }
    } else {
      assert(marker_no >= LDML_MARKER_MIN_INDEX && marker_no <= LDML_MARKER_ANY_INDEX);
      // The marker number is good, add it to the list
      if (marker_no >= LDML_MARKER_MIN_INDEX && markers != nullptr) {
        // add it to the list
        last_markers.emplace_back(marker_no);
      }
      last = i; // skip over marker
    }
  }

  // add any remaining pending markers.
  // if last == str.end() then this wil be MARKER_BEFORE_EOT
  // otherwise it will be the glue character
  add_pending_markers(markers, last_markers, last, str.end(), nfd);
  // get the suffix between the last marker and the end (could be nothing)
  out.append(last, str.end());
  return out;
}



// --- end namespaces
}
}
}
