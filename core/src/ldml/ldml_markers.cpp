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

namespace km {
namespace core {
namespace ldml {


// the 'prefix part' of a regex marker sequence, followed by RAW_PREFIX or REGEX_ANY_MATCH
const std::u32string REGEX_PREFIX       = U"\\uffff\\u0008";
const std::u32string RAW_PREFIX         = U"\uffff\u0008";
const std::u32string REGEX_ANY_MATCH    = U"[\\u0001-\\ud7fe]";

// string manipulation

/** internal function to normalize with a specified mode */
static bool normalize(const icu::Normalizer2 *n, std::u16string &str, UErrorCode &status) {
  UASSERT_SUCCESS(status);
  assert(n != nullptr);
  icu::UnicodeString dest;
  icu::UnicodeString src = icu::UnicodeString(str.data(), (int32_t)str.length());
  n->normalize(src, dest, status);
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

bool normalize_nfd_markers_segment(std::u16string &str, marker_map &map, marker_encoding encoding) {
  std::u32string rstr = km::core::kmx::u16string_to_u32string(str);
  if(!normalize_nfd_markers_segment(rstr, map, encoding)) {
    return false;
  } else {
    str = km::core::kmx::u32string_to_u16string(rstr);
    return true;
  }
}

static void add_back_markers(std::u32string &str, const std::u32string &src, marker_map &map, marker_encoding encoding) {
  // need to reconstitute.
  marker_map map2(map);  // make a copy of the map
  // clear the string
  str.clear();
  // iterator over the marker map
  auto marki = map2.rbegin();

  // add any end-of-text markers
  while(marki != map2.rend() && marki->first == MARKER_BEFORE_EOT) {
    prepend_marker(str, (marki++)->second, encoding);
  }

  // go from end to beginning of string
  for (auto p = src.rbegin(); p != src.rend(); p++) {
    const auto ch = *p;
    str.insert(0, 1, ch);  // prepend

    // add the markers at the end of the list first.
    for (; marki != map2.rend() && marki->first == ch; marki++) {
      if (marki->second != 0) {
        // set to '0' if already applied
        prepend_marker(str, marki->second, encoding);
        marki->second = 0; // mark as already applied
      }
    }

    // now, add any out of order markers.
    for (auto marki2 = marki; marki2 != map2.rend(); marki2++) {
      if (marki2->second != 0 && marki2->first == ch) {
        prepend_marker(str, marki2->second, encoding);
        marki2->second = 0; // mark as already applied
      }
    }
  }
//  assert(marki == map.rend()); // that we consumed everything
}

/**
 * TODO-LDML:
 *  - ideally this should be used on a normalization safe subsequence
 */
bool normalize_nfd_markers_segment(std::u32string &str, marker_map &map, marker_encoding encoding) {
  /** original string, but no markers */
  std::u32string str_unmarked = remove_markers(str, map, encoding);
  /** original string, no markers, NFD */
  std::u32string str_unmarked_nfd = str_unmarked;
  if(!normalize_nfd(str_unmarked_nfd)) {
    return false; // normalize failed.
  } else if (map.size() == 0) {
    // no markers. Return the normalized unmarked str
    str = str_unmarked_nfd;
  } else if (str_unmarked_nfd == str_unmarked) {
    // Normalization produced no change when markers were removed.
    // So, we'll call this a no-op.
  } else {
    add_back_markers(str, str_unmarked_nfd, map, encoding);
  }
  return true; // all OK
}

bool normalize_nfd_markers(std::u16string &str, marker_encoding encoding) {
  marker_map m;
  // TODO-LDML: split segments
  return normalize_nfd_markers_segment(str, m, encoding);
}

bool normalize_nfd_markers(std::u32string &str, marker_encoding encoding) {
  marker_map m;
  // TODO-LDML: split segments
  return normalize_nfd_markers_segment(str, m, encoding);
}

// TODO-LDML: cleanup
// bool normalize_nfc_markers(std::u32string &str, marker_map &map, marker_encoding encoding) {
//   /** original string, but no markers */
//   std::u32string str_unmarked = remove_markers(str, map, encoding);
//   /** original string, no markers, NFC */
//   std::u32string str_unmarked_nfc = str_unmarked;
//   if(!normalize_nfc(str_unmarked_nfc)) {
//     return false; // normalize failed.
//   } else if (map.size() == 0) {
//     // no markers. Return the normalized unmarked str
//     str = str_unmarked_nfc;
//   } else if (str_unmarked_nfc == str_unmarked) {
//     // Normalization produced no change when markers were removed.
//     // So, we'll call this a no-op.
//   } else {
//     add_back_markers(str, str_unmarked_nfc, map, encoding);
//   }
//   return true; // all OK
// }

// TODO-LDML: cleanup
// bool normalize_nfc(std::u32string &str) {
//   std::u16string rstr = km::core::kmx::u32string_to_u16string(str);
//   if(!normalize_nfc(rstr)) {
//     return false;
//   } else {
//     str = km::core::kmx::u16string_to_u32string(rstr);
//     return true;
//   }
// }

// TODO-LDML: cleanup
// bool normalize_nfc(std::u16string &str) {
//   UErrorCode status = U_ZERO_ERROR;
//   const icu::Normalizer2 *nfc = icu::Normalizer2::getNFCInstance(status);
//   UASSERT_SUCCESS(status);
//   return normalize(nfc, str, status);
// }

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

/** add the list to the map */
static void add_markers_to_map(marker_map &markers, char32_t marker_ch, const marker_list &list) {
  for (auto i = list.begin(); i < list.end(); i++) {
    // marker_ch is duplicate, but keeps the structure more shallow.
    markers.emplace_back(marker_ch, *i);
  }
}

/**
 * Add any markers, if needed
 * @param markers marker map or nullptr
 * @param last the 'last' parameter past the prior parsing
 * @param end end of the input string
 */
static void
add_pending_markers(
    marker_map *markers,
    marker_list &last_markers,
    const std::u32string::const_iterator &last,
    const std::u32string::const_iterator &end) {
  if(markers == nullptr || last_markers.empty()) {
    return;
  }
  char32_t marker_ch;
  if (last == end) {
    marker_ch = MARKER_BEFORE_EOT;
  } else {
    marker_ch = *last;
  }
  add_markers_to_map(*markers, marker_ch, last_markers);
  last_markers.clear(); // mark as already recorded
}

std::u32string remove_markers(const std::u32string &str, marker_map *markers, marker_encoding encoding) {
  std::u32string out;
  auto i = str.begin(); // current iterator
  auto last = i; // points to the part of the string after the last matched marker
  marker_list last_markers;
  const auto &lookfor_str = (encoding == regex_sentinel) ? REGEX_PREFIX : RAW_PREFIX;
  auto lookfor = lookfor_str.at(0);

  for (i = find(i, str.end(), lookfor); i != str.end(); i = find(i, str.end(), lookfor)) {
    if (last != i) {
      // add any markers found before this entry, but only if there is intervening
      // text. This prevents the sentinel or the '\u' from becoming the attachment char.
      add_pending_markers(markers, last_markers, last, str.end());
    }
    out.append(last, i);
    assert(*i == lookfor); // assert that find() worked
    last = i; // keep track of the last segment appended.

    std::u32string rest(i, str.end());
    if (rest.length() <= lookfor_str.length()) {
      // not enough left so it can't match, so continue
      i = str.end(); // end of string
      continue;
    }

    rest.resize(lookfor_str.length());

    i += lookfor_str.length();

    if (rest != lookfor_str) {
      // no match - could be backslash something else
      continue;
    }

    assert(i != str.end()); // caught above

    KMX_DWORD marker_no;
    if (encoding == plain_sentinel) {
      // #3 marker number
      marker_no = *i;
      i++; // if end, we'll break out of the loop
    } else {
      assert(encoding == regex_sentinel);
      // is it an escape or a range?
      if (*i == U'\\') {
        if (++i == str.end()) {
          break;
        }
        assert(*i == U'u');
        if (++i == str.end()) {
          break;
        }
        km_core_usv markno[4];

        markno[0] = *(i++);
        if (i == str.end()) {
          break;
        }
        markno[1] = *(i++);
        if (i == str.end()) {
          break;
        }
        markno[2] = *(i++);
        if (i == str.end()) {
          break;
        }
        markno[3] = *(i++);
        marker_no = parse_hex_quad(markno);
        assert (marker_no != 0); // illegal marker number
      } else if (*i == REGEX_ANY_MATCH.at(0)) { // '['
        std::u32string rest2(i, str.end());
        if (rest2.length() < REGEX_ANY_MATCH.length()) {
          // not enough left so it can't match, so continue
          continue;
        }
        i += REGEX_ANY_MATCH.length();
        marker_no = LDML_MARKER_ANY_INDEX;
      } else {
        assert(*i == U'\\' || *i == U'[');  // error.
        marker_no = 0; // error, don't record
      }
    }
    assert(marker_no >= LDML_MARKER_MIN_INDEX && marker_no <= LDML_MARKER_ANY_INDEX);
    // The marker number is good, add it to the list
    last = i;
    if (marker_no >= LDML_MARKER_MIN_INDEX && markers != nullptr) {
      // add it to the list
      last_markers.emplace_back(marker_no);
    }
  }
  // add any remaining pending markers.
  // if last == str.end() then this wil be MARKER_BEFORE_EOT
  // otherwise it will be the glue character
  add_pending_markers(markers, last_markers, last, str.end());
  // get the suffix between the last marker and the end (could be nothing)
  out.append(last, str.end());
  return out;
}


}
}
}
