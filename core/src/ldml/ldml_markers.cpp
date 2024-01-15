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

static void add_back_markers(std::u32string &str, const std::u32string &src, const marker_map &map, marker_encoding encoding) {
  // need to reconstitute.
  marker_map map2(map);  // make a copy of the map
  // clear the string
  str.clear();
  // add the end-of-text marker
  {
    const auto ch = MARKER_BEFORE_EOT;
    const auto m  = map2.find(ch);
    if (m != map2.end()) {
      for (auto q = (m->second).rbegin(); q < (m->second).rend(); q++) {
        prepend_marker(str, *q, encoding);
      }
      map2.erase(ch);  // remove it
    }
  }
  // go from end to beginning of string
  for (auto p = src.rbegin(); p != src.rend(); p++) {
    const auto ch = *p;
    str.insert(0, 1, ch);  // prepend

    const auto m = map2.find(ch);
    if (m != map2.end()) {
      for (auto q = (m->second).rbegin(); q < (m->second).rend(); q++) {
        prepend_marker(str, *q, encoding);
      }
      map2.erase(ch);  // remove it
    }
  }
}

/**
 * TODO-LDML:
 *  - doesn't support >1 marker per char - may need a set instead of a map!
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
      str.insert(0, 1, U']');
      prepend_hex_quad(str, LDML_MARKER_MAX_INDEX);
      str.insert(0, 1, U'u');
      str.insert(0, 1, U'\\');
      str.insert(0, 1, U'-');
      prepend_hex_quad(str, LDML_MARKER_MIN_INDEX);
      str.insert(0, 1, U'u');
      str.insert(0, 1, U'\\');
      str.insert(0, 1, U'[');
      str.insert(0, 1, LDML_MARKER_CODE);
      str.insert(0, 1, LDML_UC_SENTINEL);
    } else {
      // add hex part
      prepend_hex_quad(str, marker);
      // add static part
      km_core_usv markstr[] = {LDML_UC_SENTINEL, LDML_MARKER_CODE, u'\\', u'u'};
      str.insert(0, markstr, 4);
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
void add_markers_to_map(marker_map &markers, char32_t marker_ch, const marker_list &list) {
  auto rep = markers.emplace(marker_ch, list);
  if (!rep.second) {
    // already existed.
    auto existing = rep.first;
    // append all additional ones
    for(auto m = list.begin(); m < list.end(); m++) {
      existing->second.emplace_back(*m);
    }
  }
}

std::u32string remove_markers(const std::u32string &str, marker_map *markers, marker_encoding encoding) {
  std::u32string out;
  auto i = str.begin();
  auto last = i;
  marker_list last_markers;
  for (i = find(i, str.end(), LDML_UC_SENTINEL); i != str.end(); i = find(i, str.end(), LDML_UC_SENTINEL)) {
    // append any prefix (from prior pos'n to here)
    out.append(last, i);

    // #1: LDML_UC_SENTINEL (what we searched for)
    assert(*i == LDML_UC_SENTINEL); // assert that find() worked
    i++;
    last = i;
    if (i == str.end()) {
      break; // hit end
    }

    // #2 LDML_MARKER_CODE
    if (*i != LDML_MARKER_CODE) {
      continue; // can't process this, get out
    }
    i++;
    last = i;
    if (i == str.end()) {
      break; // hit end
    }

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
      } else if (*i == U'[') {
        if (++i == str.end()) {
          break;
        }
        assert(*i == U'\\');
        if (++i == str.end()) {
          break;
        }
        assert(*i == U'u');
        if (++i == str.end()) {
          break;
        }
        assert(xdigitval(*i) != -1);
        if (++i == str.end()) {
          break;
        }
        assert(xdigitval(*i) != -1);
        if (++i == str.end()) {
          break;
        }
        assert(xdigitval(*i) != -1);
        if (++i == str.end()) {
          break;
        }
        assert(xdigitval(*i) != -1);
        if (++i == str.end()) {
          break;
        }
        assert(*i == U'-');
        if (++i == str.end()) {
          break;
        }
        assert(*i == U'\\');
        if (++i == str.end()) {
          break;
        }
        assert(*i == U'u');
        if (++i == str.end()) {
          break;
        }
        assert(xdigitval(*i) != -1);
        if (++i == str.end()) {
          break;
        }
        assert(xdigitval(*i) != -1);
        if (++i == str.end()) {
          break;
        }
        assert(xdigitval(*i) != -1);
        if (++i == str.end()) {
          break;
        }
        assert(xdigitval(*i) != -1);
        if (++i == str.end()) {
          break;
        }
        assert(*i == U']');
        i++;
        marker_no = LDML_MARKER_ANY_INDEX;
      } else {
        assert(*i == U'\\' || *i == U'[');  // error.
        marker_no = 0; // error, don't record
      }
    }
    assert(marker_no >= LDML_MARKER_MIN_INDEX && marker_no <= LDML_MARKER_ANY_INDEX);
    // The marker number is good, add it to the list
    last = i;
    // record the marker
    if (marker_no >= LDML_MARKER_MIN_INDEX && markers != nullptr) {
      // add it to the list
      last_markers.emplace_back(marker_no);
      char32_t marker_ch;
      if (i == str.end()) {
        // Hit end, so mark it as the end
        marker_ch = MARKER_BEFORE_EOT;
      } else if (*i == LDML_UC_SENTINEL) {
        // it's another marker (presumably)
        continue; // loop around
      } else {
        marker_ch = *i;
      }
      add_markers_to_map(*markers, marker_ch, last_markers);
      last_markers.clear(); // mark as already recorded
    }
  }
  // get the suffix between the last marker and the end
  out.append(last, str.end());
  if (!last_markers.empty() && markers != nullptr) {
    // we had markers but couldn't find the base.
    // it's possible that there was a malformed UC_SENTINEL string in between.
    // Add it to the end.
    add_markers_to_map(*markers, MARKER_BEFORE_EOT, last_markers);
  }
  return out;
}


}
}
}
