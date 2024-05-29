/*
  Copyright:    © SIL International.
  Description:  Internal functions for LDML markers
  Create Date:  6 Jan 2024
  Authors:      Steven R. Loomis
*/

#pragma once

#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include <deque>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include "debuglog.h"

#include "core_icu.h"
#include "unicode/uniset.h"
#include "unicode/usetiter.h"
#include "unicode/regex.h"
#include "unicode/utext.h"

namespace km {
namespace core {
namespace ldml {


/** indicates that the marker was before the end of text. */
const char32_t MARKER_BEFORE_EOT = km::core::kmx::Uni_FFFE_NONCHARACTER;

/** specify the type of encoding for marker text */
enum marker_encoding {
  /** encoding as UC_SENTINEL + CODE_DEADKEY + <number> */
  plain_sentinel,
  /** encoding as a regex matching the marker */
  regex_sentinel,
};

/** a marker ID (1-based) */
typedef KMX_DWORD marker_num;

/** list of marker numbers */
typedef std::deque<marker_num> marker_list;

struct marker_entry {
  /** code point glued to or MARKER_BEFORE_EOT */
  char32_t ch;
  /** marker number */
  marker_num marker;
  /** true if processed */
  bool processed;
  /** true if an end of this codepoint */
  bool end;

  /** add an 'end' entry */
  marker_entry(char32_t ch);
  /** add a 'marker' entry */
  marker_entry(char32_t ch, marker_num marker);

  bool operator==(const marker_entry &o) const {
    // don't test 'processed'
    return (ch == o.ch) && (marker == o.marker) && (end == o.end);
  }
};

/** map from following-char to marker numbers, in front to back order */
typedef std::deque<marker_entry> marker_map;

/** count number of non-end entries */
size_t count_markers(const marker_map &map);

/** Normalize a u32string inplace to NFD, retaining markers.
 * @param markers will be populated with marker chars
 * @return false on failure
 **/
bool normalize_nfd_markers_segment(std::u32string &str, marker_map &markers, marker_encoding encoding = plain_sentinel);
bool normalize_nfd_markers(std::u32string &str, marker_encoding encoding = plain_sentinel);

/** Remove markers and optionally note their glue characters in the map */
std::u32string remove_markers(const std::u32string &str, marker_map *markers = nullptr, marker_encoding encoding = plain_sentinel);

/** same but with a reference */
inline std::u32string remove_markers(const std::u32string &str, marker_map &markers, marker_encoding encoding = plain_sentinel)  {
  return remove_markers(str, &markers, encoding);
}

/** prepend the marker string in UC_SENTINEL format to the str */
void prepend_marker(std::u32string &str, marker_num marker, marker_encoding encoding = plain_sentinel);

/** format 'marker' as 0001...FFFF and put it at the beginning of the string */
void prepend_hex_quad(std::u32string &str, KMX_DWORD marker);

/** parse 0001...FFFF into a KMX_DWORD. Returns 0 on failure */
KMX_DWORD parse_hex_quad(const km_core_usv hex_str[]);

/** re-add markers */
void add_back_markers(std::u32string &str, const std::u32string &src, marker_map &map, marker_encoding encoding);

// bool normalize_nfc_markers(std::u16string &str, marker_encoding encoding) {
//   marker_map m;
//   return normalize_nfc_markers_segment(str, m, encoding);
// }


// bool normalize_nfc_markers(std::u32string &str, marker_encoding encoding) {
//   marker_map m;
//   return normalize_nfc_markers_segment(str, m, encoding);
// }


}  // namespace ldml
}  // namespace core
}  // namespace km
