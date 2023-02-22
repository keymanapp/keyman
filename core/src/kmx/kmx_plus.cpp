/*
  Copyright:        Copyright (C) 2022 SIL International.
  Authors:          srl295
  Implementation for the KMX Plus utilities
*/

#include <km_types.h>
#include <kmx_file.h>
#include <kmx/kmx_plus.h>
#include <kmx/kmx_xstring.h>

#include "kmx_processevent.h" // for debug functions
#include "ldml/keyboardprocessor_ldml.h"

#include <assert.h>

namespace km {
namespace kbp {
namespace kmx {

// double check these modifier mappings
static_assert(LCTRLFLAG == LDML_KEYS_MOD_CTRLL, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(RCTRLFLAG == LDML_KEYS_MOD_CTRLR, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(/*K_CTRLFLAG*/ (LCTRLFLAG|RCTRLFLAG) == LDML_KEYS_MOD_CTRL, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(RALTFLAG == LDML_KEYS_MOD_ALTR, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(LALTFLAG == LDML_KEYS_MOD_ALTL, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(/*K_ALTFLAG*/ (RALTFLAG|LALTFLAG) == LDML_KEYS_MOD_ALT, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(CAPITALFLAG == LDML_KEYS_MOD_CAPS, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(K_SHIFTFLAG == LDML_KEYS_MOD_SHIFT, "LDML modifier bitfield vs. kmx_file.h #define mismatch"); // "either" shift

/**
 * \def LDML_IS_VALID_MODIFIER_BITS test whether x is a valid modifier bitfield
 * i.e. contains no bits outside of LDML_KEYS_MOD_ALL
 */
#define LDML_IS_VALID_MODIFIER_BITS(x) ((LDML_KEYS_MOD_ALL & x) == x)

static_assert(LDML_IS_VALID_MODIFIER_BITS(0), "LDML_IS_VALID_MODIFIER_BITS test");
static_assert(!LDML_IS_VALID_MODIFIER_BITS(0xFFFFFF), "LDML_IS_VALID_MODIFIER_BITS(0xFFFFFF) should be 0");
static_assert(LDML_IS_VALID_MODIFIER_BITS(LDML_KEYS_MOD_CAPS), "LDML_IS_VALID_MODIFIER_BITS test");

/**
 * \def _DEBUG_IDENT_SAFE for use by DEBUG_IDENT
 */
#define _DEBUG_IDENT_SAFE(x,b) (((x)>>b)&0x7F)<0x20?'?':(((x)>>b)&0x7F)

/**
 * \def DEBUG_IDENT
 * Expands to four chars:  a,b,c,d
 * for purposes of debug logging a hex identifier
 */
#define DEBUG_IDENT(x) \
  _DEBUG_IDENT_SAFE(x,0), \
  _DEBUG_IDENT_SAFE(x,8), \
  _DEBUG_IDENT_SAFE(x,16), \
  _DEBUG_IDENT_SAFE(x,24)

/**
 * @brief Validate (and print out) a section name dword
 *
 * @param ident the hex dword
 * @return true if valid
 * @return false if invalid
 */
static bool
validate_section_name(KMX_DWORD ident) {
  for (int i = 0; i < 4; i++) {
    unsigned char ch = ident & 0xFF;
    if (ch < 0x20 || ch > 0x7F) {
      DebugLog("Invalid section name %c%c%c%c (0x%X)", DEBUG_IDENT(ident), ident);
      assert(false);
      return false;
    }
    ident >>= 8;
  }
  return true;
}

/**
 * @brief cast to a COMP_KMXPLUS_HEADER from bytes
 *
 * @param data
 * @param length
 * @param ident
 * @return const kmx::COMP_KMXPLUS_HEADER*
 */
static const kmx::COMP_KMXPLUS_HEADER *
header_from_bytes(const uint8_t *data, KMX_DWORD length, uint32_t ident) {
  if (!data) {
    DebugLog("!data");
    assert(false);
    return nullptr;
  }
  if (length < LDML_LENGTH_HEADER) {
    DebugLog("length < LDML_LENGTH_HEADER");
    assert(false);
    return nullptr;
  }
  const COMP_KMXPLUS_HEADER *all = reinterpret_cast<const COMP_KMXPLUS_HEADER *>(data);
  if (!all->valid(length)) {
    DebugLog("header failed validation");
    assert(false);
    return nullptr;
  }
  if (all->ident != ident) {
    DebugLog("header had wrong section id");
    assert(false);
    return nullptr;
  }
  return all;
}

/**
 * @brief Accessor for a section based on bytes
 *
 * @tparam T
 * @param data
 * @param length
 * @return const T*
 */
template <class T>
const T *section_from_bytes(const uint8_t *data, KMX_DWORD length) {
  if (length < sizeof(T)) { // Does not include dynamic data. First check.
    DebugLog("length < sizeof(section)");
    assert(false);
    return nullptr;
  }
  const COMP_KMXPLUS_HEADER *header = header_from_bytes(data, length, T::IDENT);
  const T *section = reinterpret_cast<const T *>(header);
  if (section != nullptr && section->valid(length)) {
    return section;
  } else {
    assert(false);
    return nullptr;
  }
}

template <class T>
const T *section_from_sect(const COMP_KMXPLUS_SECT* sect) {
  const uint8_t *rawbytes = reinterpret_cast<const uint8_t *>(sect);
  if (rawbytes == nullptr) {
    DebugLog("section_from_sect(nullptr) == nullptr");
    assert(false);
    return nullptr;
  }
  KMX_DWORD offset = sect->find(T::IDENT);
  if (!offset) {
    DebugLog("section_from_sect() - not found. section %c%c%c%c (0x%X)", DEBUG_IDENT(T::IDENT), T::IDENT);
    return nullptr;
  }
  KMX_DWORD entrylength = sect->total - offset;
  return section_from_bytes<T>(rawbytes+offset, entrylength);
}

bool
COMP_KMXPLUS_HEADER::valid(KMX_DWORD length) const {
  DebugLog("%c%c%c%c: (%X) size 0x%X\n", DEBUG_IDENT(ident), ident, size);
  if (size < LDML_LENGTH_HEADER) {
    DebugLog("size < LDML_LENGTH_HEADER");
    assert(false);
    return false;
  }
  if (size > length) {
    DebugLog("size > length");
    assert(false);
    return false;
  }
  if (!validate_section_name(ident)) {
    return false;
  }
  DebugLog(" (header OK)"); // newline after section name
  return true;
}

bool
COMP_KMXPLUS_LOCA::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  for(KMX_DWORD i=0; i<count; i++) {
    DebugLog(" Locale #%d: #0x%X\n", i, entries[i].locale);
  }
  return true;
}

bool
COMP_KMXPLUS_META::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  DebugLog(" author:\t#0x%X\n", author);
  DebugLog(" conform:\t#0x%X\n", conform);
  DebugLog(" layout:\t#0x%X\n", layout);
  DebugLog(" normalization:\t#0x%X\n", normalization);
  DebugLog(" indicator:\t#0x%X\n", indicator);
  DebugLog(" settings:\t0x%X\n", settings);
  return true;
}

bool
COMP_KMXPLUS_VKEY::valid(KMX_DWORD _kmn_unused(length)) const {
  DebugLog("vkey: count 0x%X\n", count);
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  for (KMX_DWORD i = 0; i < count; i++) {
    DebugLog("vkey #0x%X: 0x%X->0x%X", i, entries[i].vkey, entries[i].target);
    if (entries[i].vkey > 0xFF || entries[i].target > 0xFF) {
      DebugLog("vkey source or target out of range [0x00…0xFF]");
      assert(false);
      return false;
    }
  }
  return true;
}

bool
COMP_KMXPLUS_DISP::valid(KMX_DWORD _kmn_unused(length)) const {
  DebugLog("disp: count 0x%X\n", count);
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  if (baseCharacter != 0) {
    DebugLog("disp: baseCharacter str#0x%X", baseCharacter);
  }
  for (KMX_DWORD i=0; i<count; i++) {
    DebugLog("disp#%d: to: str0x%X -> str0x%X", i, entries[i].to, entries[i].display);
    if (entries[i].to == 0 || entries[i].display == 0) {
      DebugLog("disp to: or display: has a zero string");
      assert(false);
      return false;
    }
  }
  return true;
}

bool
COMP_KMXPLUS_STRS::valid(KMX_DWORD _kmn_unused(length)) const {
  DebugLog("strs: count 0x%X\n", count);
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  for (KMX_DWORD i=0; i<count; i++) {
    DebugLog("#0x%X: ...", i);
    KMX_DWORD offset = entries[i].offset;
    KMX_DWORD length = entries[i].length;
    if(offset+((length+1)*2) > header.size) {
      DebugLog("#0x%X: expected end of string past header.size", i);
      assert(false);
      return false;
    }
    const uint8_t* thisptr = reinterpret_cast<const uint8_t*>(this);
    const KMX_WCHAR* start = reinterpret_cast<const KMX_WCHAR*>(thisptr+offset);
    if(start[length] != 0) {
      DebugLog("#0x%X: String of length 0x%x not null terminated", i, length);
      assert(start[length] == 0);
      return false;
    }
    // TODO-LDML: validate valid UTF-16LE?
    DebugLog("#0x%X: '%s'", i, Debug_UnicodeString(start));
  }
  return true;
}

bool
COMP_KMXPLUS_SECT::valid(KMX_DWORD length) const {
  DebugLog("sect: total 0x%X\n", total);
  DebugLog("sect: count 0x%X\n", count);
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }

  // now validate each component
  bool overall_valid = true;
  for (KMX_DWORD i = 0; i < count; i++) {
    const COMP_KMXPLUS_SECT_ENTRY& entry = entries[i];
    DebugLog("%c%c%c%c #%d: %X @ %X\n", DEBUG_IDENT(entry.sect), i, entry.sect, entry.offset);
    if(!validate_section_name(entry.sect)) {
      return false;
    }
    if (entry.sect == LDML_SECTIONID_SECT) {
      DebugLog("Invalid nested 'sect'");
      overall_valid = false;
      continue;
    }
    const uint8_t* data = reinterpret_cast<const uint8_t *>(this);
    const uint8_t* entrydata = data + entry.offset;
    KMX_DWORD entrylength = length - entry.offset;
    // just validate header
    if(header_from_bytes(entrydata, entrylength, entry.sect) == nullptr) {
      DebugLog("Invalid header %X", entry.sect);
      assert(false);
      overall_valid = false;
      continue;
    }
  }
  return overall_valid;
}

// ---- transform related fcns
bool
COMP_KMXPLUS_ELEM::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size 0x%X < expected size");
    return false;
  }
  const COMP_KMXPLUS_ELEM_ENTRY &firstEntry = entries[0];
  if (firstEntry.length != 0 || firstEntry.offset != 0) {
    DebugLog("ERROR: elem[0].length=0x%x, elem[0].offset=0x%x, both should be zero",
      firstEntry.length, firstEntry.offset);
    return false;
  }
  for (KMX_DWORD e = 1; e < count; e++) {
    // Don't need to recheck the first entry hbere.
    KMX_DWORD listLength;
    if (getElementList(e, listLength) == nullptr) {
      return false;
    }
    if (listLength == 0) { // only the first element should have length zero
      DebugLog("ERROR: elem[0x%x].length == 0", e);
      return false;
    }
  }
  return true;
}

const COMP_KMXPLUS_ELEM_ELEMENT *
COMP_KMXPLUS_ELEM::getElementList(KMX_DWORD elementNumber, KMX_DWORD &length) const {
  if (elementNumber >= count) {
    DebugLog("ERROR: COMP_KMXPLUS_ELEM::getElementList(%d) >= count %d", elementNumber, count);
    assert(false);
    return nullptr;
  }
  const COMP_KMXPLUS_ELEM_ENTRY &entry = entries[elementNumber];
  length = entry.length;
  if (length == 0) {
    return nullptr; // Normal case for first element
  }
  if (entry.offset + (entry.length * sizeof(COMP_KMXPLUS_ELEM_ELEMENT)) > header.size) {
    DebugLog("ERROR: !! COMP_KMXPLUS_ELEM::getElementList(%d) would be off end of data area", elementNumber);
    assert(false);
    return nullptr;
  }
  // pointer to beginning of elem section
  const uint8_t *rawdata = reinterpret_cast<const uint8_t *>(this);
  // pointer to specified entry
  return reinterpret_cast<const COMP_KMXPLUS_ELEM_ELEMENT *>(rawdata + entry.offset);
}

std::u16string
COMP_KMXPLUS_ELEM_ELEMENT::get_string() const {
  assert(!(flags & LDML_ELEM_FLAGS_UNICODE_SET)); // should not be called.
  char16_single buf;
  const int len = Utf32CharToUtf16(element, buf);
  return std::u16string(buf.ch, len);
}

bool
COMP_KMXPLUS_TRAN::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  // TODO-LDML
  DebugLog("!! More to do here.");
  return true;
}

bool
COMP_KMXPLUS_LAYR::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)
      + (listCount  * sizeof(COMP_KMXPLUS_LAYR_LIST))
      + (layerCount * sizeof(COMP_KMXPLUS_LAYR_ENTRY))
      + (rowCount   * sizeof(COMP_KMXPLUS_LAYR_ROW))
      + (keyCount   * sizeof(COMP_KMXPLUS_LAYR_KEY))) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  DebugLog("layr header is valid");
  // Note: We only do minimal validation here because of the
  // dynamic structure. See COMP_KMXPLUS_LAYR_Helper.setLayr()  (below)
  // all remaining checks
  return true;
}

COMP_KMXPLUS_LAYR_Helper::COMP_KMXPLUS_LAYR_Helper() : layr(nullptr), is_valid(false) {
}

bool
COMP_KMXPLUS_LAYR_Helper::setLayr(const COMP_KMXPLUS_LAYR *newLayr) {
  DebugLog("validating newLayr=%p", newLayr);
  is_valid = true;
  if (newLayr == nullptr) {
    // null = invalid
    is_valid = false;
    // No assert here: just a missing layer
    return false;
  }
  layr = newLayr;
  const uint8_t *rawdata = reinterpret_cast<const uint8_t *>(newLayr);
  rawdata += LDML_LENGTH_LAYR;  // skip past non-dynamic portion
  // lists
  if (layr->listCount > 0) {
    lists = reinterpret_cast<const COMP_KMXPLUS_LAYR_LIST *>(rawdata);
  } else {
    lists    = nullptr;
    is_valid = false;
    assert(is_valid);
  }
  rawdata += sizeof(COMP_KMXPLUS_LAYR_LIST) * layr->listCount;
  // entries
  if (layr->layerCount > 0) {
    entries = reinterpret_cast<const COMP_KMXPLUS_LAYR_ENTRY *>(rawdata);
  } else {
    entries  = nullptr;
    is_valid = false;
    assert(is_valid);
  }
  rawdata += sizeof(COMP_KMXPLUS_LAYR_ENTRY) * layr->layerCount;
  // rows
  if (layr->rowCount > 0) {
    rows = reinterpret_cast<const COMP_KMXPLUS_LAYR_ROW *>(rawdata);
  } else {
    rows     = nullptr;
    is_valid = false;
    assert(is_valid);
  }
  rawdata += sizeof(COMP_KMXPLUS_LAYR_ROW) * layr->rowCount;
  // keys
  if (layr->keyCount > 0) {
    keys = reinterpret_cast<const COMP_KMXPLUS_LAYR_KEY *>(rawdata);
  } else {
    keys     = nullptr;
    is_valid = false;
    assert(is_valid);
  }

  // Now, validate offsets by walking
  if (is_valid) {
    for(KMX_DWORD i = 0; is_valid && i < layr->listCount; i++) {
      const COMP_KMXPLUS_LAYR_LIST &list = lists[i];
      // is the count off the end?
      DebugLog(
          "<layers> %d: hardware s#0x%X, layers %d..%d, minDeviceWidth %.1fmm", i, list.hardware, list.layer,
          list.layer + list.count - 1, list.minDeviceWidth * (double)0.1);
      if ((list.layer >= layr->layerCount) || (list.layer + list.count > layr->layerCount)) {
        DebugLog("COMP_KMXPLUS_LAYR_Helper: list[%d] would access layer %d+%d, > count %d",
            i, list.layer, list.count, layr->layerCount);
        is_valid = false;
        assert(is_valid);
      }
    }
    for(KMX_DWORD i = 0; is_valid && i < layr->layerCount; i++) {
      const COMP_KMXPLUS_LAYR_ENTRY &entry = entries[i];
      // is the count off the end?
      DebugLog(
          "<layer> %d: id s#0x%X, rows %d..%d, modifier=0x%X", i, entry.id, entry.row, entry.row+entry.count-1, entry.mod);
      if ((entry.row >= layr->rowCount) || (entry.row + entry.count > layr->rowCount)) {
        DebugLog("COMP_KMXPLUS_LAYR_Helper: entry[%d] would access row %d+%d, > count %d",
            i, entry.row, entry.count, layr->rowCount);
        is_valid = false;
        assert(is_valid);
      }
      if (!LDML_IS_VALID_MODIFIER_BITS(entry.mod)) {
        DebugLog("Invalid modifier value");
        assert(false);
        return false;
      }
    }
    for(KMX_DWORD i = 0; is_valid && i < layr->rowCount; i++) {
      const COMP_KMXPLUS_LAYR_ROW &row = rows[i];
      // is the count off the end?
      if ((row.key >= layr->keyCount) || (row.key + row.count > layr->keyCount)) {
        DebugLog("COMP_KMXPLUS_LAYR_Helper: row[%d] would access key %d+%d, > count %d",
            i, row.key, row.count, layr->keyCount);
        is_valid = false;
        assert(is_valid);
      }
    }
  }
  // Return results
  DebugLog("COMP_KMXPLUS_LAYR_Helper.setLayr(): %s", is_valid ? "valid" : "invalid");
  assert(is_valid);
  return is_valid;
}

bool COMP_KMXPLUS_LAYR_Helper::valid() const {
  return is_valid;
}

const COMP_KMXPLUS_LAYR_LIST *
COMP_KMXPLUS_LAYR_Helper::getList(KMX_DWORD list) const {
  if (!valid() || list >= layr->listCount) {
    assert(false);
    return nullptr;
  }
  return lists + list;
}

const COMP_KMXPLUS_LAYR_ENTRY *
COMP_KMXPLUS_LAYR_Helper::getEntry(KMX_DWORD entry) const {
  if (!valid() || entry >= layr->layerCount) {
    assert(false);
    return nullptr;
  }
  return entries + entry;
}

const COMP_KMXPLUS_LAYR_ROW *
COMP_KMXPLUS_LAYR_Helper::getRow(KMX_DWORD row) const {
  if (!valid() || row >= layr->rowCount) {
    assert(false);
    return nullptr;
  }
  return rows + row;
}

const COMP_KMXPLUS_LAYR_KEY *
COMP_KMXPLUS_LAYR_Helper::getKey(KMX_DWORD key) const {
  if (!valid() || key >= layr->keyCount) {
    assert(false);
    return nullptr;
  }
  return keys + key;
}

bool
COMP_KMXPLUS_KEY2::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)
      + (keyCount    * sizeof(COMP_KMXPLUS_KEY2_KEY))
      + (flicksCount * sizeof(COMP_KMXPLUS_KEY2_FLICK_LIST))
      + (flickCount  * sizeof(COMP_KMXPLUS_KEY2_FLICK_ELEMENT))
      + (kmapCount   * sizeof(COMP_KMXPLUS_KEY2_KMAP))) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  // further validation in the COMP_KMXPLUS_KEY2_Helper helper obj
  return true;
}


COMP_KMXPLUS_KEY2_Helper::COMP_KMXPLUS_KEY2_Helper() : key2(nullptr), is_valid(false) {
}

bool
COMP_KMXPLUS_KEY2_Helper::setKey2(const COMP_KMXPLUS_KEY2 *newKey2) {
  DebugLog("validating newKey2=%p", newKey2);
  is_valid = true;
  if (newKey2 == nullptr) {
    // null = invalid
    is_valid = false;
    // No assert here: just a missing layer
    return false;
  }
  key2 = newKey2;
  const uint8_t *rawdata = reinterpret_cast<const uint8_t *>(newKey2);
  rawdata += LDML_LENGTH_KEY2;  // skip past non-dynamic portion
  // keys
  if (key2->keyCount > 0) {
    keys = reinterpret_cast<const COMP_KMXPLUS_KEY2_KEY *>(rawdata);
  } else {
    keys    = nullptr;
    is_valid = false;
    assert(is_valid);
  }
  rawdata += sizeof(COMP_KMXPLUS_KEY2_KEY) * key2->keyCount;
  // flicks
  if (key2->flicksCount > 0) {
    flickLists = reinterpret_cast<const COMP_KMXPLUS_KEY2_FLICK_LIST *>(rawdata);
  } else {
    flickLists  = nullptr; // not an error
  }
  rawdata += sizeof(COMP_KMXPLUS_KEY2_FLICK_LIST) * key2->flicksCount;
  // flick
  if (key2->flickCount > 0) {
    flickElements = reinterpret_cast<const COMP_KMXPLUS_KEY2_FLICK_ELEMENT *>(rawdata);
  } else {
    flickElements = nullptr; // not an error
  }
  rawdata += sizeof(COMP_KMXPLUS_KEY2_FLICK_ELEMENT) * key2->flickCount;
  // kmap
  if (key2->kmapCount > 0) {
    kmap = reinterpret_cast<const COMP_KMXPLUS_KEY2_KMAP *>(rawdata);
  } else {
    kmap = nullptr; // not an error
  }

  // Now, validate offsets by walking
  if (is_valid) {
    for(KMX_DWORD i = 0; is_valid && i < key2->keyCount; i++) {
      const auto &key = keys[i];
      // is the count off the end?
      DebugLog( "<key #%d> id=0x%X, to=0x%X, flicks=%d", i, key.id, key.to, key.flicks); // TODO-LDML: could dump more fields here
      if (key.flicks >0 && key.flicks >= key2->flicksCount) {
        DebugLog("key[%d] has invalid flicks index %d", i, key.flicks);
        is_valid = false;
        assert(is_valid);
      }
    }
    for(KMX_DWORD i = 0; is_valid && i < key2->flicksCount; i++) {
      const auto &e = flickLists[i];
      // is the count off the end?
      DebugLog("<flicks> %d: index %d, count %d", i, e.flick, e.count);
      if (i == 0) {
        if (e.flick != 0 || e.count != 0) {
          DebugLog("Error: Invalid Flick #0");
          is_valid = false;
          assert(is_valid);
        }
      } else if ((e.flick >= key2->flickCount) || (e.flick + e.count > key2->flickCount)) {
        DebugLog("flicks[%d] would access flick %d+%d, > count %d", i, e.flick, e.count, key2->flickCount);
        is_valid = false;
        assert(is_valid);
      }
    }
    for(KMX_DWORD i = 0; is_valid && i < key2->flickCount; i++) {
      const auto &e = flickElements[i];
      // is the count off the end?
      DebugLog("<flick> %d: to=0x%X, directions=0x%X, flags=0x%X", i, e.to, e.directions, e.flags);
    }
    // now the kmap
    DebugLog(" kmap count: #0x%X\n", key2->kmapCount);
    for (KMX_DWORD i = 0; i < key2->kmapCount; i++) {
      DebugLog(" #0x%d\n", i);
      auto &entry = kmap[i];
      DebugLog("  vkey\t0x%X\n", entry.vkey);
      DebugLog("  mod\t0x%X\n", entry.mod);
      DebugLog("  key\t#0x%X\n", entry.key);
      if (!LDML_IS_VALID_MODIFIER_BITS(entry.mod)) {
        DebugLog("Invalid modifier value");
        assert(false);
        is_valid = false;
      }
      if (entry.key >= key2->keyCount) {
        // preposterous key #
        DebugLog("kmap[0x%X].key = #0x%X, but that is >= keyCount 0x%X", i, entry.key, key2->keyCount);
        assert(false);
        is_valid = false;
      }
    }
  }
  // Return results
  DebugLog("COMP_KMXPLUS_KEY2_Helper.setKey2(): %s", is_valid ? "valid" : "invalid");
  assert(is_valid);
  return is_valid;
}

const COMP_KMXPLUS_KEY2_KEY *
COMP_KMXPLUS_KEY2_Helper::getKeys(KMX_DWORD i) const {
  if (!valid() || i >= key2->keyCount) {
    assert(false);
    return nullptr;
  }
  return keys + i;
}

const COMP_KMXPLUS_KEY2_KEY*
COMP_KMXPLUS_KEY2_Helper::findKeyByStringId(KMX_DWORD strId, KMX_DWORD &i) const {
  for (i = 0; i < key2->keyCount; i++) {
    if (keys[i].id == strId) {
      return &keys[i];
    }
  }
  return nullptr;
}

const COMP_KMXPLUS_KEY2_FLICK_LIST *
COMP_KMXPLUS_KEY2_Helper::getFlickLists(KMX_DWORD i) const {
  if (!valid() || i >= key2->flicksCount) {
    assert(false);
    return nullptr;
  }
  return flickLists + i;
}

const COMP_KMXPLUS_KEY2_FLICK_ELEMENT *
COMP_KMXPLUS_KEY2_Helper::getFlickElements(KMX_DWORD i) const {
  if (!valid() || i >= key2->flickCount) {
    assert(false);
    return nullptr;
  }
  return flickElements + i;
}

const COMP_KMXPLUS_KEY2_KMAP *
COMP_KMXPLUS_KEY2_Helper::getKmap(KMX_DWORD i) const {
  if (!valid() || i >= key2->kmapCount) {
    assert(false);
    return nullptr;
  }
  return kmap + i;
}

std::u16string
COMP_KMXPLUS_KEY2_KEY::get_string() const {
  assert(!(flags & LDML_KEY2_KEY_FLAGS_EXTEND)); // should not be called.
  char16_single buf;
  const int len = Utf32CharToUtf16(to, buf);
  return std::u16string(buf.ch, len);
}

// LIST

bool
COMP_KMXPLUS_LIST::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)
      + (listCount  * sizeof(COMP_KMXPLUS_LIST_ITEM))
      + (indexCount * sizeof(COMP_KMXPLUS_LIST_INDEX))) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  // TODO-LDML: further validation in the COMP_KMXPLUS_LIST_Helper class
  return true;
}


COMP_KMXPLUS_LIST_Helper::COMP_KMXPLUS_LIST_Helper() : list(nullptr), is_valid(false) {
}

bool
COMP_KMXPLUS_LIST_Helper::setList(const COMP_KMXPLUS_LIST *newList) {
  DebugLog("validating newList=%p", newList);
  is_valid = true;
  if (newList == nullptr) {
    // null = invalid
    is_valid = false;
    // No assert here: just a missing layer
    return false;
  }
  list = newList;
  const uint8_t *rawdata = reinterpret_cast<const uint8_t *>(newList);
  rawdata += LDML_LENGTH_LIST;  // skip past non-dynamic portion
  // lists
  if (list->listCount > 0) {
    lists = reinterpret_cast<const COMP_KMXPLUS_LIST_ITEM *>(rawdata);
  } else {
    lists    = nullptr;
    // not invalid, just empty.
  }
  rawdata += sizeof(COMP_KMXPLUS_LIST_ITEM) * list->listCount;
  // entries
  if (list->indexCount > 0) {
    indices = reinterpret_cast<const COMP_KMXPLUS_LIST_INDEX *>(rawdata);
  } else {
    indices  = nullptr;
  }
  // rawdata += sizeof(COMP_KMXPLUS_LIST_INDEX) * list->indexCount;

  // Now, validate offsets by walking
  if (is_valid) {
    for (KMX_DWORD i = 0; is_valid && i < list->listCount; i++) {
      const auto &e = lists[i];
      // is the count off the end?
      DebugLog("list 0x%X: index %d, count %d", i, e.index, e.count);
      if (i == 0) {
        if (e.index != 0 || e.count != 0) {
          DebugLog("Error: Invalid List #0");
          is_valid = false;
          assert(is_valid);
        }
      } else if ((e.index >= list->indexCount) || (e.index + e.count > list->indexCount)) {
        DebugLog("list[%d] would access index %d+%d, > count %d", i, e.index, e.count, list->indexCount);
        is_valid = false;
        assert(is_valid);
      }
    }
    for (KMX_DWORD i = 0; is_valid && i < list->indexCount; i++) {
      const auto &e = indices[i];
      DebugLog(" index %d: str 0x%X", i, e);
    }
  }
  // Return results
  DebugLog("COMP_KMXPLUS_LIST_Helper.setList(): %s", is_valid ? "valid" : "invalid");
  assert(is_valid);
  return is_valid;
}

const COMP_KMXPLUS_LIST_ITEM *
COMP_KMXPLUS_LIST_Helper::getList(KMX_DWORD i) const {
  if (!valid() || i >= list->listCount) {
    assert(false);
    return nullptr;
  }
  return lists + i;
}

const COMP_KMXPLUS_LIST_INDEX *
COMP_KMXPLUS_LIST_Helper::getIndex(KMX_DWORD i) const {
  if (!valid() || i >= list->indexCount) {
    assert(false);
    return nullptr;
  }
  return indices + i;
}

// ---- constructor

kmx_plus::kmx_plus(const COMP_KEYBOARD *keyboard, size_t length)
    : loca(nullptr), meta(nullptr),
      sect(nullptr), strs(nullptr), vkey(nullptr), valid(false) {

  DebugLog("kmx_plus(): Got a COMP_KEYBOARD at %p\n", keyboard);
  if (!(keyboard->dwFlags & KF_KMXPLUS)) {
    DebugLog("Err: flags COMP_KEYBOARD.dwFlags did not have KF_KMXPLUS set");
    valid = false;
    assert(valid);
    return;
  }
  const COMP_KEYBOARD_EX* ex = reinterpret_cast<const COMP_KEYBOARD_EX*>(keyboard);

  DebugLog("kmx_plus(): KMXPlus offset 0x%X, KMXPlus size 0x%X\n", ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize);
  if (ex->kmxplus.dpKMXPlus < sizeof(kmx::COMP_KEYBOARD_EX)) {
    DebugLog("dwKMXPlus is not past the end of COMP_KEYBOARD_EX");
    valid = false;
    assert(valid);
    return;
  }
  if ( ex->kmxplus.dpKMXPlus + ex->kmxplus.dwKMXPlusSize > length) {
    DebugLog("dpKMXPlus + dwKMXPlusSize is past the end of the file");
    valid = false;
    assert(valid);
    return;
  }

  const uint8_t* rawdata = reinterpret_cast<const uint8_t*>(keyboard);

  sect = section_from_bytes<COMP_KMXPLUS_SECT>(rawdata+ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize);
  if (sect == nullptr) {
    DebugLog("kmx_plus(): 'sect' did not validate");
    valid = false;
  } else {
    valid = true;
    // load other sections, validating as we go
    // these will be nullptr if they don't validate
    disp = section_from_sect<COMP_KMXPLUS_DISP>(sect);
    elem = section_from_sect<COMP_KMXPLUS_ELEM>(sect);
    key2 = section_from_sect<COMP_KMXPLUS_KEY2>(sect);
    layr = section_from_sect<COMP_KMXPLUS_LAYR>(sect);
    list = section_from_sect<COMP_KMXPLUS_LIST>(sect);
    loca = section_from_sect<COMP_KMXPLUS_LOCA>(sect);
    meta = section_from_sect<COMP_KMXPLUS_META>(sect);
    strs = section_from_sect<COMP_KMXPLUS_STRS>(sect);
    tran = section_from_sect<COMP_KMXPLUS_TRAN>(sect);
    vkey = section_from_sect<COMP_KMXPLUS_VKEY>(sect);

    // calculate and validate the dynamic parts
    (void)key2Helper.setKey2(key2);
    (void)layrHelper.setLayr(layr);
    (void)listHelper.setList(list);
  }
}

KMX_DWORD COMP_KMXPLUS_SECT::find(KMX_DWORD ident) const {
  for (KMX_DWORD i = 0; i < count; i++) {
    if (ident == entries[i].sect) {
        return entries[i].offset;
    }
  }
  return 0;
}

std::u16string
COMP_KMXPLUS_STRS::get(KMX_DWORD entry) const {
    assert(entry < count);
    if (entry >= count) {
        return std::u16string(); // Fallback: empty string
    }
    const KMX_DWORD offset = entries[entry].offset;
    const KMX_DWORD length = entries[entry].length;
    assert(offset+((length+1)*2) <= header.size); // assert not out of bounds
    const uint8_t* thisptr = reinterpret_cast<const uint8_t*>(this);
    const KMX_WCHAR* start = reinterpret_cast<const KMX_WCHAR*>(thisptr+offset);
    return std::u16string(start, length);
}

KMX_DWORD COMP_KMXPLUS_STRS::find(const std::u16string& s) const {
  if (s.empty()) {
    return 0; // shortcut
  }
  // TODO-LDML: You're not going to search these linearly, reinterpreting each time??!
  for (KMX_DWORD i = 0; i<count; i++) {
    if (s == get(i)) {
      return i;
    }
  }
  return 0; // not found
}


}  // namespace kmx
}  // namespace kbp
}  // namespace km
