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

#include <assert.h>

namespace km {
namespace kbp {
namespace kmx {

/**
 * @brief Validate (and print out) a section name dword
 *
 * @param ident the hex dword
 * @return true if valid
 * @return false if invalid
 */
static bool
validate_section_name(KMX_DWORD ident) {
  bool valid = true;
  for (int i = 0; i < 4; i++) {
    unsigned char ch = ident & 0xFF;
    if (ch < 0x20 || ch > 0x7F) {
      valid = false;
      DebugLog("\\x%02X", ch);
    } else {
      DebugLog("%c", ch);
    }
    ident >>= 8;
  }
  return valid;
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
    return nullptr;
  }
  if (length < LDML_LENGTH_HEADER) {
    DebugLog("length < LDML_LENGTH_HEADER");
  }
  const COMP_KMXPLUS_HEADER *all = reinterpret_cast<const COMP_KMXPLUS_HEADER *>(data);
  if (!all->valid(length)) {
    DebugLog("header failed validation");
    return nullptr;
  }
  if (all->ident != ident) {
    DebugLog("header had wrong section id");
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
    return nullptr;
  }
  const COMP_KMXPLUS_HEADER *header = header_from_bytes(data, length, T::IDENT);
  const T *section = reinterpret_cast<const T *>(header);
  if (section != nullptr && section->valid(length)) {
    return section;
  } else {
    return nullptr;
  }
}

template <class T>
const T *section_from_sect(const COMP_KMXPLUS_SECT* sect) {
  DebugLog("----"); // separate a new section's validation
  const uint8_t *rawbytes = reinterpret_cast<const uint8_t *>(sect);
  if (rawbytes == nullptr) {
    DebugLog("section_from_sect(nullptr) == nullptr");
    return nullptr;
  }
  KMX_DWORD offset = sect->find(T::IDENT);
  if (!offset) {
    DebugLog("section_from_sect() - not found. section:");
    validate_section_name(T::IDENT);
    DebugLog("");
    return nullptr;
  }
  KMX_DWORD entrylength = sect->total - offset;
  return section_from_bytes<T>(rawbytes+offset, entrylength);
}

bool
COMP_KMXPLUS_HEADER::valid(KMX_DWORD length) const {
  DebugLog(": (%X) size 0x%X\n", ident, size);
  if (size < LDML_LENGTH_HEADER) {
    DebugLog("size < LDML_LENGTH_HEADER");
    return false;
  }
  if (size > length) {
    DebugLog("size > length");
    return false;
  }
  if (!validate_section_name(ident)) {
    DebugLog("invalid section name");
    return false;
  }
  DebugLog(" (header OK)"); // newline after section name
  return true;
}

bool
COMP_KMXPLUS_KEYS::valid(KMX_DWORD _kmn_unused(length)) const {
  DebugLog(" count: #0x%X\n", this->count);
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    return false;
  }
  for (KMX_DWORD i = 0; i<this->count; i++) {
    DebugLog(" #0x%d\n", i);
    const COMP_KMXPLUS_KEYS_ENTRY& entry = this->entries[i];
    DebugLog("  vkey\t0x%X\n", entry.vkey);
    DebugLog("  mod\t0x%X\n", entry.mod);
    DebugLog("  flags\t0x%X\n", entry.flags);
    if (entry.flags & LDML_KEYS_FLAGS_EXTEND) {
        DebugLog("  \t Extend: String #0x%X\n", entry.to);
    } else {
        DebugLog("  \t UTF-32:U+%04X\n", entry.to);
    }
  }
  return true;
}

std::u16string
COMP_KMXPLUS_KEYS_ENTRY::get_string() const {
  assert(!(flags & LDML_KEYS_FLAGS_EXTEND)); // should not be called.
  char16_single buf;
  const int len = Utf32CharToUtf16(to, buf);
  return std::u16string(buf.ch, len);
}

bool
COMP_KMXPLUS_LOCA::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    return false;
  }
  // TODO-LDML
  for(KMX_DWORD i=0; i<this->count; i++) {
    DebugLog(" Locale #%d: #0x%X\n", i, this->entries[i].locale);
  }
  return true;
}

bool
COMP_KMXPLUS_META::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)) {
    DebugLog("header.size < expected size");
    return false;
  }
  DebugLog(" author:\t#0x%X\n", this->author);
  DebugLog(" conform:\t#0x%X\n", this->conform);
  DebugLog(" layout:\t#0x%X\n", this->layout);
  DebugLog(" normalization:\t#0x%X\n", this->normalization);
  DebugLog(" indicator:\t#0x%X\n", this->indicator);
  DebugLog(" settings:\t0x%X\n", this->settings);
  return true;
}

bool
COMP_KMXPLUS_VKEY::valid(KMX_DWORD _kmn_unused(length)) const {
  DebugLog("vkey: count 0x%X\n", this->count);
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    return false;
  }
  // TODO-LDML
  DebugLog("TODO: dump vkey");
  return true;
}

bool
COMP_KMXPLUS_STRS::valid(KMX_DWORD _kmn_unused(length)) const {
  DebugLog("strs: count 0x%X\n", this->count);
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    return false;
  }
  for (KMX_DWORD i=0; i<this->count; i++) {
    KMX_DWORD offset = entries[i].offset;
    KMX_DWORD length = entries[i].length;
    if(offset+((length+1)*2) > header.size) {
      DebugLog("#0x%X: expected end of string past header.size", i);
      return false;
    }
    const uint8_t* thisptr = reinterpret_cast<const uint8_t*>(this);
    const KMX_WCHAR* start = reinterpret_cast<const KMX_WCHAR*>(thisptr+offset);
    if(start[length] != 0) {
      DebugLog("#0x%X: String not null terminated", i);
      return false;
    }
    // TODO-LDML: validate valid UTF-16LE?
    DebugLog("#0x%X: '%s'", i, Debug_UnicodeString(start, length));
  }
  return true;
}

bool
COMP_KMXPLUS_SECT::valid(KMX_DWORD length) const {
  DebugLog("sect: total 0x%X\n", this->total);
  DebugLog("sect: count 0x%X\n", this->count);
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    DebugLog("header.size < expected size");
    return false;
  }

  // now validate each component
  bool overall_valid = true;
  for (KMX_DWORD i = 0; i < this->count; i++) {
    const COMP_KMXPLUS_SECT_ENTRY& entry = this->entries[i];
    DebugLog("#%d: ", i);
    if(!validate_section_name(entry.sect)) {
      DebugLog(" (invalid section name) ");
      return false;
    }
    DebugLog(" %X @ %X\n", entry.sect, entry.offset);
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
  if (firstEntry.length != 0 ) {
    // per spec, the first elem should have zero length and zero offset
    DebugLog("ERROR: elem[0].length 0x%x but should be zero", firstEntry.length);
    return false;
  }
  if (firstEntry.offset + sizeof(COMP_KMXPLUS_ELEM_ELEMENT) > header.size) {
    // TODO-LDML: change to  (firstEntry.offset != 0 )
    // Blocked by https://github.com/keymanapp/keyman/issues/7404
    DebugLog("ERROR: elem[0].offset 0x%x would put element outside of data region");
    return false;
  }
  for (KMX_DWORD e = 1; e < count; e++) {
    // Don't need to recheck the first entry here.
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
    return nullptr;
  }
  const COMP_KMXPLUS_ELEM_ENTRY &entry = entries[elementNumber];
  length = entry.length;
  if (entry.offset + (entry.length * sizeof(COMP_KMXPLUS_ELEM_ELEMENT)) > header.size) {
    DebugLog("ERROR: !! COMP_KMXPLUS_ELEM::getElementList");
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
    return false;
  }
  // TODO-LDML
  DebugLog("!! More to do here.");
  return true;
}

// ---- constructor

kmx_plus::kmx_plus(const COMP_KEYBOARD *keyboard, size_t length)
    : keys(nullptr), loca(nullptr), meta(nullptr),
      sect(nullptr), strs(nullptr), vkey(nullptr), valid(false) {

  DebugLog("kmx_plus(): Got a COMP_KEYBOARD at %p\n", keyboard);
  if (!(keyboard->dwFlags & KF_KMXPLUS)) {
    DebugLog("Err: flags COMP_KEYBOARD.dwFlags did not have KF_KMXPLUS set");
    valid = false;
    return;
  }
  const COMP_KEYBOARD_EX* ex = reinterpret_cast<const COMP_KEYBOARD_EX*>(keyboard);

  DebugLog("kmx_plus(): KMXPlus offset 0x%X, KMXPlus size 0x%X\n", ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize);
  if (ex->kmxplus.dpKMXPlus < sizeof(kmx::COMP_KEYBOARD_EX)) {
    DebugLog("dwKMXPlus is not past the end of COMP_KEYBOARD_EX");
    valid = false;
    return;
  }
  if ( ex->kmxplus.dpKMXPlus + ex->kmxplus.dwKMXPlusSize > length) {
    DebugLog("dpKMXPlus + dwKMXPlusSize is past the end of the file");
    valid = false;
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
    elem = section_from_sect<COMP_KMXPLUS_ELEM>(sect);
    keys = section_from_sect<COMP_KMXPLUS_KEYS>(sect);
    loca = section_from_sect<COMP_KMXPLUS_LOCA>(sect);
    meta = section_from_sect<COMP_KMXPLUS_META>(sect);
    strs = section_from_sect<COMP_KMXPLUS_STRS>(sect);
    tran = section_from_sect<COMP_KMXPLUS_TRAN>(sect);
    vkey = section_from_sect<COMP_KMXPLUS_VKEY>(sect);
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

}  // namespace kmx
}  // namespace kbp
}  // namespace km
