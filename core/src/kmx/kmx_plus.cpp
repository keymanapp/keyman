/*
  Copyright:        Copyright (C) 2022 SIL International.
  Authors:          srl295
  Implementation for the KMX Plus utilities
*/

#include <km_types.h>
#include <kmx_file.h>
#include <kmx/kmx_plus.h>
#include <kmx/kmx_xstring.h>

#if KMXPLUS_DEBUG
#include <iostream>
#include <stdio.h>
#endif

#include <assert.h>

namespace km {
namespace kbp {
namespace kmx {

#if KMXPLUS_DEBUG
#define debug_println(x) std::cerr << __FILE__ << ":" << __LINE__ << ": " << (x) << std::endl;
#define KMXPLUS_PRINTF(x) printf x
#else
#define debug_println(x)
#define KMXPLUS_PRINTF(x)
#endif

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
      KMXPLUS_PRINTF(("\\x%02X", ch));
    } else {
      KMXPLUS_PRINTF(("%c", ch));
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
    debug_println("!data");
    return nullptr;
  }
  if (length < LDML_LENGTH_HEADER) {
    debug_println("length < LDML_LENGTH_HEADER");
  }
  const COMP_KMXPLUS_HEADER *all = reinterpret_cast<const COMP_KMXPLUS_HEADER *>(data);
  if (!all->valid(length)) {
    debug_println("header failed validation");
    return nullptr;
  }
  if (all->ident != ident) {
    debug_println("header had wrong section id");
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
    debug_println("length < sizeof(section)");
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
  const uint8_t *rawbytes = reinterpret_cast<const uint8_t *>(sect);
  if (rawbytes == nullptr) {
    debug_println("section_from_sect(nullptr) == nullptr");
    return nullptr;
  }
  KMX_DWORD offset = sect->find(T::IDENT);
  if (!offset) {
    debug_println("section_from_sect() - not found");
    return nullptr;
  }
  KMX_DWORD entrylength = sect->total - offset;
  return section_from_bytes<T>(rawbytes+offset, entrylength);
}

bool
COMP_KMXPLUS_HEADER::valid(KMX_DWORD length) const {
  KMXPLUS_PRINTF((": (%X) size 0x%X\n", ident, size));
  if (size < LDML_LENGTH_HEADER) {
    debug_println("size < LDML_LENGTH_HEADER");
    return false;
  }
  if (size > length) {
    debug_println("size > length");
    return false;
  }
  if (!validate_section_name(ident)) {
    debug_println("invalid section name");
    return false;
  }
  return true;
}

bool
COMP_KMXPLUS_KEYS::valid(KMX_DWORD _kmn_unused(length)) const {
  KMXPLUS_PRINTF((" count: #0x%X\n", this->count));
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    debug_println("header.size < expected size");
    return false;
  }
  for (KMX_DWORD i = 0; i<this->count; i++) {
    KMXPLUS_PRINTF((" #0x%d\n", i));
    const COMP_KMXPLUS_KEYS_ENTRY& entry = this->entries[i];
    KMXPLUS_PRINTF(("  vkey\t0x%X\n", entry.vkey));
    KMXPLUS_PRINTF(("  mod\t0x%X\n", entry.mod));
    KMXPLUS_PRINTF(("  flags\t0x%X\n", entry.flags));
    if (entry.flags & LDML_KEYS_FLAGS_EXTEND) {
        KMXPLUS_PRINTF(("  \t Extend: String #0x%X\n", entry.to));
    } else {
        KMXPLUS_PRINTF(("  \t UTF-32:U+%04X\n", entry.to));
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
    debug_println("header.size < expected size");
    return false;
  }
  // TODO-LDML
  for(KMX_DWORD i=0; i<this->count; i++) {
    KMXPLUS_PRINTF((" Locale #%d: #0x%X\n", i, this->entries[i].locale));
  }
  return true;
}

bool
COMP_KMXPLUS_META::valid(KMX_DWORD _kmn_unused(length)) const {
  if (header.size < sizeof(*this)) {
    debug_println("header.size < expected size");
    return false;
  }
  KMXPLUS_PRINTF((" author:\t#0x%X\n", this->author));
  KMXPLUS_PRINTF((" conform:\t#0x%X\n", this->conform));
  KMXPLUS_PRINTF((" layout:\t#0x%X\n", this->layout));
  KMXPLUS_PRINTF((" normalization:\t#0x%X\n", this->normalization));
  KMXPLUS_PRINTF((" indicator:\t#0x%X\n", this->indicator));
  KMXPLUS_PRINTF((" settings:\t0x%X\n", this->settings));
  return true;
}

bool
COMP_KMXPLUS_VKEY::valid(KMX_DWORD _kmn_unused(length)) const {
  KMXPLUS_PRINTF(("vkey: count 0x%X\n", this->count));
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    debug_println("header.size < expected size");
    return false;
  }
  // TODO-LDML
  KMXPLUS_PRINTF(("TODO: dump vkey"));
  return true;
}

bool
COMP_KMXPLUS_STRS::valid(KMX_DWORD _kmn_unused(length)) const {
  KMXPLUS_PRINTF(("strs: count 0x%X\n", this->count));
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    debug_println("header.size < expected size");
    return false;
  }
  for (KMX_DWORD i=0; i<this->count; i++) {
    KMXPLUS_PRINTF(("#0x%X: ", i));
    KMX_DWORD offset = entries[i].offset;
    KMX_DWORD length = entries[i].length;
    if(offset+((length+1)*2) > header.size) {
      debug_println("expected end of string past header.size");
      return false;
    }
    const uint8_t* thisptr = reinterpret_cast<const uint8_t*>(this);
    const KMX_WCHAR* start = reinterpret_cast<const KMX_WCHAR*>(thisptr+offset);
    if(start[length] != 0) {
      debug_println("String not null terminated");
      return false;
    }
    // TODO-LDML: validate valid UTF-16LE?
#if KMXPLUS_DEBUG
    // first print it escaped
    for(KMX_DWORD j=0; start[j] && j<length; j++) {
        if (start[j] < 0x7F && start[j] > 0x20) {
            KMXPLUS_PRINTF(("%c", start[j]));
        } else {
            KMXPLUS_PRINTF((" U+%04X ", start[j]));
        }
    }
    KMXPLUS_PRINTF(("\n"));

    // // now print it as a string
    // TODO-LDML
    // std::u16string str = get(i);
    // std::cerr << str << std::endl;
#endif
  }
  return true;
}

bool
COMP_KMXPLUS_SECT::valid(KMX_DWORD length) const {
  KMXPLUS_PRINTF(("sect: total 0x%X\n", this->total));
  KMXPLUS_PRINTF(("sect: count 0x%X\n", this->count));
  if (header.size < sizeof(*this)+(sizeof(entries[0])*count)) {
    debug_println("header.size < expected size");
    return false;
  }

  // now validate each component
  bool overall_valid = true;
  for (KMX_DWORD i = 0; i < this->count; i++) {
    const COMP_KMXPLUS_SECT_ENTRY& entry = this->entries[i];
    if(!validate_section_name(entry.sect)) {
      KMXPLUS_PRINTF((" (invalid section name) \n"));
      return false;
    }
    KMXPLUS_PRINTF((" sect#%d: %X @ %X\n", i, entry.sect, entry.offset));
    if (entry.sect == LDML_SECTIONID_SECT) {
      debug_println("Invalid nested 'sect'");
      overall_valid = false;
      continue;
    }
    const uint8_t* data = reinterpret_cast<const uint8_t *>(this);
    const uint8_t* entrydata = data + entry.offset;
    KMX_DWORD entrylength = length - entry.offset;
    // just validate header
    if(header_from_bytes(entrydata, entrylength, entry.sect) == nullptr) {
      KMXPLUS_PRINTF(("Invalid header %X", entry.sect));
      overall_valid = false;
      continue;
    }
  }
  return overall_valid;
}

kmx_plus::kmx_plus(const COMP_KEYBOARD *keyboard, size_t length)
    : keys(nullptr), loca(nullptr), meta(nullptr),
      sect(nullptr), strs(nullptr), vkey(nullptr), valid(false) {

  KMXPLUS_PRINTF(("kmx_plus(): Got a COMP_KEYBOARD at %p\n", keyboard));
  if (!(keyboard->dwFlags & KF_KMXPLUS)) {
    KMXPLUS_PRINTF(("Err: flags COMP_KEYBOARD.dwFlags did not have KF_KMXPLUS set\n"));
    valid = false;
    return;
  }
  const COMP_KEYBOARD_EX* ex = reinterpret_cast<const COMP_KEYBOARD_EX*>(keyboard);

  KMXPLUS_PRINTF(("kmx_plus(): KMXPlus offset 0x%X, KMXPlus size 0x%X\n", ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize));
  if (ex->kmxplus.dpKMXPlus < sizeof(kmx::COMP_KEYBOARD_EX)) {
    debug_println("dwKMXPlus is not past the end of COMP_KEYBOARD_EX");
    valid = false;
    return;
  }
  if ( ex->kmxplus.dpKMXPlus + ex->kmxplus.dwKMXPlusSize > length) {
    debug_println("dpKMXPlus + dwKMXPlusSize is past the end of the file");
    valid = false;
    return;
  }

  const uint8_t* rawdata = reinterpret_cast<const uint8_t*>(keyboard);

  sect = section_from_bytes<COMP_KMXPLUS_SECT>(rawdata+ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize);
  if (sect == nullptr) {
    KMXPLUS_PRINTF(("kmx_plus(): 'sect' did not validate\n"));
    valid = false;
  } else {
    valid = true;
    // load other sections, validating as we go
    // these will be nullptr if they don't validate
    keys = section_from_sect<COMP_KMXPLUS_KEYS>(sect);
    loca = section_from_sect<COMP_KMXPLUS_LOCA>(sect);
    meta = section_from_sect<COMP_KMXPLUS_META>(sect);
    strs = section_from_sect<COMP_KMXPLUS_STRS>(sect);
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
