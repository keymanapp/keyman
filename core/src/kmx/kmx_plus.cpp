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
#include "ldml/keyman_core_ldml.h"

#include <assert.h>
#include "kmx_plus.h"

namespace km {
namespace core {
namespace kmx {

/**
 * \def KMXPLUS_DEBUG_LOAD set to 1 to print messages on KMXPLUS loading.
 * Off by default.
*/
#ifndef KMXPLUS_DEBUG_LOAD
#define KMXPLUS_DEBUG_LOAD 0
#endif

#if KMXPLUS_DEBUG_LOAD
#define DebugLoad(msg,...) DebugLog(msg, __VA_ARGS__)
#else
#define DebugLoad(msg,...)
#endif

// double check these modifier mappings
static_assert(LCTRLFLAG == LDML_KEYS_MOD_CTRLL, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(RCTRLFLAG == LDML_KEYS_MOD_CTRLR, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(K_CTRLFLAG == LDML_KEYS_MOD_CTRL, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(RALTFLAG == LDML_KEYS_MOD_ALTR, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(LALTFLAG == LDML_KEYS_MOD_ALTL, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(K_ALTFLAG == LDML_KEYS_MOD_ALT, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(CAPITALFLAG == LDML_KEYS_MOD_CAPS, "LDML modifier bitfield vs. kmx_file.h #define mismatch");
static_assert(K_SHIFTFLAG == LDML_KEYS_MOD_SHIFT, "LDML modifier bitfield vs. kmx_file.h #define mismatch"); // "either" shift
// LDML_KEYS_MOD_OTHER is not present in kmx_file.h (>16 bit)

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
 * @brief fill a COMP_KMXPLUS_HEADER from bytes
 *
 * @param data
 * @param length
 * @param ident
 * @param out
 * @return true on succcess
 */
bool
header_from_bytes(const uint8_t *data, KMX_DWORD length, KMX_DWORD fileVersion, uint32_t ident, kmx::COMP_KMXPLUS_HEADER &out) {
  if (!data) {
    DebugLog("!data");
    assert(false);
    return false;
  }
  if(fileVersion == LDML_KMXPLUS_VERSION_17) {
    const COMP_KMXPLUS_HEADER_17 *all = reinterpret_cast<const COMP_KMXPLUS_HEADER_17 *>(data);
    if (!all->valid(length)) {
      DebugLog("header failed validation");
      assert(false);
      return false;
    }

    if (all->ident != ident) {
      DebugLog("header had wrong section id");
      assert(false);
      return false;
    }

    out.set(LDML_KMXPLUS_VERSION_17, all->ident, all->size);
  } else {
    const COMP_KMXPLUS_HEADER_19 *all = reinterpret_cast<const COMP_KMXPLUS_HEADER_19 *>(data);
    if (!all->valid(length)) {
      DebugLog("header failed validation");
      assert(false);
      return false;
    }

    if(ident == LDML_SECTIONID_SECT) {
      if(all->ident != LDML_SECTIONID_SEC2) {
        DebugLog("header had SECT but expected SEC2");
        assert(false);
        return false;
      }
    } else if (all->ident != ident) {
      DebugLog("header had wrong section id");
      assert(false);
      return false;
    }

    out.set(LDML_KMXPLUS_VERSION_19, all->ident, all->size, all->version);
  }

  return true;
}

/**
 * @brief Determines the overall version of the file based on the first four bytes:
 *          'sect' = v17 file
 *          'sec2' = v19+ file
 *          other = invalid file
 * A v19+ file will include an additional version field for each section.
 * @param data
 * @param length
 * @return KMX_DWORD  a LDML_KMXPLUS_VERSION_ value, or 0 on error
 */
KMX_DWORD determine_file_version_from_bytes(const uint8_t* data, KMX_DWORD length) {
  if(length < sizeof(COMP_KMXPLUS_HEADER_17)) {
    DebugLog("data is too short");
    assert(false);
    return 0;
  }
  COMP_KMXPLUS_HEADER_17 const* header = reinterpret_cast<COMP_KMXPLUS_HEADER_17 const *>(data);
  if(header->ident == COMP_KMXPLUS_SECT::IDENT) {
    return LDML_KMXPLUS_VERSION_17;
  }
  if(header->ident == COMP_KMXPLUS_SECT::IDENT_V19) {
    return LDML_KMXPLUS_VERSION_19;
  }
  DebugLog("initial section is unrecognized");
  assert(false);
  return 0;
}
/**
 * @brief Accessor for a section based on bytes
 *
 * @tparam T
 * @param data input pointer to the section to be validated. If nullptr, 'sect' will be nullptr, but will return true (valid).
 * @param fileLength length of input data. This function will not read past the end of this length.
 * @param out on exit, will be set to the new section. On any failure, null data, or invalidity, will be nullptr.
 * @return true if section is missing (nullptr) or valid
 * @return const T*
 */
template <class T> bool section_from_bytes(const uint8_t* data, KMX_DWORD fileLength, KMX_DWORD fileVersion, COMP_KMXPLUS_HEADER& header, const T*& out) {
  out = nullptr; // null unless valid.
  if (data == nullptr) {
    // missing section - not an error.
    DebugLog("data was null (missing section)");
    return true;
  } else if (fileLength < sizeof(T)) { // Does not include dynamic data. First check.
    DebugLog("fileLength < sizeof(section)");
    assert(false);
    return false;
  }

  // verify the kmx+ section header
  if(!header_from_bytes(data, fileLength, fileVersion, T::IDENT, header)) {
    // asserted and logged in header_from_bytes
    return false;
  }

  // subtract the size of the header so validation will not be affected by the
  // change of header size in LDML_KMXPLUS_VERSION_19
  header.size -= header.headerSize();

  // now it is safe to cast this to a T
  const T *section = reinterpret_cast<const T *>(data + header.headerSize());
  if (section == nullptr) {
    // should not happen.
    DebugLog("reinterpret_cast<> failed.");
    assert(false);
    return false;
  } else if (!section->valid(header, fileLength)) {
    return false; // validation failed.
  } else {
    out = section;
    return true;
  }
}

/**
 * @brief This is a special override just for COMP_KMXPLUS_BKSP, which is identical to
 *        COMP_KMXPLUS_TRAN, except for ident. So we leverage TRAN functionality to
 *        process, but in order to keep the distinction bubbling too high, this is the
 *        only place we differentiate.
 * @param sect this is the pointer to the 'sect' section table. Should not be null!
 * @param out on exit, will be set to the requested section, or nullptr if missing or invalid
 * @returns true if section is valid or missing, false only if invalid
 */
bool get_section_from_sect(const COMP_KMXPLUS_SECT* sect, const COMP_KMXPLUS_HEADER& sectHeader, COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_TRAN>* helper, const COMP_KMXPLUS_BKSP*& out) {
  out = nullptr;

  KMX_DWORD entryLength;
  const uint8_t *rawbytes = sect->get(sectHeader, COMP_KMXPLUS_BKSP::IDENT, entryLength);
  if (rawbytes == nullptr)  {
    // just missing, not invalid
    return true;
  }

  if(!section_from_bytes<COMP_KMXPLUS_BKSP>(rawbytes, entryLength, sectHeader.fileVersion(), helper->header, out)) {
    return false;
  }

  if(!out) {
    assert(false);
    DebugLog("unexpected out == nullptr");
    return false;
  }

  if(!helper->set(out)) {
    return false;
  }

  return true;
}

/**
 * @param sect this is the pointer to the 'sect' section table. Should not be null!
 * @param out on exit, will be set to the requested section, or nullptr if missing or invalid
 * @returns true if section is valid or missing, false only if invalid
 */
template <class T>
bool get_section_from_sect(const COMP_KMXPLUS_SECT* sect, const COMP_KMXPLUS_HEADER& sectHeader, COMP_KMXPLUS_Section_Helper<T>* helper, const T*& out) {
  out = nullptr;

  KMX_DWORD entryLength;
  const uint8_t *rawbytes = sect->get(sectHeader, T::IDENT, entryLength);
  if (rawbytes == nullptr)  {
    // just missing, not invalid
    return true;
  }

  if(!section_from_bytes<T>(rawbytes, entryLength, sectHeader.fileVersion(), helper->header, out)) {
    return false;
  }

  if(!out) {
    DebugLog("unexpected out == nullptr");
    assert(false);
    return false;
  }

  if(!helper->set(out)) {
    return false;
  }

  return true;
}

inline bool is_block_valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD offset, KMX_DWORD size) {
  if(offset < header.headerSize()) {
    DebugLog("[%x] offset(%d) < header.headerSize(%d)", header.ident, offset, header.headerSize());
    assert(false);
    return false;
  }
  if(offset - header.headerSize() > header.size) {
    DebugLog("[%x] offset(%d) > header.size(%d)", header.ident, offset, header.size);
    assert(false);
    return false;
  }
  if(offset + size - header.headerSize() > header.size) {
    DebugLog("[%x] offset(%d) + size(%d) > header.size(%d)", header.ident, offset, size, header.size);
    assert(false);
    return false;
  }
  return true;
}

/**
 * @brief Get file data at byte offset; cannot retrieve the initial SECT/SEC2 header
 *
 * @tparam U          type of target data
 * @param base        start of SECT data, i.e. immediately after SECT header
 * @param fileOffset  offset in bytes from start of file
 * @param fileLength
 * @return U*
 */
template<typename U>
const U* get_file_data_at_offset(const COMP_KMXPLUS_SECT *base, COMP_KMXPLUS_HEADER const &header, KMX_DWORD fileOffset, KMX_DWORD fileLength) {
  if(fileOffset < header.headerSize()) {
    // This is retrieving data based on the SECT/SEC2 table base, which points
    // at the first byte after the SECT/SEC2 header.
    DebugLog("Attempted to retrieve SECT/SEC2 header, which is not allowed here");
    assert(false);
    return nullptr;
  }
  if(fileOffset >= fileLength) {
    DebugLog("Attempted to retrieve data outside file boundaries");
    assert(false);
    return nullptr;
  }
  fileOffset -= header.headerSize();
  const uint8_t* thisptr = reinterpret_cast<const uint8_t*>(base);
  return reinterpret_cast<const U *>(thisptr+fileOffset);
}

/**
 * @brief Get the section data at offset, casting to desired type
 *
 * @tparam T      struct type of the section
 * @tparam U      struct type of data to return
 * @param base    start of the section data, i.e. immediately after section
 *                header
 * @param header  header data for the section
 * @param offset  offset in bytes from the start of the section data
 * @param size    size of the data to return, for validation
 * @return        pointer to data, type U
 */
template<class T, typename U>
const U* get_section_data_at_offset(const T *base, COMP_KMXPLUS_HEADER const &header, KMX_DWORD offset, KMX_DWORD size) {
  if(!is_block_valid(header, offset, size)) {
    return nullptr;
  }

  offset -= header.headerSize();
  const uint8_t* thisptr = reinterpret_cast<const uint8_t*>(base);
  const U* start = reinterpret_cast<const U*>(thisptr+offset);
  return start;
}

/**
 * @brief Get the section data at offset, casting to desired type, verify that
 * the section is long enough for count * data, and update the offset to point
 * to the next byte after the data. Allows zero-length, optional data.
 *
 * @tparam T      struct type of the section
 * @tparam U      struct type of data to return
 * @param base    start of the section data, i.e. immediately after section
 *                header
 * @param header  header data for the section
 * @param count   number of U items expected
 * @param offset  (in, out) offset in bytes from the start of the section data,
 *                updated on return to next byte after data
 * @param out     (out) pointer to start of data
 * @return bool   false on error
 */
template<class T, typename U>
bool get_optional_section_data_at_offset_and_increment(const T *base, COMP_KMXPLUS_HEADER const &header,
  KMX_DWORD count, KMX_DWORD& offset, const U*& out
) {
  out = nullptr;

  if(count == 0) {
    return true;
  }

  KMX_DWORD size = count * sizeof(U);

  out = get_section_data_at_offset<T, U>(base, header, offset, size);

  if(out == nullptr) {
    return false;
  }

  offset += size;

  return true;
}

/**
 * @brief Get the section data at offset, casting to desired type, verify that
 * the section is long enough for count * data, and update the offset to point
 * to the next byte after the data. Does not allow zero-length, optional data.
 *
 * @tparam T      struct type of the section
 * @tparam U      struct type of data to return
 * @param base    start of the section data, i.e. immediately after section
 *                header
 * @param header  header data for the section
 * @param count   number of U items expected
 * @param offset  (in, out) offset in bytes from the start of the section data,
 *                updated on return to next byte after data
 * @param out     (out) pointer to start of data
 * @return bool   false on error or missing data
 */
template<class T, typename U>
bool get_required_section_data_at_offset_and_increment(const T *base, COMP_KMXPLUS_HEADER const &header,
  KMX_DWORD count, KMX_DWORD& offset, const U*& out
) {
  if(count == 0) {
    out = nullptr;
    return false;
  }

  return get_optional_section_data_at_offset_and_increment<T,U>(base, header, count, offset, out);
}

bool
COMP_KMXPLUS_HEADER_17::valid(KMX_DWORD length) const {
  DebugLog("%c%c%c%c: (%X) size 0x%X\n", DEBUG_IDENT(ident), ident, size);
  if (size < LDML_LENGTH_HEADER_17) {
    DebugLog("size %d < LDML_LENGTH_HEADER_17 %d", size, LDML_LENGTH_HEADER_17);
    assert(false);
    return false;
  }
  if (size > length) {
    DebugLog("size %d > length %d", size, length);
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
COMP_KMXPLUS_HEADER_19::valid(KMX_DWORD length) const {
  DebugLog("%c%c%c%c: (%X) size 0x%X\n", DEBUG_IDENT(ident), ident, size);
  if (size < LDML_LENGTH_HEADER_19) {
    DebugLog("size %d < LDML_LENGTH_HEADER %d", size, LDML_LENGTH_HEADER_19);
    assert(false);
    return false;
  }
  if (size > length) {
    DebugLog("size %d > length %d", size, length);
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
COMP_KMXPLUS_LOCA::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(!is_block_valid(header, header.headerSize(), sizeof(*this)+(sizeof(entries[0])*count))) {
    return false;
  }
  for(KMX_DWORD i=0; i<count; i++) {
    DebugLog(" Locale #%d: #0x%X\n", i, entries[i].locale);
  }
  return true;
}

bool
COMP_KMXPLUS_META::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(!is_block_valid(header, header.headerSize(), sizeof(*this))) {
    return false;
  }
  DebugLog(" author:\t#0x%X\n", author);
  DebugLog(" conform:\t#0x%X\n", conform);
  DebugLog(" layout:\t#0x%X\n", layout);
  DebugLog(" name:\t#0x%X\n", name);
  DebugLog(" indicator:\t#0x%X\n", indicator);
  DebugLog(" settings:\t0x%X\n", settings);
  return true;
}

bool
COMP_KMXPLUS_DISP::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const {
  if(header.version == LDML_KMXPLUS_VERSION_17) {
    return valid_17(header, length);
  }

  if(header.version == LDML_KMXPLUS_VERSION_19) {
    return valid_19(header, length);
  }

  assert(false);
  return false;
}

bool
COMP_KMXPLUS_DISP::valid_19(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(header.version != LDML_KMXPLUS_VERSION_19) {
    assert(false);
    return false;
  }

  DebugLog("disp: count 0x%X\n", count);

  if (!is_block_valid(header, header.headerSize(), sizeof(*this)+(sizeof(entries[0])*count))) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }

  if (baseCharacter != 0) {
    DebugLog("disp: baseCharacter str#0x%X", baseCharacter);
  }
  for (KMX_DWORD i=0; i<count; i++) {
    DebugLoad("disp#%d: toId: str0x%X -> str0x%X, flags: 0x%x", i, entries[i].toId, entries[i].display, entries[i].flags);
    if (entries[i].toId == 0 || entries[i].display == 0) {
      DebugLog("disp must have output/keyId, and must have display");
      assert(false);
      return false;
    }
    //TODO-EMBED-OSK-IN-KMX: validate flags
  }
  return true;
}

bool
COMP_KMXPLUS_DISP::valid_17(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(header.version != LDML_KMXPLUS_VERSION_17) {
    assert(false);
    return false;
  }

  DebugLog("disp: count 0x%X\n", count);

  if (!is_block_valid(header, header.headerSize(), sizeof(*this)+(sizeof(entries[0])*count))) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }

  if (baseCharacter != 0) {
    DebugLog("disp: baseCharacter str#0x%X", baseCharacter);
  }

  const COMP_KMXPLUS_DISP_ENTRY_17 *entries17 = reinterpret_cast<const COMP_KMXPLUS_DISP_ENTRY_17 *>(&entries[0]);
  for (KMX_DWORD i=0; i<count; i++) {
    DebugLoad("disp#%d: id: str0x%X to: str0x%X -> str0x%X", i, entries17[i].id, entries17[i].to, entries17[i].display);
    if ((entries17[i].to == 0 && entries17[i].id == 0) || entries17[i].display == 0) {
      DebugLog("disp must have either keyId/output, and must have display");
      assert(false);
      return false;
    }
  }
  return true;
}

bool
COMP_KMXPLUS_DISP_Helper::set(const COMP_KMXPLUS_DISP *newDisp) {
  if(header.version != LDML_KMXPLUS_VERSION_17 && header.version != LDML_KMXPLUS_VERSION_19) {
    return false;
  }

  // TODO-EMBED-OSK-IN-KMX - more transforms required for in-memory representation of OSK; convert v17 to v19 in memory

  if(!COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_DISP>::set(newDisp)) {
    return false;
  }

  return true;
}

bool
COMP_KMXPLUS_STRS::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  DebugLog("strs: count 0x%X\n", count);

  if(!is_block_valid(header, header.headerSize(), sizeof(*this)+(sizeof(entries[0])*count))) {
    return false;
  }

  for (KMX_DWORD i=0; i<count; i++) {
    const KMX_DWORD offset = entries[i].offset;
    const KMX_DWORD length = entries[i].length;
    const KMX_WCHAR* start = get_section_data_at_offset<COMP_KMXPLUS_STRS, KMX_WCHAR>(this, header, offset, (length+1)*sizeof(KMX_WCHAR));
    if(!start) {
      return false;
    }
    if(start[length] != 0) {
      DebugLog("#0x%X: String of length 0x%x not null terminated", i, length);
      assert(start[length] == 0);
      return false;
    }
    DebugLoad("strs #0x%X: '%s'", i, Debug_UnicodeString(start));
    if (!COMP_KMXPLUS_STRS::valid_string(start, length)) {
      DebugLog("#0x%X: String of length 0x%x invalid", i, length);
      return false;
    }
  }
  return true;
}

bool COMP_KMXPLUS_STRS::valid_string(const KMX_WCHAR* start, KMX_DWORD length) {
  for (KMX_DWORD n = 0; n < length; n++) {
    const auto& ch = start[n];
    if (Uni_IsSurrogate2(ch)) {
      DebugLog("String of length 0x%x @ 0x%x: Char U+%04X is a trailing (unpaired) surrogate", length, n, ch);
      return false;
    } else if (Uni_IsSurrogate1(ch)) {
      n++;
      if (n == length) {
        DebugLog("String of length 0x%x @ 0x%x: Char U+%04X ends with leading (unpaired) surrogate", length, n, ch);
        return false;
      }
      const auto& ch2 = start[n];
      if (!Uni_IsSurrogate2(ch2)) {
        DebugLog("String of length 0x%x @ 0x%x: Char U+%04X not a trailing surrogate", length, n, ch2);
        return false;
      }
      const km_core_usv ch32 = Uni_SurrogateToUTF32(ch, ch2);
      if (!Uni_IsValid(ch32)) {
        DebugLog("String of length 0x%x @ 0x%x: Char U+%04X is illegal char", length, n, ch32);
        return false;
      }
    } else if (ch == LDML_UC_SENTINEL) {
      n++;
      if (n == length) {
        DebugLog("String of length 0x%x @ 0x%x: Sentinel value U+%04X at end of string", length, n, ch);
        return false;
      }
      const auto& ch2 = start[n];
      if (ch2 != LDML_MARKER_CODE) {
        DebugLog("String of length 0x%x @ 0x%x: Sentinel value followed by U+%04X", length, n, ch2);
        return false;
      }
      n++;
      if (n == length) {
        DebugLog("String of length 0x%x @ 0x%x: Sentinel value  followed by U+%04X at end of string", length, n, ch);
        return false;
      }
      const auto& ch3 = start[n];
      if (!is_valid_marker(ch3)) {
        DebugLog("String of length 0x%x @ 0x%x: Sentinel value + CODE followed by invalid marker U+%04X", length, n, ch);
        return false;
      }
      // else OK (good marker)
    } else if(!Uni_IsValid(ch)) {
      DebugLog("String of length 0x%x @ 0x%x: Char U+%04X is illegal char", length, n, ch);
      return false;
    } // else OK (other char)
  }
  return true;
}

/**
 * helper for extracting single char values
 * @param v field with char type
 * @return value a string
 */
std::u16string COMP_KMXPLUS_STRS::str_from_char(KMX_DWORD v) {
  char16_single buf;
  const int len = Utf32CharToUtf16(v, buf);
  return std::u16string(buf.ch, len);
}


bool
COMP_KMXPLUS_SECT::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD fileLength) const {
  DebugLog("sect: total 0x%X\n", total);
  DebugLog("sect: count 0x%X\n", count);
  if(!is_block_valid(header, header.headerSize(), sizeof(*this)+(sizeof(entries[0])*count))) {
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

    const uint8_t* entrydata = get_file_data_at_offset<const uint8_t>(this, header, entry.offset, fileLength);
    KMX_DWORD entryMaxLength = fileLength - entry.offset;
    // just validate header
    COMP_KMXPLUS_HEADER localHeader;
    if(!header_from_bytes(entrydata, entryMaxLength, header.version, entry.sect, localHeader)) {
      DebugLog("Invalid header %X", entry.sect);
      assert(false);
      overall_valid = false;
      continue;
    }
  }
  return overall_valid;
}

KMX_DWORD COMP_KMXPLUS_SECT::find(KMX_DWORD ident) const {
  for (KMX_DWORD i = 0; i < count; i++) {
    if (ident == entries[i].sect) {
        return entries[i].offset;
    }
  }
  return 0;
}

const uint8_t *COMP_KMXPLUS_SECT::get(COMP_KMXPLUS_HEADER const& header, KMX_DWORD ident, KMX_DWORD &entryLength) const {
  entryLength = 0;
  // the section table is also the beginning of the file.
  // lookup the offset from the table
  KMX_DWORD offset = find(ident);
  if (!offset) {
    DebugLog("COMP_KMXPLUS_SECT::get() - not found. section %c%c%c%c (0x%X)", DEBUG_IDENT(ident), ident);
    return nullptr;
  }
  // return the potential length, based on table.
  // we approximate the entryLength here, to at least not run off the end of the file.
  // we could take the offset of the next section, as an improvement.
  entryLength = total - offset;
  // return the pointer to the raw bytes
  return get_file_data_at_offset<uint8_t>(this, header, offset, total);
}

// ---- transform related fcns
bool
COMP_KMXPLUS_ELEM::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(!is_block_valid(header, header.headerSize(), sizeof(*this)+(sizeof(entries[0])*count))) {
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
    if (getElementList(header, e, listLength) == nullptr) {
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
COMP_KMXPLUS_ELEM_Helper::getElementList(KMX_DWORD elementNumber, KMX_DWORD &length) const {
  assert(data());
  if(!data()) return nullptr;
  return data()->getElementList(header, elementNumber, length);
}

const COMP_KMXPLUS_ELEM_ELEMENT *
COMP_KMXPLUS_ELEM::getElementList(const COMP_KMXPLUS_HEADER &header, KMX_DWORD elementNumber, KMX_DWORD &length) const {
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
  if (entry.offset - header.headerSize() + (entry.length * sizeof(COMP_KMXPLUS_ELEM_ELEMENT)) > header.size) {
    DebugLog("ERROR: !! COMP_KMXPLUS_ELEM::getElementList(%d) would be off end of data area", elementNumber);
    assert(false);
    return nullptr;
  }

  // pointer to specified entry
  return get_section_data_at_offset<COMP_KMXPLUS_ELEM, COMP_KMXPLUS_ELEM_ELEMENT>(this, header, entry.offset,
    entry.length * sizeof(COMP_KMXPLUS_ELEM_ELEMENT));
}


std::u16string
COMP_KMXPLUS_ELEM_ELEMENT::get_element_string() const {
  assert(type() == LDML_ELEM_FLAGS_TYPE_CHAR); // should only be called on char
  return COMP_KMXPLUS_STRS::str_from_char(element);
}

std::deque<std::u32string>
COMP_KMXPLUS_ELEM_ELEMENT::loadAsStringList(KMX_DWORD length, const COMP_KMXPLUS_STRS_Helper &strs) const {
  std::deque<std::u32string> list;
  for (KMX_DWORD i = 0; i<length; i++) {
    const auto &o = this[i];
    std::u32string str;
    if (o.type() == LDML_ELEM_FLAGS_TYPE_STR) {
      // fetch the string
      const auto str16 = strs.get(o.element);
      str = km::core::kmx::u16string_to_u32string(str16);
    } else {
      // single char
      str = std::u32string(1, (km_core_usv)o.element);
    }
    list.emplace_back(str);
  }
  return list;
}

KMX_DWORD
COMP_KMXPLUS_ELEM_ELEMENT::type() const {
  return (flags & LDML_ELEM_FLAGS_TYPE);
}

  // Note: shared with subclass COMP_KMXPLUS_BKSP
bool
COMP_KMXPLUS_TRAN::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(!is_block_valid(header, header.headerSize(),
    sizeof(*this) + (sizeof(COMP_KMXPLUS_TRAN_GROUP) * groupCount) +
                    (sizeof(COMP_KMXPLUS_TRAN_TRANSFORM) * transformCount) +
                    (sizeof(COMP_KMXPLUS_TRAN_REORDER) * reorderCount))) {
    return false;
  }
  return true;
}

COMP_KMXPLUS_TRAN_Helper::COMP_KMXPLUS_TRAN_Helper()
    : is_valid(false), groups(nullptr), transforms(nullptr), reorders(nullptr) {
}

bool COMP_KMXPLUS_TRAN_Helper::valid() const {
  return is_valid;
}

const COMP_KMXPLUS_TRAN_GROUP *
COMP_KMXPLUS_TRAN_Helper::getGroup(KMX_DWORD group) const {
  if (!valid() || group >= data()->groupCount) {
    assert(false);
    return nullptr;
  }
  return groups + group;
}

const COMP_KMXPLUS_TRAN_TRANSFORM *
COMP_KMXPLUS_TRAN_Helper::getTransform(KMX_DWORD transform) const {
  if (!valid() || transform >= data()->transformCount) {
    assert(false);
    return nullptr;
  }
  return transforms + transform;
}

const COMP_KMXPLUS_TRAN_REORDER *
COMP_KMXPLUS_TRAN_Helper::getReorder(KMX_DWORD reorder) const {
  if (!valid() || reorder >= data()->reorderCount) {
    assert(false);
    return nullptr;
  }
  return reorders + reorder;
}

bool
COMP_KMXPLUS_TRAN_Helper::set(const COMP_KMXPLUS_TRAN *newTran) {
  is_valid = false;
  groups = nullptr;
  transforms = nullptr;
  reorders = nullptr;

  if(!COMP_KMXPLUS_Section_Helper::set(newTran)) {
    return false;
  }
  is_valid = true;
  if (newTran == nullptr) {
    DebugLog("tran helper: missing, newTran=%p", newTran);
    // Note: kmx_plus::kmx_plus has already called section_from_bytes()
    // which validates this section's length. Will be nullptr here if invalid.
    // No assert here: just a missing section
    return true;
  }
  KMX_DWORD offset = this->header.calculateBaseSize(LDML_LENGTH_TRAN);

  // groups (required)
  is_valid = is_valid && get_required_section_data_at_offset_and_increment<COMP_KMXPLUS_TRAN, COMP_KMXPLUS_TRAN_GROUP>(
      data(), header, data()->groupCount, offset, groups);
  assert(is_valid);

  // transforms (optional)
  is_valid = is_valid && get_optional_section_data_at_offset_and_increment<COMP_KMXPLUS_TRAN, COMP_KMXPLUS_TRAN_TRANSFORM>(
      data(), header, data()->transformCount, offset, transforms);
  assert(is_valid);

  // reorders (optional)
  is_valid = is_valid && get_optional_section_data_at_offset_and_increment<COMP_KMXPLUS_TRAN, COMP_KMXPLUS_TRAN_REORDER>(
      data(), header, data()->reorderCount, offset, reorders);
  assert(is_valid);

  // Now, validate offsets by walking
  if (is_valid) {
    for(KMX_DWORD i = 0; is_valid && i < data()->groupCount; i++) {
      const COMP_KMXPLUS_TRAN_GROUP &group = groups[i];
      // is the count off the end?
      DebugLog(
          "<transformGroup> %d: type 0x%X, entries [%d..%d]", i, group.type, group.index,
          group.index + group.count - 1);
      if (group.type == LDML_TRAN_GROUP_TYPE_TRANSFORM) {
        DebugLog(" .. type=transform");
        if ((group.index >= data()->transformCount) || (group.index + group.count > data()->transformCount)) {
          DebugLog("COMP_KMXPLUS_TRAN_Helper: group[%d] would access transform %d+%d, > count %d",
              i, group.index, group.count, data()->transformCount);
          is_valid = false;
          assert(is_valid);
        }
        for(KMX_DWORD t = 0; is_valid && t < group.count; t++) {
          const auto &transform = transforms[group.index + t];
          if (transform.from == 0) {
            DebugLog("COMP_KMXPLUS_TRAN_Helper: transform [%d].[%d] has empty 'from' string", i, t);
            is_valid = false;
            assert(is_valid);
          } else if ((transform.mapFrom == 0) != (transform.mapTo == 0)) {
            DebugLog("COMP_KMXPLUS_TRAN_Helper: transform [%d].[%d] should have neither or both mapFrom=%d/mapTo=%d", i, t, transform.mapFrom, transform.mapTo);
            is_valid = false;
            assert(is_valid);
          }
        }
      } else if (group.type == LDML_TRAN_GROUP_TYPE_REORDER) {
        DebugLog(" .. type=reorder");
        if ((group.index >= data()->reorderCount) || (group.index + group.count > data()->reorderCount)) {
          DebugLog("COMP_KMXPLUS_TRAN_Helper: group[%d] would access reorder %d+%d, > count %d",
              i, group.index, group.count, data()->reorderCount);
          is_valid = false;
          assert(is_valid);
        }
        for(KMX_DWORD t = 0; is_valid && t < group.count; t++) {
          const auto &reorder = reorders[group.index + t];
          if (reorder.elements == 0) {
            DebugLog("COMP_KMXPLUS_TRAN_Helper: reorder [%d].[%d] has elements=0", i, t);
            // TODO-LDML: is this an error?
            // is_valid = false;
            // assert(is_valid);
          }
        }
      } else {
        DebugLog(" .. type=illegal 0x%X", group.type);
        is_valid = false;
        assert(is_valid);
      }
    }
  }
  // Return results
  DebugLog("COMP_KMXPLUS_TRAN_Helper.set(): %s", is_valid ? "valid" : "invalid");
  assert(is_valid);
  return is_valid;
}

bool
COMP_KMXPLUS_LAYR::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD length) const {
  if(header.version == LDML_KMXPLUS_VERSION_17) {
    return valid_17(header, length);
  }
  if(header.version == LDML_KMXPLUS_VERSION_19) {
    return valid_19(header, length);
  }
  assert(false);
  return false;
}

bool
COMP_KMXPLUS_LAYR::valid_19(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(header.version != LDML_KMXPLUS_VERSION_19) {
    assert(false);
    return false;
  }

  if(!is_block_valid(header, header.headerSize(),
    sizeof(*this)
      + (formCount  * sizeof(COMP_KMXPLUS_LAYR_FORM_V19))
      + (layerCount * sizeof(COMP_KMXPLUS_LAYR_ENTRY))
      + (rowCount   * sizeof(COMP_KMXPLUS_LAYR_ROW))
      + (keyCount   * sizeof(COMP_KMXPLUS_LAYR_KEY)))) {
    return false;
  }

  DebugLog("layr header is valid");
  // Note: We only do minimal validation here because of the
  // dynamic structure. See COMP_KMXPLUS_LAYR_Helper.set()  (below)
  // all remaining checks
  return true;
}

bool
COMP_KMXPLUS_LAYR::valid_17(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(header.version != LDML_KMXPLUS_VERSION_17) {
    assert(false);
    return false;
  }

  if(!is_block_valid(header, header.headerSize(),
    sizeof(*this)
      + (formCount  * sizeof(COMP_KMXPLUS_LAYR_FORM_V17))
      + (layerCount * sizeof(COMP_KMXPLUS_LAYR_ENTRY))
      + (rowCount   * sizeof(COMP_KMXPLUS_LAYR_ROW))
      + (keyCount   * sizeof(COMP_KMXPLUS_LAYR_KEY)))) {
    return false;
  }

  DebugLog("layr header is valid");
  // Note: We only do minimal validation here because of the
  // dynamic structure. See COMP_KMXPLUS_LAYR_Helper.set()  (below)
  // all remaining checks
  return true;
}

COMP_KMXPLUS_LAYR_Helper::COMP_KMXPLUS_LAYR_Helper() : is_valid(false), own_forms(false) {
}

bool
COMP_KMXPLUS_LAYR_Helper::set(const COMP_KMXPLUS_LAYR *newLayr) {
  is_valid = false;
  forms = nullptr;
  entries = nullptr;
  rows = nullptr;
  keys = nullptr;

  if(header.version != LDML_KMXPLUS_VERSION_17 && header.version != LDML_KMXPLUS_VERSION_19) {
    return false;
  }

  if(!COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_LAYR>::set(newLayr)) {
    return false;
  }
  DebugLog("validating newLayr=%p", newLayr);
  is_valid = true;
  if (newLayr == nullptr) {
    // Note: kmx_plus::kmx_plus has already called section_from_bytes()
    // which validates this section's length. Will be nullptr here if invalid.
    return true; // not invalid, just missing
  }
  KMX_DWORD offset = this->header.calculateBaseSize(LDML_LENGTH_LAYR);  // skip past non-dynamic portion

  own_forms = false;
  if(header.version == LDML_KMXPLUS_VERSION_17) {
    // forms (required)
    const COMP_KMXPLUS_LAYR_FORM_V17 *forms_v17;
    is_valid = is_valid && get_required_section_data_at_offset_and_increment<COMP_KMXPLUS_LAYR, COMP_KMXPLUS_LAYR_FORM_V17>(
        data(), header, data()->formCount, offset, forms_v17);
    assert(is_valid);
    if(is_valid) {
      own_forms = true;
      COMP_KMXPLUS_LAYR_FORM_V19 *localForms = new COMP_KMXPLUS_LAYR_FORM_V19[data()->formCount];
      for(KMX_DWORD i = 0; i < data()->formCount; i++) {
        localForms[i].hardware = forms_v17[i].hardware;
        localForms[i].layer = forms_v17[i].layer;
        localForms[i].count = forms_v17[i].count;
        localForms[i].minDeviceWidth = forms_v17[i].minDeviceWidth;

        localForms[i].baseLayout = 0;
        localForms[i].fontFaceName = 0;
        localForms[i].fontSizePct = 100;
        localForms[i].flags = 0;
      }
      forms = localForms;
    }
  } else { // header.version == LDML_KMXPLUS_VERSION_19
    is_valid = is_valid && get_required_section_data_at_offset_and_increment<COMP_KMXPLUS_LAYR, COMP_KMXPLUS_LAYR_FORM_V19>(
        data(), header, data()->formCount, offset, forms);
    assert(is_valid);
  }
  // entries (required) - note, "entryCount" is called "layerCount" in COMP_KMXPLUS_LAYR
  is_valid = is_valid && get_required_section_data_at_offset_and_increment<COMP_KMXPLUS_LAYR, COMP_KMXPLUS_LAYR_ENTRY>(
      data(), header, data()->layerCount, offset, entries);
  assert(is_valid);

  // rows (required)
  is_valid = is_valid && get_required_section_data_at_offset_and_increment<COMP_KMXPLUS_LAYR, COMP_KMXPLUS_LAYR_ROW>(
      data(), header, data()->rowCount, offset, rows);
  assert(is_valid);

  // keys (required)
  is_valid = is_valid && get_required_section_data_at_offset_and_increment<COMP_KMXPLUS_LAYR, COMP_KMXPLUS_LAYR_KEY>(
      data(), header, data()->keyCount, offset, keys);
  assert(is_valid);

  // Now, validate offsets by walking
  if (is_valid) {
    for(KMX_DWORD i = 0; is_valid && i < data()->formCount; i++) {
      const COMP_KMXPLUS_LAYR_FORM_V19 &form = forms[i];
      // is the count off the end?
      DebugLog(
          "<layers> %d: hardware s#0x%X, layers %d..%d, minDeviceWidth %.1fmm", i, form.hardware, form.layer,
          form.layer + form.count - 1, form.minDeviceWidth * (double)0.1);
      if ((form.layer >= data()->layerCount) || (form.layer + form.count > data()->layerCount)) {
        DebugLog("COMP_KMXPLUS_LAYR_Helper: form[%d] would access layer %d+%d, > count %d",
            i, form.layer, form.count, data()->layerCount);
        is_valid = false;
        assert(is_valid);
      }
      // TODO-EMBED-OSK-IN-KMX: other validations needed?
    }
    for(KMX_DWORD i = 0; is_valid && i < data()->layerCount; i++) {
      const COMP_KMXPLUS_LAYR_ENTRY &entry = entries[i];
      // is the count off the end?
      DebugLog(
          "<layer> %d: id s#0x%X, rows %d..%d, modifier=0x%X", i, entry.id, entry.row, entry.row+entry.count-1, entry.mod);
      if ((entry.row >= data()->rowCount) || (entry.row + entry.count > data()->rowCount)) {
        DebugLog("COMP_KMXPLUS_LAYR_Helper: entry[%d] would access row %d+%d, > count %d",
            i, entry.row, entry.count, data()->rowCount);
        is_valid = false;
        assert(is_valid);
      }
      if (!LDML_IS_VALID_MODIFIER_BITS(entry.mod)) {
        DebugLog("Invalid modifier value");
        assert(false);
        return false;
      }
    }
    for(KMX_DWORD i = 0; is_valid && i < data()->rowCount; i++) {
      const COMP_KMXPLUS_LAYR_ROW &row = rows[i];
      // is the count off the end?
      if ((row.key >= data()->keyCount) || (row.key + row.count > data()->keyCount)) {
        DebugLog("COMP_KMXPLUS_LAYR_Helper: row[%d] would access key %d+%d, > count %d",
            i, row.key, row.count, data()->keyCount);
        is_valid = false;
        assert(is_valid);
      }
    }
  }
  // Return results
  DebugLog("COMP_KMXPLUS_LAYR_Helper.set(): %s", is_valid ? "valid" : "invalid");
  assert(is_valid);
  return is_valid;
}

bool COMP_KMXPLUS_LAYR_Helper::valid() const {
  return is_valid;
}

const COMP_KMXPLUS_LAYR_FORM_V19 *
COMP_KMXPLUS_LAYR_Helper::getForm(KMX_DWORD form) const {
  if (!valid() || form >= data()->formCount) {
    assert(false);
    return nullptr;
  }
  return forms + form;
}

const COMP_KMXPLUS_LAYR_ENTRY *
COMP_KMXPLUS_LAYR_Helper::getEntry(KMX_DWORD entry) const {
  if (!valid() || entry >= data()->layerCount) {
    assert(false);
    return nullptr;
  }
  return entries + entry;
}

const COMP_KMXPLUS_LAYR_ROW *
COMP_KMXPLUS_LAYR_Helper::getRow(KMX_DWORD row) const {
  if (!valid() || row >= data()->rowCount) {
    assert(false);
    return nullptr;
  }
  return rows + row;
}

const COMP_KMXPLUS_LAYR_KEY *
COMP_KMXPLUS_LAYR_Helper::getKey(KMX_DWORD key) const {
  if (!valid() || key >= data()->keyCount) {
    assert(false);
    return nullptr;
  }
  return keys + key;
}

bool
COMP_KMXPLUS_KEYS::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(!is_block_valid(header, header.headerSize(),
    sizeof(*this)
      + (keyCount    * sizeof(COMP_KMXPLUS_KEYS_KEY))
      + (flicksCount * sizeof(COMP_KMXPLUS_KEYS_FLICK_LIST))
      + (flickCount  * sizeof(COMP_KMXPLUS_KEYS_FLICK_ELEMENT))
      + (kmapCount   * sizeof(COMP_KMXPLUS_KEYS_KMAP)))) {
    return false;
  }
  // further validation in the COMP_KMXPLUS_KEYS_Helper helper obj
  return true;
}


COMP_KMXPLUS_KEYS_Helper::COMP_KMXPLUS_KEYS_Helper() : is_valid(false) {
}

bool
COMP_KMXPLUS_KEYS_Helper::set(const COMP_KMXPLUS_KEYS *newKeys) {
  is_valid = false;
  keys = nullptr;
  flickLists = nullptr;
  flickElements = nullptr;
  kmap = nullptr;

  if(!COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_KEYS>::set(newKeys)) {
    return false;
  }
  DebugLog("validating newKeys=%p", newKeys);
  is_valid = true;
  if (newKeys == nullptr) {
    // Note: kmx_plus::kmx_plus has already called section_from_bytes()
    // which validates this section's length. Will be nullptr here if invalid.
    return true; // not invalid, just missing
  }

  KMX_DWORD offset = this->header.calculateBaseSize(LDML_LENGTH_KEYS);

  // keys (required)
  is_valid = is_valid && get_required_section_data_at_offset_and_increment<COMP_KMXPLUS_KEYS, COMP_KMXPLUS_KEYS_KEY>(
      data(), header, data()->keyCount, offset, keys);
  assert(is_valid);

  // flicks (optional) - note tricky "flicksCount" vs "flickCount" in COMP_KMXPLUS_KEYS
  is_valid = is_valid && get_optional_section_data_at_offset_and_increment<COMP_KMXPLUS_KEYS, COMP_KMXPLUS_KEYS_FLICK_LIST>(
      data(), header, data()->flicksCount, offset, flickLists);
  assert(is_valid);

  // flick (optional) - note tricky "flicksCount" vs "flickCount" in COMP_KMXPLUS_KEYS
  is_valid = is_valid && get_optional_section_data_at_offset_and_increment<COMP_KMXPLUS_KEYS, COMP_KMXPLUS_KEYS_FLICK_ELEMENT>(
      data(), header, data()->flickCount, offset, flickElements);
  assert(is_valid);

  // kmap (optional)
  is_valid = is_valid && get_optional_section_data_at_offset_and_increment<COMP_KMXPLUS_KEYS, COMP_KMXPLUS_KEYS_KMAP>(
      data(), header, data()->kmapCount, offset, kmap);
  assert(is_valid);

  // Now, validate offsets by walking
  if (is_valid) {
    for(KMX_DWORD i = 0; is_valid && i < data()->keyCount; i++) {
      const auto &key = keys[i];
      // is the count off the end?
      DebugLoad( "<key #%d> id=0x%X, to=0x%X, flicks=%d", i, key.id, key.to, key.flicks); // TODO-LDML: could dump more fields here
      if (key.flicks >0 && key.flicks >= data()->flicksCount) {
        DebugLog("key[%d] has invalid flicks index %d", i, key.flicks);
        is_valid = false;
        assert(is_valid);
      }
      if (!(key.flags & LDML_KEYS_KEY_FLAGS_EXTEND)) {
        // if extend flag is clear, then the 'to' field is a UTF-32 char
        km_core_usv to = key.to;
        if (!Uni_IsValid(to)) {
          DebugLog("key[%d] has invalid non-extended UChar to U+%04X", i, key.to);
          is_valid = false;
        }
      }
    }
    for(KMX_DWORD i = 0; is_valid && i < data()->flicksCount; i++) {
      const auto &e = flickLists[i];
      // is the count off the end?
      DebugLoad("<flicks> %d: index %d, count %d", i, e.flick, e.count);
      if (i == 0) {
        if (e.flick != 0 || e.count != 0) {
          DebugLog("Error: Invalid Flick #0");
          is_valid = false;
          assert(is_valid);
        }
      } else if ((e.flick >= data()->flickCount) || (e.flick + e.count > data()->flickCount)) {
        DebugLog("flicks[%d] would access flick %d+%d, > count %d", i, e.flick, e.count, data()->flickCount);
        is_valid = false;
        assert(is_valid);
      }
    }
    for(KMX_DWORD i = 0; is_valid && i < data()->flickCount; i++) {
      const auto &e = flickElements[i];
      // validate to is present
      if (e.to == 0 || e.directions == 0) {
        DebugLog("flickElement[%d] has empty to=%0x%X or directions=%0x%X", i, e.to, e.directions);
        is_valid = false;
        assert(is_valid);
      }
      DebugLoad("<flick> %d: to=0x%X, directions=0x%X", i, e.to, e.directions);
    }
    // now the kmap
    DebugLoad(" kmap count: #0x%X", data()->kmapCount);
    for (KMX_DWORD i = 0; i < data()->kmapCount; i++) {
      DebugLoad(" #0x%d\n", i);
      auto &entry = kmap[i];
      DebugLoad("  vkey\t0x%X", entry.vkey);
      DebugLoad("  mod\t0x%X", entry.mod);
      DebugLoad("  key\t#0x%X", entry.key);
      if (!LDML_IS_VALID_MODIFIER_BITS(entry.mod)) {
        DebugLog("Invalid modifier value");
        assert(false);
        is_valid = false;
      }
      if (entry.key >= data()->keyCount) {
        // preposterous key #
        DebugLog("kmap[0x%X].key = #0x%X, but that is >= keyCount 0x%X", i, entry.key, data()->keyCount);
        assert(false);
        is_valid = false;
      }
    }
  }
  // Return results
  DebugLog("COMP_KMXPLUS_KEYS_Helper.set(): %s", is_valid ? "valid" : "invalid");
  return is_valid;
}

const COMP_KMXPLUS_KEYS_KEY *
COMP_KMXPLUS_KEYS_Helper::getKeys(KMX_DWORD i) const {
  if (!valid() || i >= data()->keyCount) {
    assert(false);
    return nullptr;
  }
  return keys + i;
}

const COMP_KMXPLUS_KEYS_KEY*
COMP_KMXPLUS_KEYS_Helper::findKeyByStringId(KMX_DWORD strId, KMX_DWORD &i) const {
  for (; i < data()->keyCount; i++) {
    if (keys[i].id == strId) {
      return &keys[i];
    }
  }
  return nullptr;
}

const COMP_KMXPLUS_KEYS_KEY*
COMP_KMXPLUS_KEYS_Helper::findKeyByStringTo(const std::u16string& str, KMX_DWORD strId, KMX_DWORD &i) const {
  for (; i < data()->keyCount; i++) {
    if (keys[i].flags & LDML_KEYS_KEY_FLAGS_EXTEND) {
      if (strId != 0 && keys[i].to == strId) {
        return &keys[i];
      }
    } else if (keys[i].get_to_string() == str) {
      return &keys[i];
    }
  }
  return nullptr;
}

const COMP_KMXPLUS_KEYS_FLICK_LIST *
COMP_KMXPLUS_KEYS_Helper::getFlickLists(KMX_DWORD i) const {
  if (!valid() || i >= data()->flicksCount) {
    assert(false);
    return nullptr;
  }
  return flickLists + i;
}

const COMP_KMXPLUS_KEYS_FLICK_ELEMENT *
COMP_KMXPLUS_KEYS_Helper::getFlickElements(KMX_DWORD i) const {
  if (!valid() || i >= data()->flickCount) {
    assert(false);
    return nullptr;
  }
  return flickElements + i;
}

const COMP_KMXPLUS_KEYS_KMAP *
COMP_KMXPLUS_KEYS_Helper::getKmap(KMX_DWORD i) const {
  if (!valid() || i >= data()->kmapCount) {
    assert(false);
    return nullptr;
  }
  return kmap + i;
}

std::u16string
COMP_KMXPLUS_KEYS_KEY::get_to_string() const {
  assert(!(flags & LDML_KEYS_KEY_FLAGS_EXTEND)); // should not be called.
  return COMP_KMXPLUS_STRS::str_from_char(to);
}

// LIST

bool
COMP_KMXPLUS_LIST::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(!is_block_valid(header, header.headerSize(),
    sizeof(*this)
      + (listCount  * sizeof(COMP_KMXPLUS_LIST_ITEM))
      + (indexCount * sizeof(COMP_KMXPLUS_LIST_INDEX)))) {
    return false;
  }
  return true;
}


COMP_KMXPLUS_LIST_Helper::COMP_KMXPLUS_LIST_Helper() : is_valid(false) {
}

bool
COMP_KMXPLUS_LIST_Helper::set(const COMP_KMXPLUS_LIST *newList) {
  is_valid = false;
  lists = nullptr;
  indices = nullptr;

  if(!COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_LIST>::set(newList)) {
    return false;
  }
  DebugLog("validating newList=%p", newList);
  is_valid = true;
  if (newList == nullptr) {
    // Note: kmx_plus::kmx_plus has already called section_from_bytes()
    // which validates this section's length. Will be nullptr here if invalid.
    return true; // not invalid, just missing
  }

  KMX_DWORD offset = this->header.calculateBaseSize(LDML_LENGTH_LIST);  // skip past non-dynamic portion

  // lists (optional)
  is_valid = is_valid && get_optional_section_data_at_offset_and_increment<COMP_KMXPLUS_LIST, COMP_KMXPLUS_LIST_ITEM>(
      data(), header, data()->listCount, offset, lists);
  assert(is_valid);

  // indices (optional)
  is_valid = is_valid && get_optional_section_data_at_offset_and_increment<COMP_KMXPLUS_LIST, COMP_KMXPLUS_LIST_INDEX>(
      data(), header, data()->indexCount, offset, indices);
  assert(is_valid);

  // Now, validate offsets by walking
  if (is_valid) {
    for (KMX_DWORD i = 0; is_valid && i < data()->listCount; i++) {
      const auto &e = lists[i];
      // is the count off the end?
      DebugLog("list 0x%X: index %d, count %d", i, e.index, e.count);
      if (i == 0) {
        if (e.index != 0 || e.count != 0) {
          DebugLog("Error: Invalid List #0");
          is_valid = false;
          assert(is_valid);
        }
      } else if ((e.index >= data()->indexCount) || (e.index + e.count > data()->indexCount)) {
        DebugLog("list[%d] would access index %d+%d, > count %d", i, e.index, e.count, data()->indexCount);
        is_valid = false;
        assert(is_valid);
      }
    }
#if KMXPLUS_DEBUG_LOAD
    for (KMX_DWORD i = 0; is_valid && i < data()->indexCount; i++) {
      const auto &e = indices[i];
      DebugLoad(" index %d: str 0x%X", i, e);
    }
#endif
  }
  // Return results
  DebugLog("COMP_KMXPLUS_LIST_Helper.set(): %s", is_valid ? "valid" : "invalid");
  assert(is_valid);
  return is_valid;
}

const COMP_KMXPLUS_LIST_ITEM *
COMP_KMXPLUS_LIST_Helper::getList(KMX_DWORD i) const {
  if (!valid() || i >= data()->listCount) {
    assert(false);
    return nullptr;
  }
  return lists + i;
}

const COMP_KMXPLUS_LIST_INDEX *
COMP_KMXPLUS_LIST_Helper::getIndex(KMX_DWORD i) const {
  if (!valid() || i >= data()->indexCount) {
    assert(false);
    return nullptr;
  }
  return indices + i;
}


// USET

bool
COMP_KMXPLUS_USET::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
  if(!is_block_valid(header, header.headerSize(), sizeof(*this)
      + (usetCount  * sizeof(COMP_KMXPLUS_USET_USET))
      + (rangeCount * sizeof(COMP_KMXPLUS_USET_RANGE)))) {
    return false;
  }
  return true; // see helper
}

COMP_KMXPLUS_USET_RANGE::COMP_KMXPLUS_USET_RANGE(KMX_DWORD s, KMX_DWORD e) : start(s), end(e) {
}

COMP_KMXPLUS_USET_RANGE::COMP_KMXPLUS_USET_RANGE(const COMP_KMXPLUS_USET_RANGE &other) : start(other.start), end(other.end) {
}

COMP_KMXPLUS_USET_Helper::COMP_KMXPLUS_USET_Helper() : is_valid(false), usets(nullptr), ranges(nullptr) {
}

bool
COMP_KMXPLUS_USET_Helper::set(const COMP_KMXPLUS_USET *newUset) {
  is_valid = false;
  usets = nullptr;
  ranges = nullptr;

  if(!COMP_KMXPLUS_Section_Helper<COMP_KMXPLUS_USET>::set(newUset)) {
    return false;
  }
  DebugLoad("validating newUset=%p", newUset);
  is_valid = true;
  if (newUset == nullptr) {
    // Note: kmx_plus::kmx_plus has already called section_from_bytes()
    // which validates this section's length. Will be nullptr here if invalid.
    return true; // not invalid, just missing
  }
  KMX_DWORD offset = this->header.calculateBaseSize(LDML_LENGTH_USET);  // skip past non-dynamic portion

  // usets (optional)
  is_valid = is_valid && get_optional_section_data_at_offset_and_increment<COMP_KMXPLUS_USET, COMP_KMXPLUS_USET_USET>(
      data(), header, data()->usetCount, offset, usets);
  assert(is_valid);

  // ranges (optional)
  is_valid = is_valid && get_optional_section_data_at_offset_and_increment<COMP_KMXPLUS_USET, COMP_KMXPLUS_USET_RANGE>(
      data(), header, data()->rangeCount, offset, ranges);
  assert(is_valid);

  // Now, validate offsets by walking
  // is_valid must be true at this point.
  for (KMX_DWORD i = 0; is_valid && i < data()->usetCount; i++) {
    const auto &e = usets[i];
    // is the count off the end?
    DebugLog("uset 0x%X: range %d, count %d, pattern 0x%X", i, e.range, e.count, e.pattern);
    if ((e.range >= data()->rangeCount) || (e.range + e.count > data()->rangeCount)) {
      DebugLog("uset[%d] would access range %d+%d, > count %d", i, e.range, e.count, data()->rangeCount);
      is_valid = false;
      assert(is_valid);
    } else {
      /** last lastEnd value */
      KMX_DWORD lastEnd = 0x0;
      for (KMX_DWORD r = 0; is_valid && r < e.count; r++) {
        const auto &range = ranges[e.range + r];  // already range-checked 'r' above
        if (!Uni_IsValid(range.start, range.end)) {
          DebugLog("uset[%d][%d] not valid: [U+%04X-U+%04X]", i, r, range.start, range.end);
          is_valid = false;
          assert(is_valid);
        } else if (range.end < range.start) {
          // range swapped
          DebugLog("uset[%d]: range[%d+%d] end 0x%X<start 0x%X", i, e.range, r, range.end, range.start);
          is_valid = false;
          assert(is_valid);
        } else if (range.start < lastEnd) {
          // overlaps prior range AND/OR ranges aren't in order
          DebugLog("uset[%d]: range[%d+%d] has start 0x%X, not > prior range (overlap/ranges unsorted?)",
            i, e.range, r, range.start);
            is_valid = false;
            assert(is_valid);
        } else {
          lastEnd = range.end;
        }
      }
    }
  }
  // Return results
  DebugLog("COMP_KMXPLUS_USET_Helper.set(): %s", is_valid ? "valid" : "invalid");
  assert(is_valid);
  return is_valid;
}

SimpleUSet::SimpleUSet(const COMP_KMXPLUS_USET_RANGE *newRange, size_t newCount)  {
  for (size_t i = 0; i < newCount; i++) {
    ranges.emplace_back(newRange[i].start, newRange[i].end);
  }
}

SimpleUSet::SimpleUSet() {
}

bool SimpleUSet::contains(km_core_usv ch) const {
  for (const auto &range : ranges) {
    if (range.start <= ch && range.end >= ch) {
      return true;
    }
  }
  return false;
}

bool
SimpleUSet::valid() const {
  // double check
  for (const auto &range : ranges) {
    if (!Uni_IsValid(range.start, range.end)) {
      DebugLog("Invalid UnicodeSet (contains noncharacters): [U+%04X,U+%04X]", (int)range.start, (int)range.end);
      return false;
    }
  }
  return true;
}

void
SimpleUSet::dump() const {
  DebugLog(" - USet size=%d", ranges.size());
  for (const auto &range : ranges) {
    if (range.start == range.end) {
      DebugLog("  - [U+%04X]", (uint32_t)range.start);
    } else {
      DebugLog("  - [U+%04X-U+%04X]", (uint32_t)range.start, (uint32_t)range.end);
    }
  }
}

SimpleUSet
COMP_KMXPLUS_USET_Helper::getUset(KMXPLUS_USET i) const {
  if (!valid() || i >= data()->usetCount) {
    assert(false);
    return SimpleUSet(nullptr, 0); // empty set
  }
  auto &set = usets[i];
  return SimpleUSet(getRange(set.range), set.count);
}

const COMP_KMXPLUS_USET_RANGE *
COMP_KMXPLUS_USET_Helper::getRange(KMX_DWORD i) const {
  if (!valid() || i >= data()->rangeCount) {
    assert(false);
    return nullptr;
  }
  return ranges + i;
}

// ---- constructor

kmx_plus::kmx_plus(const COMP_KEYBOARD *keyboard, size_t length)
    : bksp(nullptr), disp(nullptr), elem(nullptr), key2(nullptr), layr(nullptr), list(nullptr), loca(nullptr), meta(nullptr),
      sect(nullptr), strs(nullptr), tran(nullptr), vars(nullptr), valid(false) {
  DebugLog("kmx_plus: Got a COMP_KEYBOARD at %p\n", keyboard);
#if !KMXPLUS_DEBUG_LOAD
  DebugLog("Note: define KMXPLUS_DEBUG_LOAD=1 at compile time for more verbosity in loading");
#endif
  if (!(keyboard->dwFlags & KF_KMXPLUS)) {
    DebugLog("Err: flags COMP_KEYBOARD.dwFlags did not have KF_KMXPLUS set");
    valid = false;
    assert(valid);
    return;
  }
  const COMP_KEYBOARD_EX* ex = reinterpret_cast<const COMP_KEYBOARD_EX*>(keyboard);

  DebugLog("kmx_plus(): KMXPlus offset 0x%X, KMXPlus size 0x%X\n", ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize);
  if (ex->kmxplus.dpKMXPlus < sizeof(COMP_KEYBOARD_EX)) {
    DebugLog("dwKMXPlus is not past the end of COMP_KEYBOARD_EX");
    valid = false;
    assert(valid);
    return;
  }
  // check individual components to avoid overflow on sum (we'll never get even
  // a 2GB file so if both components are < length then we are okay to sum)
  if (ex->kmxplus.dpKMXPlus > length ||
      ex->kmxplus.dwKMXPlusSize > length ||
      ex->kmxplus.dpKMXPlus + ex->kmxplus.dwKMXPlusSize > length) {
    DebugLog("dpKMXPlus + dwKMXPlusSize is past the end of the file");
    valid = false;
    assert(valid);
    return;
  }

  if( ex->kmxplus.dwKMXPlusSize < sizeof(COMP_KMXPLUS_HEADER_17)) {
    DebugLog("dwKMXPlusSize is too small to contain a section");
    valid = false;
    assert(valid);
    return;
  }

  const uint8_t* rawdata = reinterpret_cast<const uint8_t*>(keyboard);
  valid = true;
  COMP_KMXPLUS_HEADER sect_header;
  KMX_DWORD fileVersion = determine_file_version_from_bytes(rawdata+ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize);
  if(fileVersion == 0) {
    valid = false;
    assert(valid);
    return;
  }

  valid = section_from_bytes<COMP_KMXPLUS_SECT>(rawdata+ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize, fileVersion, sect_header, sect) && valid;
  if (sect == nullptr) {
    DebugLog("kmx_plus(): 'sect' missing or did not validate");
    valid = false;
    assert(valid);
    return;
  }

  // load other sections, validating as we go each field will be set to nullptr
  // if validation fails or if the section is missing and then initialize the
  // helper objects for sections with dynamic parts. Note: all of the helper
  // setters will be passed 'nullptr' if any section had failed validation, or
  // was missing.
  //
  // A missing section does not invalidate the kmxplus. We attempt to initialize
  // each one
  valid = get_section_from_sect(sect, sect_header, &bkspHelper, bksp) && valid; // note overload
  valid = get_section_from_sect<COMP_KMXPLUS_DISP>(sect, sect_header, &dispHelper, disp) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_ELEM>(sect, sect_header, &elemHelper, elem) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_KEYS>(sect, sect_header, &key2Helper, key2) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_LAYR>(sect, sect_header, &layrHelper, layr) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_LIST>(sect, sect_header, &listHelper, list) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_LOCA>(sect, sect_header, &locaHelper, loca) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_META>(sect, sect_header, &metaHelper, meta) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_STRS>(sect, sect_header, &strsHelper, strs) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_TRAN>(sect, sect_header, &tranHelper, tran) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_USET>(sect, sect_header, &usetHelper, uset) && valid;
  valid = get_section_from_sect<COMP_KMXPLUS_VARS>(sect, sect_header, &varsHelper, vars) && valid;
}

std::u16string
COMP_KMXPLUS_STRS::get(const COMP_KMXPLUS_HEADER& header, KMX_DWORD entry) const {
    assert(entry < count);
    if (entry >= count) {
        return std::u16string(); // Fallback: empty string
    }
    const KMX_DWORD offset = entries[entry].offset;
    const KMX_DWORD length = entries[entry].length;

    // the string is null terminated in the data file, thus length + 1
    auto start = get_section_data_at_offset<COMP_KMXPLUS_STRS, KMX_WCHAR>(this, header, offset, (length + 1) * sizeof(KMX_WCHAR));
    if(!start) {
      return std::u16string();
    }
    return std::u16string(start, length);
}

std::u16string COMP_KMXPLUS_STRS_Helper::get(KMX_DWORD entry) const {
  assert(data());
  if(!data()) return std::u16string();
  return data()->get(header, entry);
}

KMX_DWORD COMP_KMXPLUS_STRS::find(const COMP_KMXPLUS_HEADER& header, const std::u16string& s) const {
  if (s.empty()) {
    return 0; // shortcut
  }
  // TODO-LDML: suboptimal, but currently only run from the test runner. Could be a binary search since the strings are already in codepoint order.
  for (KMX_DWORD i = 0; i<count; i++) {
    if (s == get(header, i)) {
      return i;
    }
  }
  return 0; // not found
}

KMX_DWORD COMP_KMXPLUS_STRS_Helper::find(const std::u16string& s) const {
  assert(data());
  if(!data()) return 0;
  return data()->find(header, s);
}

bool
COMP_KMXPLUS_VARS::valid(COMP_KMXPLUS_HEADER const &header, KMX_DWORD _kmn_unused(length)) const {
    if(!is_block_valid(header, header.headerSize(), sizeof(*this)
      + (varCount  * sizeof(COMP_KMXPLUS_VARS_ITEM)))) {
    DebugLog("header.size < expected size");
    assert(false);
    return false;
  }
  return true;
}

const COMP_KMXPLUS_VARS_ITEM *COMP_KMXPLUS_VARS::findByStringId(KMX_DWORD strId) const {
  if (strId == 0) {
    return nullptr;
  }
  for (KMX_DWORD index = 0; index < varCount; index++) {
    if (varEntries[index].id == strId) {
      return &(varEntries[index]);
    }
  }
  return nullptr;
}


}  // namespace kmx
}  // namespace core
}  // namespace km
