/*
  Copyright:        Copyright (C) 2022 SIL International.
  Authors:          srl295
  This file defines the structure of a kmx_plus file, starting at COMP_KEYBOARD_KMXPLUSINFO.dpKMXPlus
*/

#pragma once

#include <km_types.h>
#include <kmx/kmx_base.h>
#include <kmx_file.h>
#include <ldml/keyboardprocessor_ldml.h>

/**
 * @def KMXPLUS_DEBUG Set to 1 to enable debug output
 */
#ifndef KMXPLUS_DEBUG
#if defined(DEBUG)
#define KMXPLUS_DEBUG 1
#else
#define KMXPLUS_DEBUG 0
#endif
#endif

namespace km {
namespace kbp {
namespace kmx {

/**
 * Using C99 flexible array initializers: entries[]
 * https://docs.microsoft.com/en-us/cpp/error-messages/compiler-warnings/compiler-warning-levels-2-and-4-c4200
 */
#if defined(_WIN32)
#pragma warning ( disable : 4200 )
#endif

struct COMP_KMXPLUS_HEADER {
  KMX_DWORD ident;  // 0000 Section name
  KMX_DWORD size;   // 0004 Section length
};

// Assert that the length matches the declared length
static_assert(sizeof(struct COMP_KMXPLUS_HEADER) == LDML_LENGTH_HEADER, "mismatched size of section header");

struct COMP_KMXPLUS_SECT_ENTRY {
  KMX_DWORD sect;    // 0010+ Section identity
  KMX_DWORD offset;  // 0014+ Section offset relative to dpKMXPlus of section
};

struct COMP_KMXPLUS_SECT {
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD total;                     // 0008 KMXPlus entire length
  KMX_DWORD count;                     // 000C number of section headers
  COMP_KMXPLUS_SECT_ENTRY entries[];   // 0010 section entries
  /**
   * @brief Get the offset of a section, or 0
   *
   * @param ident section id such as 'strs'. Never 'sect'
   * @return KMX_DWORD offset from beginning of kmxplus
   */
  KMX_DWORD find(KMX_DWORD ident) const;
};

// Assert that the length matches the declared length
static_assert(sizeof(struct COMP_KMXPLUS_SECT) == LDML_LENGTH_SECT, "mismatched size of section sect");
static_assert(sizeof(struct COMP_KMXPLUS_SECT) % 0x10 == 0, "Structs prior to entries[] should align to 128-bit boundary");

struct COMP_KMXPLUS_STRS_ENTRY {
    KMX_DWORD offset;                 // 0010+ offset from this blob
    KMX_DWORD length;                 // 0014+ str length (UTF-16LE units)
};

struct COMP_KMXPLUS_STRS {
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;                    // 0008 count of str entries
  KMX_DWORD reserved;                 // 000C padding
  COMP_KMXPLUS_STRS_ENTRY entries[];  // 0010+ entries

  /**
   * @brief Get a string entry
   *
   * @param entry entry number
   * @param buf output buffer
   * @param bufsiz buffer size in bytes
   * @return nullptr or a pointer to the output buffer
   */
  PKMX_WCHAR get(KMX_DWORD entry, PKMX_WCHAR buf, KMX_DWORD bufsiz) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_STRS) % 0x10 == 0, "Structs prior to entries[] should align to 128-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_STRS) == LDML_LENGTH_STRS, "mismatched size of section strs");

struct COMP_KMXPLUS_META {
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD name;
  KMX_DWORD author;
  KMX_DWORD conform;
  KMX_DWORD layout;
  KMX_DWORD normalization;
  KMX_DWORD indicator;
  KMX_DWORD settings;
};

static_assert(sizeof(struct COMP_KMXPLUS_META) == LDML_LENGTH_META, "mismatched size of section meta");

struct COMP_KMXPLUS_LOCA_ENTRY {
  KMX_DWORD locale; // 0010+ locale string entry
};

struct COMP_KMXPLUS_LOCA {
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count; // 0008 number of locales
  KMX_DWORD reserved;
  COMP_KMXPLUS_LOCA_ENTRY entries[];
};

static_assert(sizeof(struct COMP_KMXPLUS_LOCA) % 0x10 == 0, "Structs prior to entries[] should align to 128-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_LOCA) == LDML_LENGTH_LOCA, "mismatched size of section loca");

struct COMP_KMXPLUS_KEYS_ENTRY {
    KMX_DWORD vkey;
    KMX_DWORD mod;
    KMX_DWORD to;
    KMX_DWORD flags;
};

struct COMP_KMXPLUS_KEYS {
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;    // number of keys
  KMX_DWORD reserved; // padding
  COMP_KMXPLUS_KEYS_ENTRY entries[];
};

static_assert(sizeof(struct COMP_KMXPLUS_KEYS) % 0x10 == 0, "Structs prior to entries[] should align to 128-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_KEYS) == LDML_LENGTH_KEYS, "mismatched size of section keys");

struct COMP_KMXPLUS_VKEY_ENTRY {
    KMX_DWORD vkey;
    KMX_DWORD target;
};

struct COMP_KMXPLUS_VKEY {
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;
  KMX_DWORD reserved;
  COMP_KMXPLUS_VKEY_ENTRY entries[];
};

static_assert(sizeof(struct COMP_KMXPLUS_VKEY) % 0x10 == 0, "Structs prior to entries[] should align to 128-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_VKEY) == LDML_LENGTH_VKEY, "mismatched size of section vkey");

/**
 * See above
 */
#if defined(_WIN32)
#pragma warning ( default : 4200 )
#endif


/**
 * @brief Validate that this data is the named section.
 *
 * @param data raw data
 * @param ident 4-byte section type
 * @return COMP_KMXPLUS_ALLDATA* or null
 */
static inline const COMP_KMXPLUS_HEADER *
validate_as_section(const uint8_t *data, uint32_t ident) {
  if (!data) {
    return nullptr;
  }
  const COMP_KMXPLUS_HEADER *all = reinterpret_cast<const COMP_KMXPLUS_HEADER *>(data);
  if (ident != all->ident || (all->size < LDML_LENGTH_HEADER)) {
    return nullptr;  // invalid header or wrong section
  }
  return all;
}

/**
 * cast raw data to section
 * @return section data or null on error
 */
static inline const COMP_KMXPLUS_SECT *
as_kmxplus_sect(const uint8_t *data) {
  const COMP_KMXPLUS_HEADER *all = validate_as_section(data, LDML_SECTIONID_SECT);
  return reinterpret_cast<const COMP_KMXPLUS_SECT *>(all);
}

/**
 * cast raw data to section
 * @return section data or null on error
 */
static inline const COMP_KMXPLUS_STRS *
as_kmxplus_strs(const uint8_t *data) {
  const COMP_KMXPLUS_HEADER *all = validate_as_section(data, LDML_SECTIONID_STRS);
  return reinterpret_cast<const COMP_KMXPLUS_STRS *>(all);
}

/**
 * cast raw data to section
 * @return section data or null on error
 */
static inline const COMP_KMXPLUS_KEYS *
as_kmxplus_keys(const uint8_t *data) {
  const COMP_KMXPLUS_HEADER *all = validate_as_section(data, LDML_SECTIONID_KEYS);
  return reinterpret_cast<const COMP_KMXPLUS_KEYS *>(all);
}
/**
 * cast raw data to section
 * @return section data or null on error
 */
static inline const COMP_KMXPLUS_LOCA *
as_kmxplus_loca(const uint8_t *data) {
  const COMP_KMXPLUS_HEADER *all = validate_as_section(data, LDML_SECTIONID_LOCA);
  return reinterpret_cast<const COMP_KMXPLUS_LOCA *>(all);
}
/**
 * cast raw data to section
 * @return section data or null on error
 */
static inline const COMP_KMXPLUS_META *
as_kmxplus_meta(const uint8_t *data) {
  const COMP_KMXPLUS_HEADER *all = validate_as_section(data, LDML_SECTIONID_META);
  return reinterpret_cast<const COMP_KMXPLUS_META *>(all);
}
/**
 * convert raw data to section
 * @return section data or null on error
 */
static inline const COMP_KMXPLUS_VKEY *
as_kmxplus_vkey(const uint8_t *data) {
  const COMP_KMXPLUS_HEADER *all = validate_as_section(data, LDML_SECTIONID_VKEY);
  return reinterpret_cast<const COMP_KMXPLUS_VKEY *>(all);
}

/**
 * @param kmxplusdata data from the beginning of the KMXPlus section
 * @return true if valid
 */
bool validate_kmxplus_data(const uint8_t *kmxplusdata);

/**
 * @param keyboard pointer to PCOMP_KEYBOARD with plus data following
 * @return true if valid
 */
bool validate_kmxplus_data(kmx::PCOMP_KEYBOARD keyboard);

}  // namespace kmx
}  // namespace kbp
}  // namespace km
