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
  bool valid(KMX_DWORD length) const;
};

// Assert that the length matches the declared length
static_assert(sizeof(struct COMP_KMXPLUS_HEADER) == LDML_LENGTH_HEADER, "mismatched size of section header");

struct COMP_KMXPLUS_SECT_ENTRY {
  KMX_DWORD sect;    // 0010+ Section identity
  KMX_DWORD offset;  // 0014+ Section offset relative to dpKMXPlus of section
};

struct COMP_KMXPLUS_SECT {
  static const KMX_DWORD IDENT = LDML_SECTIONID_SECT;
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
  /**
   * @brief True if section is valid.
   * Does not validate the entire file.
   */
  bool valid(KMX_DWORD length) const;
};

// Assert that the length matches the declared length
static_assert(sizeof(struct COMP_KMXPLUS_SECT) == LDML_LENGTH_SECT, "mismatched size of section sect");
static_assert(sizeof(struct COMP_KMXPLUS_SECT) % 0x10 == 0, "Structs prior to entries[] should align to 128-bit boundary");

struct COMP_KMXPLUS_STRS_ENTRY {
    KMX_DWORD offset;                 // 0010+ offset from this blob
    KMX_DWORD length;                 // 0014+ str length (UTF-16LE units)
};

struct COMP_KMXPLUS_STRS {
  static const KMX_DWORD IDENT = LDML_SECTIONID_STRS;
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
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_STRS) % 0x10 == 0, "Structs prior to entries[] should align to 128-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_STRS) == LDML_LENGTH_STRS, "mismatched size of section strs");

struct COMP_KMXPLUS_META {
  static const KMX_DWORD IDENT = LDML_SECTIONID_META;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD name;
  KMX_DWORD author;
  KMX_DWORD conform;
  KMX_DWORD layout;
  KMX_DWORD normalization;
  KMX_DWORD indicator;
  KMX_DWORD settings;
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_META) == LDML_LENGTH_META, "mismatched size of section meta");

struct COMP_KMXPLUS_LOCA_ENTRY {
  KMX_DWORD locale; // 0010+ locale string entry
};

struct COMP_KMXPLUS_LOCA {
  static const KMX_DWORD IDENT = LDML_SECTIONID_LOCA;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count; // 0008 number of locales
  KMX_DWORD reserved;
  COMP_KMXPLUS_LOCA_ENTRY entries[];
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
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
  static const KMX_DWORD IDENT = LDML_SECTIONID_KEYS;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;    // number of keys
  KMX_DWORD reserved; // padding
  COMP_KMXPLUS_KEYS_ENTRY entries[];
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_KEYS) % 0x10 == 0, "Structs prior to entries[] should align to 128-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_KEYS) == LDML_LENGTH_KEYS, "mismatched size of section keys");

struct COMP_KMXPLUS_VKEY_ENTRY {
    KMX_DWORD vkey;
    KMX_DWORD target;
};

struct COMP_KMXPLUS_VKEY {
  static const KMX_DWORD IDENT = LDML_SECTIONID_VKEY;
  COMP_KMXPLUS_HEADER header;
  KMX_DWORD count;
  KMX_DWORD reserved;
  COMP_KMXPLUS_VKEY_ENTRY entries[];
  /**
   * @brief True if section is valid.
   */
  bool valid(KMX_DWORD length) const;
};

static_assert(sizeof(struct COMP_KMXPLUS_VKEY) % 0x10 == 0, "Structs prior to entries[] should align to 128-bit boundary");
static_assert(sizeof(struct COMP_KMXPLUS_VKEY) == LDML_LENGTH_VKEY, "mismatched size of section vkey");

/**
 * @brief helper accessor object for
 *
 */
class kmx_plus {
  public:
    /**
     * @brief Construct a new kmx_plus object.
     * Caller must preserve the keyboard file memory
     * until after this object is destroyed.
     *
     * @param keyboard the KMXPlus data
     */
    kmx_plus(const COMP_KEYBOARD *keyboard);
    const COMP_KMXPLUS_KEYS *keys;
    const COMP_KMXPLUS_LOCA *loca;
    const COMP_KMXPLUS_META *meta;
    const COMP_KMXPLUS_SECT *sect;
    const COMP_KMXPLUS_STRS *strs;
    const COMP_KMXPLUS_VKEY *vkey;
    inline bool is_valid() { return valid; }
  private:
    bool valid; // true if valid
};

/**
 * See above - undo workaround for variable arrays
 */
#if defined(_WIN32)
#pragma warning ( default : 4200 )
#endif

}  // namespace kmx
}  // namespace kbp
}  // namespace km
