/*
  Copyright:        Copyright (C) 2022 SIL International.
  Authors:          srl295
  Implementation for the KMX Plus utilities
*/

#include <km_types.h>
#include <kmx_file.h>
#include <kmx/kmx_plus.h>

/**
 * @def KMXPLUS_DEBUG Set to 1 to enable debug output
 */
#define KMXPLUS_DEBUG 0

#if KMXPLUS_DEBUG
#include <stdio.h>
#endif

#include <assert.h>

namespace km {
namespace kbp {
namespace kmx {

/**
 * @brief Usage: `KMXPLUS_PRINTF(("str: %s\n", "something"));`
 * Note double parens
 * \def KMXPLUS_DEBUG
 */
#if KMXPLUS_DEBUG
#define KMXPLUS_PRINTF(x) printf x
#else
#define KMXPLUS_PRINTF(x)
#endif


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

static bool
validate_kmxplus_header(const COMP_KMXPLUS_HEADER* hdr) {
  if (hdr == nullptr) {
    KMXPLUS_PRINTF(("! dump_kmxplus_header: NULL header\n"));
    return false;
  }
  if(!validate_section_name(hdr->ident)) {
    KMXPLUS_PRINTF((" (invalid section name) \n"));
    return false;
  }
  KMXPLUS_PRINTF((": (%X) size 0x%X\n", hdr->ident, hdr->size));
  return true;
}

static bool
validate_kmxplus_keys(const uint8_t* /*data*/, const COMP_KMXPLUS_KEYS* keys) {
  if(keys == nullptr) {
    KMXPLUS_PRINTF(("! could not load 'keys' section\n"));
    return false;
  }
  if(!validate_kmxplus_header((const COMP_KMXPLUS_HEADER*)keys)) {
    return false;
  }
  KMXPLUS_PRINTF((" count: #0x%X\n", keys->count));
  for (KMX_DWORD i = 0; i<keys->count; i++) {
    KMXPLUS_PRINTF((" #0x%d\n", i));
    const COMP_KMXPLUS_KEYS_ENTRY& entry = keys->entries[i];
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

static bool
validate_kmxplus_loca(const uint8_t* /*data*/, const COMP_KMXPLUS_LOCA* loca) {
  if(loca == nullptr) {
    KMXPLUS_PRINTF(("! could not load 'loca' section\n"));
    return false;
  }
  if(!validate_kmxplus_header((const COMP_KMXPLUS_HEADER*)loca)) {
    return false;
  }
  // TODO-LDML
  for(KMX_DWORD i=0; i<loca->count; i++) {
    KMXPLUS_PRINTF((" Locale #%d: #0x%X\n", i, loca->entries[i].locale));
  }
  return true;
}

static bool
validate_kmxplus_meta(const uint8_t* /*data*/, const COMP_KMXPLUS_META* meta) {
  if(meta == nullptr) {
    KMXPLUS_PRINTF(("! could not load 'meta' section\n"));
    return false;
  }
  if (!validate_kmxplus_header((const COMP_KMXPLUS_HEADER*)meta)) {
    return false;
  }
  KMXPLUS_PRINTF((" name:\t#0x%X\n", meta->name));
  KMXPLUS_PRINTF((" author:\t#0x%X\n", meta->author));
  KMXPLUS_PRINTF((" conform:\t#0x%X\n", meta->conform));
  KMXPLUS_PRINTF((" layout:\t#0x%X\n", meta->layout));
  KMXPLUS_PRINTF((" normalization:\t#0x%X\n", meta->normalization));
  KMXPLUS_PRINTF((" indicator:\t#0x%X\n", meta->indicator));
  KMXPLUS_PRINTF((" settings:\t0x%X\n", meta->settings));
  return true;
}

static bool
validate_kmxplus_vkey(const uint8_t* /*data*/, const COMP_KMXPLUS_VKEY* vkey) {
  if (vkey == nullptr) {
    KMXPLUS_PRINTF(("! could not load 'vkey' section\n"));
    return false;
  }
  if(!validate_kmxplus_header((const COMP_KMXPLUS_HEADER*)vkey)) {
    return false;
  }
  // TODO-LDML
  KMXPLUS_PRINTF(("TODO: dump vkey"));
  return true;
}

static bool
validate_kmxplus_strs(const uint8_t* /*data*/, const COMP_KMXPLUS_STRS* strs) {
  if (strs == nullptr) {
    KMXPLUS_PRINTF(("! could not load 'strs' section\n"));
    return false;
  }
  if(!validate_kmxplus_header((const COMP_KMXPLUS_HEADER*)strs)) {
    return false;
  }
  KMXPLUS_PRINTF(("strs: count 0x%X\n", strs->count));
  for (KMX_DWORD i=0; i<strs->count; i++) {
    const size_t MYBUFSIZ = 256;
    KMX_WCHAR buf[MYBUFSIZ];
    KMXPLUS_PRINTF(("#0x%X: ", i));
    PKMX_WCHAR str = strs->get(i, buf, MYBUFSIZ);
    if (!str) {
        KMXPLUS_PRINTF(("NULL/ERR\n"));
        return false;
    }
    for(int j=0; str[j] && j<0x30; j++) {
        if (str[j] < 0x7F && str[j] > 0x20) {
            KMXPLUS_PRINTF(("%c", str[j]));
        } else {
            KMXPLUS_PRINTF((" U+%04X ", str[j]));
        }
    }
    KMXPLUS_PRINTF(("\n"));
  }
  return true;
}

static bool
validate_kmxplus_sect(const uint8_t* data, const COMP_KMXPLUS_SECT* sect) {
  if (sect == nullptr) {
    KMXPLUS_PRINTF(("! could not load 'sect' section\n"));
    return false;
  }
  if (!validate_kmxplus_header((const COMP_KMXPLUS_HEADER*)sect)) {
    return false;
  }
  KMXPLUS_PRINTF(("sect: total 0x%X\n", sect->total));
  KMXPLUS_PRINTF(("sect: count 0x%X\n", sect->count));

  bool overall_valid = true;

  for (KMX_DWORD i = 0; i < sect->count; i++) {
    const COMP_KMXPLUS_SECT_ENTRY& entry = sect->entries[i];
    if(!validate_section_name(entry.sect)) {
      KMXPLUS_PRINTF((" (invalid section name) \n"));
      return false;
    }
    KMXPLUS_PRINTF((" sect#%d: %X @ %X\n", i, entry.sect, entry.offset));
    const uint8_t* entrydata = (data+entry.offset);
    bool validity = false;
    switch(entry.sect) {
      case LDML_SECTIONID_KEYS:
          validity = validate_kmxplus_keys(data, as_kmxplus_keys(entrydata));
          break;
      case LDML_SECTIONID_LOCA:
          validity = validate_kmxplus_loca(data, as_kmxplus_loca(entrydata));
          break;
      case LDML_SECTIONID_META:
          validity = validate_kmxplus_meta(data, as_kmxplus_meta(entrydata));
          break;
      case LDML_SECTIONID_SECT:
          KMXPLUS_PRINTF(("! Cowardly refusing to dump invalid nested 'sect' section.\n"));
          validity = false;
          break;
      case LDML_SECTIONID_STRS:
          validity = validate_kmxplus_strs(data, as_kmxplus_strs(entrydata));
          break;
      case LDML_SECTIONID_VKEY:
          validity = validate_kmxplus_vkey(data, as_kmxplus_vkey(entrydata));
          break;
      default:
          KMXPLUS_PRINTF(("Unknown section %X", entry.sect));
          validity = false;
          break;
    }
    if (!validity) {
      overall_valid = false; // if any section is invalid
    }
  }
  return overall_valid;
}

bool
validate_kmxplus_data(const uint8_t* data) {
  const COMP_KMXPLUS_SECT* sect = as_kmxplus_sect(data);
  if (sect == nullptr) {
    KMXPLUS_PRINTF(("Err: 'sect' NULL from %p\n", data));
    return false;
  }
  return validate_kmxplus_sect(data, sect);
}

bool
validate_kmxplus_data(kmx::PCOMP_KEYBOARD keyboard) {
  KMXPLUS_PRINTF(("dump_kmxplus_data(): Got a PCOMP_KEYBOARD at %p\n", keyboard));
  if (!(keyboard->dwFlags & KF_KMXPLUS)) {
    KMXPLUS_PRINTF(("Err: flags KF_KMXPLUS not set\n"));
    return false;
  }
  const COMP_KEYBOARD_EX* ex = reinterpret_cast<const COMP_KEYBOARD_EX*>(keyboard);

  KMXPLUS_PRINTF(("KMXPlus offset 0x%X, KMXPlus size 0x%X\n", ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize));
  const uint8_t* rawdata = reinterpret_cast<const uint8_t*>(keyboard);
  return validate_kmxplus_data(rawdata + ex->kmxplus.dpKMXPlus);
}

const kmx::COMP_KMXPLUS_KEYS_ENTRY *COMP_KMXPLUS_KEYS::find(KMX_DWORD vkey, KMX_DWORD mod) const {
    // TODO-LDML: eventually, assume sorted order & binary search
    for (KMX_DWORD i=0; i<count; i++) {
        if(entries[i].vkey == vkey && entries[i].mod == mod) {
            return &entries[i];
        }
    }
    return nullptr;
}

KMX_DWORD COMP_KMXPLUS_SECT::find(KMX_DWORD ident) const {
  for (KMX_DWORD i = 0; i < count; i++) {
    if (ident == entries[i].sect) {
        return entries[i].offset;
    }
  }
  return 0;
}

PKMX_WCHAR
COMP_KMXPLUS_STRS::get(KMX_DWORD entry, PKMX_WCHAR buf, KMX_DWORD bufsiz) const {
    assert(entry < count);
    if (entry >= count) {
        return nullptr;
    }
    KMX_DWORD offset = entries[entry].offset;
    KMX_DWORD length = entries[entry].length;
    assert(bufsiz > (length+1)); // assert bufsiz big enough
    assert(offset+((length+1)*2) <= header.size); // assert not out of bounds
    const uint8_t* thisptr = reinterpret_cast<const uint8_t*>(this);
    const KMX_WCHAR* start = reinterpret_cast<const KMX_WCHAR*>(thisptr+offset);
    for(KMX_DWORD i=0;i<=length;i++) {
        buf[i] = start[i];
    }
    return buf;
}

}  // namespace kmx
}  // namespace kbp
}  // namespace km
