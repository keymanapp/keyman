/*
  Copyright:        Copyright (C) 2022 SIL International.
  Authors:          srl295
  Implementation for the KMX Plus utilities
*/

#include <kmx/kmx_plus.h>

/**
 * @def KMXPLUS_DEBUG Set to 1 to enable debug output
 */
#define KMXPLUS_DEBUG 1

#if KMXPLUS_DEBUG
#include <stdio.h>
#endif

#include <assert.h>

namespace km {
namespace kbp {
namespace kmx {

#if KMXPLUS_DEBUG
static void
dump_section_name(KMX_DWORD ident) {
  for (int i = 0; i < 4; i++) {
    putchar((ident & 0xFF000000) >> 24);
    ident <<= 8;
  }
}

static void
dump_kmxplus_header(const COMP_KMXPLUS_HEADER* hdr) {
  if (hdr == nullptr) {
    printf("! dump_kmxplus_header: NULL header\n");
    return;
  }
  dump_section_name(hdr->ident);
  printf(": (%X) size 0x%X\n", hdr->ident, hdr->size);
}

static void
dump_kmxplus_keys(const uint8_t* /*data*/, const COMP_KMXPLUS_KEYS* keys) {
  if(keys == nullptr) {
    printf("! could not load 'keys' section\n");
    return;
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)keys);
  printf(" count: #0x%X\n", keys->count);
  for (KMX_DWORD i = 0; i<keys->count; i++) {
    printf(" #0x%d\n", i);
    const COMP_KMXPLUS_KEYS_ENTRY& entry = keys->entries[i];
    printf("  vkey\t0x%X\n", entry.vkey);
    printf("  mod\t0x%X\n", entry.mod);
    printf("  flags\t0x%X\n", entry.flags);
    if (entry.flags & LDML_KEYS_FLAGS_EXTEND) {
        printf("  \t Extend: String #0x%X\n", entry.to);
    } else {
        printf("  \t UTF-32:U+%04X\n", entry.to);
    }
  }
}

static void
dump_kmxplus_loca(const uint8_t* /*data*/, const COMP_KMXPLUS_LOCA* loca) {
  if(loca == nullptr) {
    printf("! could not load 'loca' section\n");
    return;
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)loca);
  // TODO-LDML
  for(KMX_DWORD i=0; i<loca->count; i++) {
    printf(" Locale #%d: #0x%X\n", i, loca->entries[i].locale);
  }
}

static void
dump_kmxplus_meta(const uint8_t* /*data*/, const COMP_KMXPLUS_META* meta) {
  if(meta == nullptr) {
    printf("! could not load 'meta' section\n");
    return;
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)meta);
  printf(" name:\t#0x%X\n", meta->name);
  printf(" author:\t#0x%X\n", meta->author);
  printf(" conform:\t#0x%X\n", meta->conform);
  printf(" layout:\t#0x%X\n", meta->layout);
  printf(" normalization:\t#0x%X\n", meta->normalization);
  printf(" indicator:\t#0x%X\n", meta->indicator);
  printf(" settings:\t0x%X\n", meta->settings);
}

static void
dump_kmxplus_vkey(const uint8_t* /*data*/, const COMP_KMXPLUS_VKEY* vkey) {
  if (vkey == nullptr) {
    printf("! could not load 'vkey' section\n");
    return;
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)vkey);
  // TODO-LDML
}

static void
dump_kmxplus_strs(const uint8_t* /*data*/, const COMP_KMXPLUS_STRS* strs) {
  if (strs == nullptr) {
    printf("! could not load 'strs' section\n");
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)strs);
  printf("strs: count 0x%X\n", strs->count);
  for (KMX_DWORD i=0; i<strs->count; i++) {
    KMX_WCHAR buf[BUFSIZ];
    printf("#0x%X: ", i);
    PKMX_WCHAR str = strs->get(i, buf, BUFSIZ);
    if (!str) {
        printf("NULL/ERR\n");
        continue;
    }
    for(int j=0; str[j] && j<0x30; j++) {
        if (str[j] < 0x7F && str[j] > 0x20) {
            putchar(str[j]);
        } else {
            printf(" U+%04X ", str[j]);
        }
    }
    printf("\n");
  }
}

static void
dump_kmxplus_sect(const uint8_t* data, const COMP_KMXPLUS_SECT* sect) {
  if (sect == nullptr) {
    printf("! could not load 'sect' section\n");
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)sect);
  printf("sect: total 0x%X\n", sect->total);
  printf("sect: count 0x%X\n", sect->count);

  for (KMX_DWORD i = 0; i < sect->count; i++) {
    const COMP_KMXPLUS_SECT_ENTRY& entry = sect->entries[i];
    dump_section_name(entry.sect);
    printf(" sect#%d: %X @ %X\n", i, entry.sect, entry.offset);
    const uint8_t* entrydata = (data+entry.offset);
    switch(entry.sect) {
        case LDML_SECTION_KEYS:
            dump_kmxplus_keys(data, as_kmxplus_keys(entrydata));
            break;
        case LDML_SECTION_LOCA:
            dump_kmxplus_loca(data, as_kmxplus_loca(entrydata));
            break;
        case LDML_SECTION_META:
            dump_kmxplus_meta(data, as_kmxplus_meta(entrydata));
            break;
        case LDML_SECTION_SECT:
            printf("! Cowardly refusing to dump nested 'sect' section.\n");
            break;
        case LDML_SECTION_STRS:
            dump_kmxplus_strs(data, as_kmxplus_strs(entrydata));
            break;
        case LDML_SECTION_VKEY:
            dump_kmxplus_vkey(data, as_kmxplus_vkey(entrydata));
            break;
        default:
            printf("Unknown section %X", entry.sect);
    }
  }
}
#endif

void
dump_kmxplus_data(const uint8_t* data) {
#if KMXPLUS_DEBUG
  const COMP_KMXPLUS_SECT* sect = as_kmxplus_sect(data);
  if (sect == nullptr) {
    printf("Err: 'sect' NULL from %p\n", data);
    return;
  }
  dump_kmxplus_sect(data, sect);
#endif
}

void
dump_kmxplus_data(kmx::PCOMP_KEYBOARD keyboard) {
#if KMXPLUS_DEBUG
  printf("dump_kmxplus_data(): Got a PCOMP_KEYBOARD at %p\n", keyboard);
  if (!(keyboard->dwFlags & KF_KMXPLUS)) {
    printf("Err: flags KF_KMXPLUS not set\n");
    return;
  }
  const COMP_KEYBOARD_EX* ex = reinterpret_cast<const COMP_KEYBOARD_EX*>(keyboard);

  printf("KMXPlus offset 0x%X, KMXPlus size 0x%X\n", ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize);
  const uint8_t* rawdata = reinterpret_cast<const uint8_t*>(keyboard);
  dump_kmxplus_data(rawdata + ex->kmxplus.dpKMXPlus);
#endif
}

const COMP_KMXPLUS_KEYS_ENTRY *COMP_KMXPLUS_KEYS::find(KMX_DWORD vkey, KMX_DWORD mod) const {
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
