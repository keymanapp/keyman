#include <kmx/kmx_plus.h>
#include <stdio.h>

namespace km {
namespace kbp {
namespace kmx {

static void
dump_section_name(KMX_DWORD ident) {
  for (int i = 0; i < 4; i++) {
    putchar((ident & 0xFF000000) >> 24);
    ident <<= 8;
  }
}

static void
dump_kmxplus_header(const COMP_KMXPLUS_HEADER* hdr) {
  if (hdr == NULL) {
    printf("! dump_kmxplus_header: NULL header\n");
  }
  dump_section_name(hdr->ident);
  printf(": (%X) size 0x%X\n", hdr->ident, hdr->size);
}

static void
dump_kmxplus_keys(const uint8_t* /*data*/, const COMP_KMXPLUS_KEYS* keys) {
  if(keys == NULL) {
    printf("! could not load 'keys' section\n");
    return;
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)keys);
  // TODO-LDML
}

static void
dump_kmxplus_loca(const uint8_t* /*data*/, const COMP_KMXPLUS_LOCA* loca) {
  if(loca == NULL) {
    printf("! could not load 'loca' section\n");
    return;
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)loca);
  // TODO-LDML
}

static void
dump_kmxplus_meta(const uint8_t* /*data*/, const COMP_KMXPLUS_META* meta) {
  if(meta == NULL) {
    printf("! could not load 'meta' section\n");
    return;
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)meta);
  // TODO-LDML
}

static void
dump_kmxplus_vkey(const uint8_t* /*data*/, const COMP_KMXPLUS_VKEY* vkey) {
  if(vkey == NULL) {
    printf("! could not load 'vkey' section\n");
    return;
  }
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)vkey);
  // TODO-LDML
}



static void
dump_kmxplus_strs(const uint8_t* /*data*/, const COMP_KMXPLUS_STRS* strs) {
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
        if (str[j] < 0x7F && str[j] != 0x0020 && str[j] > 0x20) {
            putchar(str[j]);
        } else {
            printf("U+%04X ", str[j]);
        }
    }
    printf("\n");
  }
}

static void
dump_kmxplus_sect(const uint8_t* data, const COMP_KMXPLUS_SECT* sect) {
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

void
dump_kmxplus_data(const uint8_t* data) {
  const COMP_KMXPLUS_SECT* sect = as_kmxplus_sect(data);
  if (sect == NULL) {
    printf("Err: 'sect' null from %p\n", data);
    return;
  }
  dump_kmxplus_sect(data, sect);
}

void
dump_kmxplus_data(kmx::PCOMP_KEYBOARD keyboard) {
  printf("dump_kmxplus_data(): Got a PCOMP_KEYBOARD at %p\n", keyboard);
  if (!(keyboard->dwFlags & KF_KMXPLUS)) {
    printf("Err: flags KF_KMXPLUS not set\n");
    return;
  }
  const COMP_KEYBOARD_EX* ex = reinterpret_cast<const COMP_KEYBOARD_EX*>(keyboard);

  printf("KMXPlus offset 0x%X, KMXPlus size 0x%X\n", ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize);
  const uint8_t* rawdata = reinterpret_cast<const uint8_t*>(keyboard);
  dump_kmxplus_data(rawdata + ex->kmxplus.dpKMXPlus);
}

 PKMX_WCHAR
COMP_KMXPLUS_STRS::get(KMX_DWORD entry, PKMX_WCHAR buf, KMX_DWORD bufsiz) const {
    assert(entry < count);
    if (entry >= count) {
        return NULL;
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
