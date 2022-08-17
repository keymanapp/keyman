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
  dump_section_name(hdr->ident);
  printf(": (%X) size 0x%X\n", hdr->ident, hdr->size);
}

static void
dump_kmxplus_strs(const uint8_t* /*data*/, const COMP_KMXPLUS_STRS* strs) {
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)strs);
  printf("strs: count 0x%X\n", strs->count);
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

    switch(entry.sect) {
        case LDML_SECTION_STRS:
            dump_kmxplus_strs(data, as_kmxplus_strs((void*)(data+entry.offset)));
            break;
        default:
            ;
    }
  }
}

void
dump_kmxplus_data(const uint8_t* data) {
  const COMP_KMXPLUS_SECT* sect = as_kmxplus_sect((void*)data);
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
COMP_KMXPLUS_STRS::get(KMX_DWORD entry, PKMX_WCHAR buf, KMX_DWORD bufsiz) {
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
