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
dump_kmxplus_sect(const uint8_t* /*data*/, const COMP_KMXPLUS_SECT* sect) {
  dump_kmxplus_header((const COMP_KMXPLUS_HEADER*)sect);
  printf("sect: total 0x%X\n", sect->total);
  printf("sect: count 0x%X\n", sect->count);

  for (KMX_DWORD i = 0; i < sect->count; i++) {
    dump_section_name(sect->entries[i].sect);
    printf(" sect#%d: %X @ %X\n", i, sect->entries[i].sect, sect->entries[i].offset);
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

}  // namespace kmx
}  // namespace kbp
}  // namespace km
