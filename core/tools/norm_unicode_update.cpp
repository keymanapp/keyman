#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include "core_icu.h"

#include <fstream>
#include <iostream>
#include <string>

#include <assert.h>

#include <unicode/uchar.h>

#ifndef __EMSCRIPTEN__

int
write_nfd_table(const char *NFD_FILE) {
  std::cout << " writing: " << NFD_FILE << std::endl;
  auto f =  std::ofstream(NFD_FILE);
  assert(f.good());

  // write preamble
  f << "//NFD hasBoundaryBefore" << std::endl;
  f << "#pragma once" << std::endl;
  f << "#define KM_HASBOUNDARYBEFORE_UNICODE_VERSION \"" << U_UNICODE_VERSION << "\"" << std::endl;
  f << "#define KM_HASBOUNDARYBEFORE_ICU_VERSION \"" << U_ICU_VERSION << "\"" << std::endl;
  f << "static char32_t km_noBoundaryBefore[] = {" << std::endl;
  // we're going to need an NFD normalizer
  UErrorCode status           = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(status);
  assert(U_SUCCESS(status));

  for (km_core_usv ch = 0; ch < 0x10FFFF; ch++) {
    bool bb = nfd->hasBoundaryBefore(ch);
    assert(!(ch == 0 && !bb)); // assert that we can use U+0000 as a terminator

    // TODO: This test may be better in test_unicode
    // icu::UnicodeString s;
    // s.append((UChar32)ch);
    // bool lccc = nfd->isNormalized(s, status) && u_getCombiningClass(ch) == 0;
    // assert(U_SUCCESS(status));
    // if (bb != lccc) {
    //   printf("0x%04x - bb=%s but lccc=%s\n", (unsigned int)ch, bb ? "y" : "n", lccc ? "y" : "n");
    // }
    // assert(bb == lccc);
    if (bb) continue; //only emit nonboundary
    // char key[10];
    // snprintf(key, 10, "%04X", (unsigned int)ch);
    f << "\t0x" << std::hex << ch << "," << std::endl;
  }
  // termination
  f << "\t0x" << std::hex << 0 << "," << std::endl;
  f << "};" << std::endl;
  return 0;
}

int
main(int argc, const char *argv[]) {
  assert(argc == 2); // call with one param: @OUTPUT@
  write_nfd_table(argv[1]);
  return 0;
}
#else
int main(int argc, const char *argv[]) {
  std::cerr << "Can't run this under Emscripten - run under another platform." << std::endl;
  return 1;
}
#endif
