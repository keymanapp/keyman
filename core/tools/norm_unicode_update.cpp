#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include "core_icu.h"

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

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
  // we're going to need an NFD normalizer
  UErrorCode status           = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(status);
  assert(U_SUCCESS(status));

  std::vector<km_core_usv> noBoundary;

  for (km_core_usv ch = 0; ch < 0x10FFFF; ch++) {
    bool bb = nfd->hasBoundaryBefore(ch);
    assert(!(ch == 0 && !bb)); // assert that we can use U+0000 as a terminator
    if (bb) continue; //only emit nonboundary
    noBoundary.push_back(ch);
  }

  std::vector<std::pair<km_core_usv,std::size_t>> runs; // start,len

  km_core_usv first = 0;
  km_core_usv last = 0;
  for(auto i = noBoundary.begin(); i <= noBoundary.end(); i++) {
    if (first == 0) {
      first = last = *i;
    } else {
      last++;
      if(i == noBoundary.end() || *i != last) {
        // end of a run
        runs.emplace_back(first, last - first);
        if (i != noBoundary.end()) {
          // setup for next
          first = last = *i;
        }
      }
    }
  }
  f << "#define km_noBoundaryBefore_entries " << runs.size() << "\n";

  f << "static char32_t km_noBoundaryBefore[km_noBoundaryBefore_entries * 2 ] = {" << std::endl;

  for (auto i = runs.begin(); i < runs.end(); i++) {
    f << "\t0x" << std::hex << i->first << std::dec << ",\t " << i->second << ", // ...0x" << std::hex << (i->first+i->second-1) << std::endl;
  }

  // termination
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
