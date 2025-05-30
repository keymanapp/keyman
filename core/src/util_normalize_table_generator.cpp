/*
  Copyright:    © SIL International.
  Description:  Generator for util_normalize_table.h
  Create Date:  5 Jun 2024
  Authors:      Steven R. Loomis

  util_normalize_table.h is used under wasm by utilities in util_normalize.cpp to implement
  normalization functions without needing ICU4C linked.

  This generator is invoked automatically by meson as part of the build.
*/

#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"

#define KMN_NO_ICU 0 // we will need ICU..

#include "core_icu.h"

#include <iostream>
#include <string>
#include <vector>

#include <assert.h>

#include <unicode/uchar.h>



int
write_nfd_table() {
#ifndef __EMSCRIPTEN__
  std::cerr << "Note: This is unusual - this generator is usually only run under emscripten!" << std::endl;
#endif

  // We write to stdout instead of to a file to avoid dealing with the filesystem under emscripten.

  std::cerr << "Writing to stdout." << std::endl;

  // write preamble
  std::cout << "// GENERATED FILE: DO NOT EDIT" << std::endl;
  std::cout << "//" << std::endl;
  std::cout << "// util_normalize_table.h is generated by util_normalize_table_generator.cpp" << std::endl;
  std::cout << "// and used by util_normalize.cpp" << std::endl;
  std::cout << std::endl;
  std::cout << "#pragma once" << std::endl;
  std::cout << "#define KM_HASBOUNDARYBEFORE_UNICODE_VERSION \"" << U_UNICODE_VERSION << "\"" << std::endl;
  std::cout << "#define KM_HASBOUNDARYBEFORE_ICU_VERSION \"" << U_ICU_VERSION << "\"" << std::endl;
  std::cout << std::endl;
  // we're going to need an NFD normalizer
  UErrorCode status           = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(status);
  assert(U_SUCCESS(status));

  // collect the raw list of chars that do NOT have a boundary before them.
  std::vector<km_core_usv> noBoundary;
  for (km_core_usv ch = 0; ch < km::core::kmx::Uni_MAX_CODEPOINT; ch++) {
    bool bb = nfd->hasBoundaryBefore(ch);
    assert(!(ch == 0 && !bb)); // assert that we can use U+0000 as a terminator
    if (bb) continue; //only emit nonboundary
    noBoundary.push_back(ch);
  }

  // now, compress these into runs
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

  // finally, write out metadata and the runs themselves.
  std::cout << "#define km_noBoundaryBefore_entries " << runs.size() << "\n";

  std::cout << "static char32_t km_noBoundaryBefore[km_noBoundaryBefore_entries * 2 ] = {" << std::endl;

  std::cout << "/*   start codepoint,   count (inclusive),    ...range end */" << std::endl;

  for (auto i = runs.begin(); i < runs.end(); i++) {
    std::cout << "\t0x" << std::hex << i->first << std::dec << ",\t " << i->second << ", // ...0x" << std::hex << (i->first+i->second-1) << std::endl;
  }

  // termination
  std::cout << "};" << std::endl;
  std::cout << "// end" << std::endl;
  std::cerr << "Wrote " << runs.size() << " runs representing " << noBoundary.size() << " entries." << std::endl;
  return 0;
}

int
main(int /*argc*/, const char * /*argv*/[]) {
  write_nfd_table();
  return 0;
}
