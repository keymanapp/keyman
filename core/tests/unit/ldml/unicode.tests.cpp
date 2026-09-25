/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Steven R. Loomis on 2024-05-03
 *
 * Keyman Core - Tests for versions of Unicode resources
 */

#include <string>
#include <fstream>
#include <iostream>

// Ensure that ICU gets included even on wasm.
#define KMN_IN_LDML_TESTS

#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"
#include "json.hpp"

#include "../helpers/core_test_helpers.h"

#include "core_icu.h"
#include <unicode/uversion.h>
#include <unicode/uchar.h>
#include "util_normalize.hpp"
#include "kmx/kmx_xstring.h"

#ifdef __EMSCRIPTEN__
// Pull this in to verify versions
#include "util_normalize_table.h"
#endif

//-------------------------------------------------------------------------------------
// Unicode version tests
//-------------------------------------------------------------------------------------

/**
 * Load a .json file into a json object
 * @param jsonpath path to the .json file
 * @param out json object
 */
bool load_json(const km::core::path &jsonpath, nlohmann::json &out) {
  std::cout << "== " << __FUNCTION__ << " loading " << jsonpath << std::endl;
  std::ifstream json_file(jsonpath.native());
  if (!json_file) {
    std::cerr << "ERROR Could not load: " << jsonpath << std::endl;
    return false;
  }
  out = nlohmann::json::parse(json_file);
  return !out.empty();
}

/** @param out the major version of 'ver', skipping initial '^'. empty on err */
void get_major(const std::string& ver, std::string& out) {
  ASSERT_FALSE(ver.empty());
  auto start = 0;
  // skip leading '^'
  if (ver[start] == '^') {
    start++;
  }
  // find first '.'
  auto end = ver.find('.', start);
  ASSERT_NE(end, std::string::npos);
  out = ver.substr(start, end - start);
}

/**
 *  @return the Unicode version from a Blocks.txt file, such as `15.1.0`
 */
void get_block_unicode_ver(km::core::path const & blocks_path, std::string& result) {
  // open Blocks.txt
  std::ifstream blocks_file(blocks_path.native());
  ASSERT_TRUE(blocks_file.good());
  std::string block_line;
  ASSERT_TRUE(std::getline(blocks_file, block_line));  // first line

  // The first line is something such as '# Blocks-15.1.0.txt'
  // We skip the prefix, and then stop before the suffix

  const std::string prefix = "# Blocks-";
  const std::string txt_suffix = ".txt";

  // find and skip the prefix - "15.1.0.txt"
  ASSERT_TRUE(block_line.length() > prefix.length());
  result = block_line.substr(prefix.length()); // "15.1.0"

  // find and trim before the suffix
  auto txt_pos = result.find(txt_suffix, 0);
  ASSERT_NE(txt_pos, std::string::npos);
  result.resize(txt_pos);
}

TEST(UnicodeTests, TestUnicodeVersions) {

  km::core::path jsonpath = test_dir / "nodeversions.json";
  km::core::path packagepath = test_dir / "package.json";
  km::core::path blockspath = test_dir / "Blocks.txt";

  // load the dump of node's process.versions which the meson.build file generated
  nlohmann::json versions, package;
  ASSERT_TRUE(load_json(jsonpath, versions));

  // load our top level package.json
  ASSERT_TRUE(load_json(packagepath, package));

  std::string block_unicode_ver;
  ASSERT_NO_FATAL_FAILURE(get_block_unicode_ver(blockspath, block_unicode_ver));

  // 'raw' versions
  const std::string cxx_icu_unicode(U_UNICODE_VERSION);
  const std::string cxx_icu(U_ICU_VERSION);
  const std::string node_engine(package["engines"]["node"].template get<std::string>());
  const std::string node_icu_unicode(versions["unicode"].template get<std::string>());
  const std::string node_icu(versions["icu"].template get<std::string>());
  const std::string node(versions["node"].template get<std::string>());

  // calculated versions
  std::string block_ver_major        ; ASSERT_NO_FATAL_FAILURE(get_major(block_unicode_ver, block_ver_major));
  std::string cxx_icu_major          ; ASSERT_NO_FATAL_FAILURE(get_major(cxx_icu, cxx_icu_major));
  std::string cxx_icu_unicode_major  ; ASSERT_NO_FATAL_FAILURE(get_major(cxx_icu_unicode, cxx_icu_unicode_major));
  std::string node_engine_major      ; ASSERT_NO_FATAL_FAILURE(get_major(node_engine, node_engine_major));
  std::string node_icu_major         ; ASSERT_NO_FATAL_FAILURE(get_major(node_icu, node_icu_major));
  std::string node_icu_unicode_major ; ASSERT_NO_FATAL_FAILURE(get_major(node_icu_unicode, node_icu_unicode_major));
  std::string node_major             ; ASSERT_NO_FATAL_FAILURE(get_major(node, node_major));

  // macro to output string value

  std::cout << "ICU Versions:" << std::endl;
  std::cout << "* " << cxx_icu << "\t"
            << "..linked from C++" << std::endl;
  std::cout << "* " << node_icu << "\t"
            << "..in Node.js" << std::endl;
  std::cout << std::endl;

  std::cout << "Unicode Versions:" << std::endl;
  std::cout << "* " << cxx_icu_unicode << "\t"
            << "..in ICU linked from C++" << std::endl;
  std::cout << "* " << node_icu_unicode << "\t"
            << "..in ICU in Node.js" << std::endl;
  std::cout << "* " << block_unicode_ver << "\t"
            << "..in Keyman repo Blocks.txt" << std::endl;
  std::cout << std::endl;

  std::cout << "Node.js" << std::endl;
  std::cout << "* " << versions["node"] << "\t"
            << "Actual version of Node.js" << std::endl;
  std::cout << "* " << node_engine << "\t"
            << "Version of Node.js requested by package.json" << std::endl;
  std::cout << std::endl;

  // ---- tests ------

  // allow the Node.js version to be >= required
  auto node_engine_num = std::atoi(node_engine_major.c_str());
  auto node_num        = std::atoi(node_major.c_str());
  ASSERT_GE(node_num, node_engine_num);

  // the cxx_icu can come from the Ubuntu environment, so do not depend on it
  // for now.
  // TODO: Resolve with ICU4C 76 in #12398
  //assert_basic_equal(node_icu_unicode_major, cxx_icu_unicode_major);
  //assert_basic_equal(node_icu_unicode_major, block_ver_major);

  // seems less important if the C++ ICU verison matches the Node.js ICU version.
  //assert_basic_equal(cxx_icu_major, node_icu_major);

  std::cout << "All OK!" << std::endl;

  std::cout << std::endl;
}

#ifdef __EMSCRIPTEN__
inline const char *boolstr(bool b) {
  return b?"T":"f";
}
#endif

TEST(UnicodeTests, TestHasBoundaryBefore) {
  std::cout << "= " << __FUNCTION__ << std::endl;

  // Latin - #15505
  ASSERT_TRUE(km::core::util::has_nfc_boundary_before(0x0065));
  ASSERT_FALSE(km::core::util::has_nfc_boundary_before(0x0301));

  // Bengali - #15505
  ASSERT_TRUE(km::core::util::has_nfc_boundary_before(0x0995));
  ASSERT_FALSE(km::core::util::has_nfc_boundary_before(0x09d7));

#ifdef __EMSCRIPTEN__
  std::cout << "I see we are on Emscripten / wasm! Now we will do some additional tests." << std::endl;
  std::string icu4c_unicode(U_UNICODE_VERSION), header_unicode(KM_HASBOUNDARYBEFORE_UNICODE_VERSION),
              icu4c_icu(U_ICU_VERSION), header_icu(KM_HASBOUNDARYBEFORE_ICU_VERSION);
  std::cout << "Unicode: " << U_UNICODE_VERSION << ", and from the table file: " << KM_HASBOUNDARYBEFORE_UNICODE_VERSION << std::endl;
  std::cout << "It would be very strange for these versions to be out of sync. Some sort of build or tool problem." << std::endl;
  ASSERT_EQ(icu4c_unicode, header_unicode);
  ASSERT_EQ(icu4c_icu, header_icu);

  std::cout << std::endl << "Now, let's make sure has_nfc_boundary_before() matches ICU." << std::endl;

  UErrorCode status           = U_ZERO_ERROR;
  const icu::Normalizer2 *nfc = icu::Normalizer2::getNFCInstance(status);
  UASSERT_SUCCESS(status);

  // now, test that hasBoundaryBefore is the same
  for (km_core_usv cp = 0; cp < km::core::kmx::Uni_MAX_CODEPOINT; cp++) {
    auto km_hbb = km::core::util::has_nfc_boundary_before(cp);
    auto icu_hbb = nfc->hasBoundaryBefore(cp);

    if (km_hbb != icu_hbb) {
      std::cerr << "Error: util_normalize_table.h said " << boolstr(km_hbb) << " but ICU said " << boolstr(icu_hbb) << " for "
                << "has_nfc_boundary_before(0x" << std::hex << cp << std::dec << ")" << std::endl;
    }
    ASSERT_EQ(km_hbb, icu_hbb);
  }
#endif
  std::cout << "All OK!" << std::endl;
}
