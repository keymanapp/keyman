/*
  Copyright:    © 2024 SIL International.
  Description:  Tests for normalization in the context API.
  Create Date:  3 May 2024
  Authors:      Steven R. Loomis
  History:      3 May 2024 - SRL - Initial implementation.
*/

#include <string>
#include <fstream>
#include <iostream>

// Ensure that ICU gets included even on wasm.
#define KMN_IN_LDML_TESTS

#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"

#include <test_assert.h>
#include "../emscripten_filesystem.h"

#include "core_icu.h"
#include <unicode/uversion.h>
#include <unicode/uchar.h>
#include "json.hpp"
#include "util_normalize.hpp"
#include "kmx/kmx_xstring.h"

#include <test_assert.h>
#include <test_color.h>

#ifdef assert_basic_equal
#undef assert_basic_equal
#endif
#define assert_basic_equal(actual, expected) { \
  if ((actual) != (expected)) { \
    std::cerr \
             << "Test failed at " << __FILE__ << ":" << __LINE__ << ":" \
             << std::endl \
             << "expected: " << (expected) << std::endl \
             << "actual:   " << (actual) << std::endl; \
    std::exit(EXIT_FAILURE); \
  } \
}

#ifdef __EMSCRIPTEN__
// Pull this in to verify versions
#include "util_normalize_table.h"
#endif

//-------------------------------------------------------------------------------------
// Unicode version tests
//-------------------------------------------------------------------------------------

std::string arg_path;

/**
 * Load a .json file into a json object
 * @param jsonpath path to the .json file
 * @returns json object
 */
nlohmann::json load_json(const km::core::path &jsonpath) {
  std::cout << "== " << __FUNCTION__ << " loading " << jsonpath << std::endl;
  std::ifstream json_file(jsonpath.native());
  if (!json_file) {
    std::cerr << "ERROR Could not load: " << jsonpath << std::endl;
    assert (json_file);
  }
  nlohmann::json data = nlohmann::json::parse(json_file);
  return data;
}

/** @returns the major version of 'ver', skipping initial '^'. empty on err */
std::string get_major(const std::string& ver) {
  assert(!ver.empty());
  auto start = 0;
  // skip leading '^'
  if (ver[start] == '^') {
    start++;
  }
  // find first '.'
  auto end = ver.find('.', start);
  assert(end != std::string::npos);
  return ver.substr(start, end - start);
}

/**
 *  @return the Unicode version from a Blocks.txt file, such as `15.1.0`
 */
std::string get_block_unicode_ver(const char *blocks_path) {
  std::cout << "= " << __FUNCTION__ << " load " << blocks_path << std::endl;
  // open Blocks.txt
  std::ifstream blocks_file(
      km::core::path(blocks_path).native());
  assert(blocks_file.good());
  std::string block_line;
  assert(std::getline(blocks_file, block_line));  // first line

  // The first line is something such as '# Blocks-15.1.0.txt'
  // We skip the prefix, and then stop before the suffix

  const std::string prefix = "# Blocks-";
  const std::string txt_suffix = ".txt";

  // find and skip the prefix - "15.1.0.txt"
  assert(block_line.length() > prefix.length());
  std::string result = block_line.substr(prefix.length()); // "15.1.0"

  // find and trim before the suffix
  auto txt_pos = result.find(txt_suffix, 0);
  assert(txt_pos != std::string::npos);
  result.resize(txt_pos);

  return result;
}

void test_unicode_versions(const nlohmann::json &versions, const nlohmann::json &package,
const std::string &block_unicode_ver) {
  std::cout << "== test: " << __FUNCTION__ << std::endl;

  // 'raw' versions
  const std::string cxx_icu_unicode(U_UNICODE_VERSION);
  const std::string cxx_icu(U_ICU_VERSION);
  const std::string node_engine(package["engines"]["node"].template get<std::string>());
  const std::string node_icu_unicode(versions["unicode"].template get<std::string>());
  const std::string node_icu(versions["icu"].template get<std::string>());
  const std::string node(versions["node"].template get<std::string>());

  // calculated versions
  const std::string block_ver_major        = get_major(block_unicode_ver);
  const std::string cxx_icu_major          = get_major(cxx_icu);
  const std::string cxx_icu_unicode_major  = get_major(cxx_icu_unicode);
  const std::string node_engine_major      = get_major(node_engine);
  const std::string node_icu_major         = get_major(node_icu);
  const std::string node_icu_unicode_major = get_major(node_icu_unicode);
  const std::string node_major             = get_major(node);

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
  assert(node_num >= node_engine_num);

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

void test_has_boundary_before() {
  std::cout << "= " << __FUNCTION__ << std::endl;
  std::cout << "I see we are on Emscripten / wasm! Now we will do some additional tests." << std::endl;
  std::string icu4c_unicode(U_UNICODE_VERSION), header_unicode(KM_HASBOUNDARYBEFORE_UNICODE_VERSION),
              icu4c_icu(U_ICU_VERSION), header_icu(KM_HASBOUNDARYBEFORE_ICU_VERSION);
  std::cout << "Unicode: " << U_UNICODE_VERSION << ", and from the table file: " << KM_HASBOUNDARYBEFORE_UNICODE_VERSION << std::endl;
  std::cout << "It would be very strange for these versions to be out of sync. Some sort of build or tool problem." << std::endl;
  assert_basic_equal(icu4c_unicode, header_unicode);
  assert_basic_equal(icu4c_icu, header_icu);

  std::cout << std::endl << "Now, let's make sure has_nfd_boundary_before() matches ICU." << std::endl;

  UErrorCode status           = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(status);
  UASSERT_SUCCESS(status);

  // now, test that hasBoundaryBefore is the same
  for (km_core_usv cp = 0; cp < km::core::kmx::Uni_MAX_CODEPOINT; cp++) {
    auto km_hbb = km::core::util::has_nfd_boundary_before(cp);
    auto icu_hbb = nfd->hasBoundaryBefore(cp);

    if (km_hbb != icu_hbb) {
      std::cerr << "Error: util_normalize_table.h said " << boolstr(km_hbb) << " but ICU said " << boolstr(icu_hbb) << " for "
                << "has_nfd_boundary_before(0x" << std::hex << cp << std::dec << ")" << std::endl;
    }
    assert(km_hbb == icu_hbb);
  }
  std::cout << "All OK!" << std::endl;
}
#endif

int test_all(const char *jsonpath, const char *packagepath, const char *blockspath) {
  std::cout << "= " << __FUNCTION__ << std::endl;

  // load the dump of node's process.versions which the meson.build file generated
  auto versions = load_json(km::core::path(jsonpath));
  assert(!versions.empty());

  // load our top level package.json
  auto package = load_json(km::core::path(packagepath));
  assert(!package.empty());

  const auto block_unicode_ver = get_block_unicode_ver(blockspath);

  test_unicode_versions(versions, package, block_unicode_ver);

#ifdef __EMSCRIPTEN__
  test_has_boundary_before();
#endif

  return EXIT_SUCCESS;
}

//-------------------------------------------------------------------------------------
// Launcher
//-------------------------------------------------------------------------------------

constexpr const auto help_str = "\
test_unicode [--color] nodeversions.json package.json Blocks.txt\n\
\n\
  --color         Force color output\n";

int error_args() {
  std::cerr << "test_unicode: Invalid arguments." << std::endl;
  std::cout << help_str;
  return EXIT_FAILURE;
}

int main(int argc, char *argv []) {
  int first_arg = 1;
  auto arg_color = argc > first_arg && std::string(argv[first_arg]) == "--color";
  if (arg_color) first_arg++;
  console_color::enabled = console_color::isaterminal() || arg_color;

  // Get the path of the current executable
  arg_path = argv[0];
  auto last = arg_path.find_last_of("/\\");
  if(last == std::string::npos) {
    std::cerr << "could not parse argv[0]: " << argv[0] << std::endl;
    return 1;
  }
  arg_path.resize(last+1);

#ifdef __EMSCRIPTEN__
  arg_path = get_wasm_file_path(arg_path);
#endif

  if (argc <= first_arg) {
    return error_args();
  }
  auto jsonpath = argv[first_arg++];

  if (argc <= first_arg) {
    return error_args();
  }
  auto packagepath = argv[first_arg++];

  if (argc <= first_arg) {
    return error_args();
  }
  auto blockspath = argv[first_arg++];

  int rc = test_all(jsonpath, packagepath, blockspath);

  return rc;
}
