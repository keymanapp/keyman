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

#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"

#include <test_assert.h>
#include "../emscripten_filesystem.h"

#include "core_icu.h"
#include <unicode/uversion.h>
#include <unicode/uchar.h>
#include "json.hpp"

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

//-------------------------------------------------------------------------------------
// Unicode version tests
//-------------------------------------------------------------------------------------

std::string arg_path;

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

/** @return the Unicode version from a Blocks.txt file */
std::string get_block_unicode_ver(const char *blocks_path) {
  std::cout << "= " << __FUNCTION__ << " load " << blocks_path << std::endl;
  // fetch Blocks.txt
  std::ifstream blocks_file(
      km::core::path(blocks_path).native());
  assert(blocks_file.good());
  std::string block_line;
  assert(std::getline(blocks_file, block_line));  // first line
  const std::string prefix = "# Blocks-";
  assert(block_line.length() > prefix.length());
  return block_line.substr(prefix.length());
}

void test_unicode_versions(const nlohmann::json &versions, const nlohmann::json &package,
const std::string &block_unicode_ver) {
  std::cout << "== test: " << __FUNCTION__ << std::endl;

#define SHOW_VAR(x) (std::cout << #x << "\t" << (x) << std::endl)

  const std::string cxx_icu(U_ICU_VERSION);
  SHOW_VAR(cxx_icu);

  const std::string cxx_icu_unicode(U_UNICODE_VERSION);
  SHOW_VAR(cxx_icu_unicode);

  SHOW_VAR(versions);

  const std::string node_icu_unicode(versions["unicode"].template get<std::string>());
  SHOW_VAR(node_icu_unicode);
  SHOW_VAR(versions["node"]);

  const std::string node(versions["node"].template get<std::string>());
  SHOW_VAR(node);

  const std::string node_icu(versions["icu"].template get<std::string>());
  SHOW_VAR(node_icu);

  const std::string node_engine(package["engines"]["node"].template get<std::string>());
  SHOW_VAR(node_engine);

  std::cout << "=== Loaded from JSON" << std::endl;

  SHOW_VAR(block_unicode_ver);
  std::cout << "=== calculating major versions" << std::endl;

  // calculations
  auto block_ver_major        = get_major(block_unicode_ver);
  auto node_engine_major      = get_major(node_engine);
  auto node_major             = get_major(node);
  auto cxx_icu_major          = get_major(cxx_icu);
  auto node_icu_major         = get_major(node_icu);
  auto cxx_icu_unicode_major  = get_major(cxx_icu_unicode);
  auto node_icu_unicode_major = get_major(node_icu_unicode);

#undef SHOW_VAR

  // allow the Node.js version to be >= required
  auto node_engine_num = std::atoi(node_engine_major.c_str());
  auto node_num        = std::atoi(node_major.c_str());
  assert(node_num >= node_engine_num);

  // the cxx_icu can come from the Ubuntu environment, so do not depend on it
  // for now.
  //assert_basic_equal(node_icu_unicode_major, cxx_icu_unicode_major);
  assert_basic_equal(node_icu_unicode_major, block_ver_major);

  // seems less important if the C++ ICU verison matches the Node.js ICU version.
  //assert_basic_equal(cxx_icu_major, node_icu_major);

  std::cout << "All OK!" << std::endl;

  std::cout << std::endl;
}

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
