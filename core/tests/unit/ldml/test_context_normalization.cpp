/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for normalization in the context API.
  Create Date:  15 Jan 2024
  Authors:      Marc Durdin
  History:      15 Jan 2024 - MCD - Initial implementation.
*/
#include <string>
#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"

#include <test_assert.h>
#include "../emscripten_filesystem.h"
#include "../load_kmx_file.hpp"

//-------------------------------------------------------------------------------------
// Context normalization tests
//-------------------------------------------------------------------------------------

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
};

km_core_keyboard * test_kb = nullptr;
km_core_state * test_state = nullptr;
std::string arg_path;

void teardown() {
  if(test_state) {
    km_core_state_dispose(test_state);
    test_state = nullptr;
  }
  if(test_kb) {
    km_core_keyboard_dispose(test_kb);
    test_kb = nullptr;
  }
}

void setup(const char *keyboard) {
  teardown();

  km::core::path path = km::core::path::join(arg_path, "keyboards", keyboard);
  auto blob = km::tests::load_kmx_file(path.native().c_str());
  try_status(km_core_keyboard_load_from_blob(path.stem().c_str(), blob.data(), blob.size(), &test_kb));
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
}

void debug_context(km_core_debug_context_type context_type) {
  auto context = km_core_state_context_debug(test_state, context_type);
  if(context_type == KM_CORE_DEBUG_CONTEXT_APP) {
    std::cout << "app context: " << context << std::endl;
  } else {
    std::cout << "cached context: " << context << std::endl;
  }
  km_core_cu_dispose(context);
}

bool is_identical_context(km_core_cu const *cached_context, km_core_debug_context_type context_type) {
  size_t buf_size;
  km_core_context_item * citems = nullptr;

  debug_context(context_type);

  if(context_type == KM_CORE_DEBUG_CONTEXT_APP) {
    try_status(km_core_context_get(km_core_state_app_context(test_state), &citems));
  } else {
    try_status(km_core_context_get(km_core_state_context(test_state), &citems));
  }
  try_status(context_items_to_utf16(citems, nullptr, &buf_size));
  km_core_cu* new_cached_context = new km_core_cu[buf_size];
  try_status(context_items_to_utf16(citems, new_cached_context, &buf_size));

  km_core_context_items_dispose(citems);

  bool result = std::u16string(cached_context) == new_cached_context;
  delete[] new_cached_context;
  return result;
}

void test_context_normalization_already_nfd() {
  km_core_cu const *app_context_nfd = u"A\u0300";
  setup("k_001_tiny.kmx");
  assert(km_core_state_context_set_if_needed(test_state, app_context_nfd) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(is_identical_context(app_context_nfd, KM_CORE_DEBUG_CONTEXT_APP));
  assert(is_identical_context(app_context_nfd, KM_CORE_DEBUG_CONTEXT_CACHED));
  teardown();
}

void test_context_normalization_basic() {
  km_core_cu const *application_context = u"This is a test À";
  km_core_cu const *cached_context =      u"This is a test A\u0300";
  setup("k_001_tiny.kmx");
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(is_identical_context(application_context, KM_CORE_DEBUG_CONTEXT_APP));
  assert(is_identical_context(cached_context, KM_CORE_DEBUG_CONTEXT_CACHED));
  teardown();
}

void test_context_normalization_hefty() {
                                          // Latin     Latin            "ṩ"                   "Å"                 Tirhuta U+114bc -> U+114B9 U+114B0
  km_core_cu const *application_context = u"À"       u"é̖"             u"\u1e69"              u"\u212b"           u"\U000114BC";
  km_core_cu const *cached_context =      u"A\u0300" u"e\u0316\u0301" u"\u0073\u0323\u0307"  u"\u0041\u030a"     u"\U000114B9\U000114B0";
  setup("k_001_tiny.kmx");
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(is_identical_context(application_context, KM_CORE_DEBUG_CONTEXT_APP));
  assert(is_identical_context(cached_context, KM_CORE_DEBUG_CONTEXT_CACHED));
  teardown();
}

void test_context_normalization_invalid_unicode() {
                                          // unpaired surrogate     illegal
  km_core_cu const application_context[] = { 0xDC01, 0x0020, 0x0020, 0xFFFF, 0x0000 };
  km_core_cu const cached_context[] =      { 0xDC01, 0x0020, 0x0020, 0xFFFF, 0x0000 };
  setup("k_001_tiny.kmx");
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(is_identical_context(application_context, KM_CORE_DEBUG_CONTEXT_APP));
  assert(is_identical_context(cached_context, KM_CORE_DEBUG_CONTEXT_CACHED));
  teardown();
}

void test_context_normalization_lone_trailing_surrogate() {
                                   // unpaired trail surrogate
  km_core_cu const application_context[] = { 0xDC01, 0x0020, 0x0020, 0x0000 };
  km_core_cu const cached_context[] = /* skipped*/ { 0x0020, 0x0020, 0x0000 };
  setup("k_001_tiny.kmx");
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(is_identical_context(application_context+1, KM_CORE_DEBUG_CONTEXT_APP)); // first code unit is skipped
  assert(is_identical_context(cached_context, KM_CORE_DEBUG_CONTEXT_CACHED));
  teardown();
}

void test_context_normalization() {
  test_context_normalization_already_nfd();
  test_context_normalization_basic();
  test_context_normalization_hefty();
  // TODO: see #10392 we need to strip illegal chars: test_context_normalization_invalid_unicode(); // -- unpaired surrogate, illegals
  test_context_normalization_lone_trailing_surrogate();
}

//-------------------------------------------------------------------------------------
// Launcher
//-------------------------------------------------------------------------------------

constexpr const auto help_str = "\
context_normalization [--color]\n\
\n\
  --color         Force color output\n";

int error_args() {
  std::cerr << "context_normalization: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int main(int argc, char *argv []) {
  auto arg_color = argc > 1 && std::string(argv[1]) == "--color";
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

  test_context_normalization();
}
