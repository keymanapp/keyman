// Tests for the state_context_* API methods

#include <keyman/keyman_core_api.h>
#include <string>

#include "path.hpp"

#include "../emscripten_filesystem.h"
#include <test_assert.h>

//-------------------------------------------------------------------------------------
// Context tests
//-------------------------------------------------------------------------------------

km_core_option_item test_env_opts[] = {KM_CORE_OPTIONS_END};

km_core_keyboard *test_kb    = nullptr;
km_core_state *test_state    = nullptr;
km_core_context_item *citems = nullptr;
std::string arg_path;

void
teardown() {
  if (citems) {
    km_core_context_items_dispose(citems);
    citems = nullptr;
  }
  if (test_state) {
    km_core_state_dispose(test_state);
    test_state = nullptr;
  }
  if (test_kb) {
    km_core_keyboard_dispose(test_kb);
    test_kb = nullptr;
  }
}

void
setup(const char *keyboard, const km_core_cp *context) {
  teardown();

  km::core::path path = km::core::path::join(arg_path, keyboard);
  try_status(km_core_keyboard_load(path.native().c_str(), &test_kb));
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_core_context_items_from_utf16(context, &citems));
  try_status(km_core_context_set(km_core_state_context(test_state), citems));
}

bool
is_identical_context(km_core_cp const *cached_context) {
  size_t buf_size;
  try_status(km_core_context_get(km_core_state_context(test_state), &citems));
  try_status(km_core_context_items_to_utf16(citems, nullptr, &buf_size));
  km_core_cp *new_cached_context = new km_core_cp[buf_size];
  try_status(km_core_context_items_to_utf16(citems, new_cached_context, &buf_size));
  bool result = std::u16string(cached_context) == new_cached_context;
  delete[] new_cached_context;
  return result;
}

void
test_context_set_if_needed_identical_context() {
  km_core_cp const *application_context = u"This is a test";
  km_core_cp const *cached_context      = u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UNCHANGED);
  assert(is_identical_context(cached_context));
  teardown();
}

void
test_context_set_if_needed_different_context() {
  km_core_cp const *application_context = u"This is a    test";
  km_core_cp const *cached_context      = u"This isn't a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(application_context));
  teardown();
}

void
test_context_set_if_needed_cached_context_cleared() {
  km_core_cp const *application_context = u"This is a test";
  km_core_cp const *cached_context      = u"";
  setup("k_000___null_keyboard.kmx", cached_context);
  km_core_state_context_clear(test_state);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(application_context));
  teardown();
}

void
test_context_set_if_needed_application_context_empty() {
  km_core_cp const *application_context = u"";
  km_core_cp const *cached_context      = u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(application_context));
  teardown();
}

void
test_context_set_if_needed_app_context_is_longer() {
  km_core_cp const *application_context = u"Longer This is a test";
  km_core_cp const *cached_context      = u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UNCHANGED);
  // Should be true -- longer, but what exists is identical to cached
  assert(is_identical_context(cached_context));
  teardown();
}

void
test_context_set_if_needed_app_context_is_shorter() {
  km_core_cp const *application_context = u"is a test";
  km_core_cp const *cached_context      = u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UPDATED);
  // Should be false -- app ctxt is shorter, so doesn't matter that what we have
  // matches
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(application_context));
  teardown();
}

void
test_context_set_if_needed_cached_context_has_markers() {
  km_core_cp const *application_context = u"123";
  km_core_cp const *cached_context      = u"123";
  setup("k_000___null_keyboard.kmx", cached_context);

  km_core_context_item const citems[] = {
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};

  try_status(km_core_context_set(km_core_state_context(test_state), citems));
  assert(km_core_state_context_set_if_needed(test_state, application_context) == KM_CORE_CONTEXT_STATUS_UNCHANGED);

  km_core_context_item *citems_new;

  try_status(km_core_context_get(km_core_state_context(test_state), &citems_new));

  for (int i = 0; citems[i].type || citems_new[i].type; i++) {
    assert(citems_new[i].type == citems[i].type);
    if (citems[i].type == KM_CORE_CT_CHAR) {
      assert(citems_new[i].character == citems[i].character);
    } else {
      assert(citems_new[i].marker == citems[i].marker);
    }
  }

  teardown();
}

void
test_context_set_if_needed() {
  test_context_set_if_needed_identical_context();
  test_context_set_if_needed_different_context();
  test_context_set_if_needed_cached_context_cleared();
  test_context_set_if_needed_application_context_empty();
  test_context_set_if_needed_app_context_is_longer();
  test_context_set_if_needed_app_context_is_shorter();
  test_context_set_if_needed_cached_context_has_markers();
}

void
test_context_clear() {
  km_core_cp const *cached_context = u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  try_status(km_core_state_context_clear(test_state));
  assert(!is_identical_context(cached_context));
  assert(is_identical_context(u""));
  teardown();
}

//-------------------------------------------------------------------------------------

void test_context_debug_empty() {
  km_core_cp const *cached_context =      u"";
  setup("k_000___null_keyboard.kmx", cached_context);
  auto str = km_core_state_context_debug(test_state, KM_CORE_DEBUG_CONTEXT_CACHED);
  // std::cout << str << std::endl;
  assert(std::u16string(str) == u"|| (len: 0) [ ]");
  km_core_cp_dispose(str);
}

void test_context_debug_various() {
  km_core_cp const *cached_context =      u"";
  setup("k_000___null_keyboard.kmx", cached_context);

  km_core_context_item const citems[] = {
    { KM_CORE_CT_MARKER, {0}, { 5 } },
    { KM_CORE_CT_CHAR, {0}, { '1' } },
    { KM_CORE_CT_MARKER, {0}, { 1 } },
    { KM_CORE_CT_CHAR, {0}, { '2' } },
    { KM_CORE_CT_MARKER, {0}, { 2 } },
    { KM_CORE_CT_CHAR, {0}, { '3' } },
    { KM_CORE_CT_MARKER, {0}, { 3 } },
    { KM_CORE_CT_MARKER, {0}, { 4 } },
    { KM_CORE_CT_CHAR, {0}, { 0x1F923 /* 🤣 */ } },
    KM_CORE_CONTEXT_ITEM_END
  };

  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  auto str = km_core_state_context_debug(test_state, KM_CORE_DEBUG_CONTEXT_CACHED);
  // std::cout << str << std::endl;
  assert(std::u16string(str) == u"|123🤣| (len: 9) [ M(5) U+0031 M(1) U+0032 M(2) U+0033 M(3) M(4) U+1f923 ]");
  km_core_cp_dispose(str);
}

void test_context_debug() {
  test_context_debug_empty();
  test_context_debug_various();
}

//-------------------------------------------------------------------------------------
// Launcher
//-------------------------------------------------------------------------------------

constexpr const auto help_str =
    "\
state_context [--color] <SOURCE_PATH>\n\
\n\
  --color         Force color output\n\
  SOURCE_PATH     Path where state_context.cpp is found; kmx files are\n\
                  located relative to this path.\n";

int
error_args() {
  std::cerr << "debug_api: Invalid arguments." << std::endl;
  std::cout << help_str;
  return 1;
}

int
main(int argc, char *argv[]) {
  if (argc < 2) {
    return error_args();
  }

  auto arg_color = std::string(argv[1]) == "--color";
  if (arg_color && argc < 3) {
    return error_args();
  }
  console_color::enabled = console_color::isaterminal() || arg_color;

#ifdef __EMSCRIPTEN__
  arg_path = get_wasm_file_path(argv[arg_color ? 2 : 1]);
#else
  arg_path = argv[arg_color ? 2 : 1];
#endif

  test_context_set_if_needed();
  test_context_clear();
  test_context_debug();
}
