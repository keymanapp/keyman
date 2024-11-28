// Tests for the state_context_* API methods

#include "keyman_core.h"
#include <string>

#include "context.hpp"
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
setup(const char *keyboard, const km_core_cu *context, bool setup_app_context = true) {
  teardown();

  km::core::path path = km::core::path::join(arg_path, keyboard);
  try_status(km_core_keyboard_load(path.native().c_str(), &test_kb));
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
  try_status(context_items_from_utf16(context, &citems));
  try_status(km_core_context_set(km_core_state_context(test_state), citems));
  if (setup_app_context) {
    try_status(km_core_context_set(km_core_state_app_context(test_state), citems));
  }
}

bool
is_identical_context(km_core_cu const *cached_context) {
  size_t buf_size;
  try_status(km_core_context_get(km_core_state_context(test_state), &citems));
  try_status(context_items_to_utf16(citems, nullptr, &buf_size));
  km_core_cu *new_cached_context = new km_core_cu[buf_size];
  try_status(context_items_to_utf16(citems, new_cached_context, &buf_size));
  bool result = std::u16string(cached_context) == new_cached_context;
  delete[] new_cached_context;
  return result;
}

#define assert_equal_msg(actual, expected, msg) { \
  if ((actual) != (expected)) { \
    std::wcerr << console_color::fg(console_color::BRIGHT_RED) \
             << "Test failed at " << __FILE__ << ":" << __LINE__ << ":" \
             << console_color::reset() \
             << std::endl \
             << (msg) << std::endl \
             << "expected: " << (expected) << std::endl \
             << "actual:   " << (actual) << std::endl; \
    std::exit(EXIT_FAILURE); \
  } \
}

#define assert_identical_context_with_markers(context, citems)                                     \
  {                                                                                                \
    km_core_context_item *citems_new;                                                              \
    try_status(km_core_context_get((context), &citems_new));                                       \
    for (int i = 0; (citems)[i].type || citems_new[i].type; i++) {                                 \
      assert_equal_msg(citems_new[i].type, (citems)[i].type, "Unexpected type:");                  \
      if ((citems)[i].type == KM_CORE_CT_CHAR) {                                                   \
        assert_equal_msg(citems_new[i].character, (citems)[i].character, "Unexpected character:"); \
      } else {                                                                                     \
        assert_equal_msg(citems_new[i].marker, (citems)[i].marker, "Unexpected marker:");          \
      }                                                                                            \
    }                                                                                              \
    km_core_context_items_dispose(citems_new);                                                     \
  }

// citems contains markers, but we will skip over them
#define assert_identical_context_without_markers(context, citems)                                  \
  {                                                                                                \
    km_core_context_item *citems_new;                                                              \
    try_status(km_core_context_get((context), &citems_new));                                       \
    for (int i = 0, i_new = 0; (citems)[i].type || citems_new[i_new].type; i++) {                  \
      if ((citems)[i].type == KM_CORE_CT_CHAR) {                                                   \
        assert_equal_msg(citems_new[i_new].type, (citems)[i].type, "Unexpected type:");            \
        assert_equal_msg(citems_new[i_new].character, (citems)[i].character, "Unexpected character:"); \
        i_new++;                                                                                   \
      } else if((citems)[i].type == KM_CORE_CT_END ) {                                             \
        assert_equal_msg(citems_new[i_new].type, (citems)[i].type, "Unexpected type:");            \
      }                                                                                            \
    }                                                                                              \
    km_core_context_items_dispose(citems_new);                                                     \
  }

#define assert_equal_status(actual, expected) { \
  if ((actual) != (expected)) { \
    std::wcerr << console_color::fg(console_color::BRIGHT_RED) \
             << "Test failed at " << __FILE__ << ":" << __LINE__ << ":" \
             << console_color::reset() \
             << std::endl \
             << "expected: " << #expected << std::endl \
             << "actual:   " << (actual) << std::endl; \
    std::exit(EXIT_FAILURE); \
  } \
}


void
test_context_set_if_needed__identical_context() {
  km_core_cu const *cached_context  = u"This is a test";
  km_core_cu const *new_app_context = u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context, false);
  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UNCHANGED);
  test_assert(is_identical_context(cached_context));
  teardown();
}

void
test_context_set_if_needed__different_context() {
  km_core_cu const *cached_context  = u"This isn't a test";
  km_core_cu const *new_app_context = u"This is a    test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  test_assert(!is_identical_context(cached_context));
  test_assert(is_identical_context(new_app_context));
  teardown();
}

void
test_context_set_if_needed__cached_context_cleared() {
  km_core_cu const *cached_context  = u"";
  km_core_cu const *new_app_context = u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  km_core_state_context_clear(test_state);
  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  test_assert(!is_identical_context(cached_context));
  test_assert(is_identical_context(new_app_context));
  teardown();
}

void
test_context_set_if_needed__application_context_empty() {
  km_core_cu const *cached_context  = u"This is a test";
  km_core_cu const *new_app_context = u"";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  test_assert(!is_identical_context(cached_context));
  test_assert(is_identical_context(new_app_context));
  teardown();
}

void
test_context_set_if_needed__app_context_is_longer() {
  km_core_cu const *cached_context  = u"This is a test";
  km_core_cu const *new_app_context = u"Longer This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  test_assert(!is_identical_context(cached_context));
  test_assert(is_identical_context(new_app_context));
  teardown();
}

void
test_context_set_if_needed__app_context_is_shorter() {
  km_core_cu const *cached_context  = u"This is a test";
  km_core_cu const *new_app_context = u"is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  test_assert(!is_identical_context(cached_context));
  test_assert(is_identical_context(new_app_context));
  teardown();
}

void
test_context_set_if_needed__identical_context_and_markers() {
  km_core_cu const *cached_context  = u"123";
  km_core_cu const *new_app_context = u"123";
  setup("k_000___null_keyboard.kmx", cached_context);

  km_core_context_item const citems[] = {
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};

  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UNCHANGED);

  assert_identical_context_with_markers(km_core_state_context(test_state), citems);
  assert_identical_context_without_markers(km_core_state_app_context(test_state), citems);

  teardown();
}

void
test_context_set_if_needed__cached_context_shorter_and_markers() {
  km_core_cu const *cached_context  = u"123";
  km_core_cu const *new_app_context = u"0123";
  setup("k_000___null_keyboard.kmx", cached_context);

  km_core_context_item const citems[] = {
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};
  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);

  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'0'}},
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};
  assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems);
  assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_citems);
  test_assert(is_identical_context(new_app_context));

  teardown();
}

void
test_context_set_if_needed__cached_context_shorter_and_markers_nfu() {
  km_core_cu const *cached_context  = u"bce\u0323\u0302";
  km_core_cu const *new_app_context = u"abcệ";
  setup("/a/dummy/keyboard.mock", cached_context);

  km_core_context_item const citems[] = {
      {KM_CORE_CT_MARKER, {0}, {1}}, {KM_CORE_CT_CHAR, {0}, {'b'}}, {KM_CORE_CT_MARKER, {0}, {2}},
      {KM_CORE_CT_CHAR, {0}, {'c'}}, {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}},
      {KM_CORE_CT_CHAR, {0}, {'e'}}, {KM_CORE_CT_CHAR, {0}, {u'\u0323'}},
      {KM_CORE_CT_CHAR, {0}, {u'\u0302'}}, KM_CORE_CONTEXT_ITEM_END};
  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);

  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {1}}, {KM_CORE_CT_CHAR, {0}, {'b'}}, {KM_CORE_CT_MARKER, {0}, {2}},
      {KM_CORE_CT_CHAR, {0}, {'c'}}, {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}},
      {KM_CORE_CT_CHAR, {0}, {'e'}}, {KM_CORE_CT_CHAR, {0}, {u'\u0323'}},
      {KM_CORE_CT_CHAR, {0}, {u'\u0302'}}, KM_CORE_CONTEXT_ITEM_END};
  assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems);
  km_core_context_item const expected_app_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_CHAR, {0}, {'b'}},
      {KM_CORE_CT_CHAR, {0}, {'c'}},
      {KM_CORE_CT_CHAR, {0}, {u'ệ'}},
      KM_CORE_CONTEXT_ITEM_END};
  assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_app_citems);

  teardown();
}

void
test_context_set_if_needed__cached_context_longer_and_markers() {
  km_core_cu const *cached_context  = u"0123";
  km_core_cu const *new_app_context = u"123";
  setup("k_000___null_keyboard.kmx", cached_context);

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'0'}},
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};
  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);

  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};
  assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems);
  assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_citems);

  teardown();
}

void
test_context_set_if_needed__cached_context_longer_and_markers_nfu() {
  km_core_cu const *cached_context  = u"abce\u0323\u0302";
  km_core_cu const *new_app_context = u"bcệ";
  setup("/a/dummy/keyboard.mock", cached_context);

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {1}}, {KM_CORE_CT_CHAR, {0}, {'b'}}, {KM_CORE_CT_MARKER, {0}, {2}},
      {KM_CORE_CT_CHAR, {0}, {'c'}}, {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}},
      {KM_CORE_CT_CHAR, {0}, {'e'}}, {KM_CORE_CT_CHAR, {0}, {u'\u0323'}},
      {KM_CORE_CT_CHAR, {0}, {u'\u0302'}}, KM_CORE_CONTEXT_ITEM_END};
  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);

  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_MARKER, {0}, {1}}, {KM_CORE_CT_CHAR, {0}, {'b'}}, {KM_CORE_CT_MARKER, {0}, {2}},
      {KM_CORE_CT_CHAR, {0}, {'c'}}, {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}},
      {KM_CORE_CT_CHAR, {0}, {'e'}}, {KM_CORE_CT_CHAR, {0}, {u'\u0323'}},
      {KM_CORE_CT_CHAR, {0}, {u'\u0302'}}, KM_CORE_CONTEXT_ITEM_END};
  assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems);

  km_core_context_item const expected_app_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'b'}},
      {KM_CORE_CT_CHAR, {0}, {'c'}},
      {KM_CORE_CT_CHAR, {0}, {u'ệ'}},
      KM_CORE_CONTEXT_ITEM_END};
  assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_app_citems);

  teardown();
}

void
test_context_set_if_needed__surrogate_pairs_unchanged() {
  km_core_cu const *cached_context  = u"a\U00010100";
  km_core_cu const *new_app_context = u"a\U00010100";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UNCHANGED);
  test_assert(is_identical_context(new_app_context));
  teardown();
}

void
test_context_set_if_needed__surrogate_pairs_app_context_longer() {
  km_core_cu const *cached_context  = u"a\U00010100";
  km_core_cu const *new_app_context = u"xa\U00010100";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  test_assert(is_identical_context(new_app_context));
  teardown();
}

void
test_context_set_if_needed__surrogate_pairs_cached_context_longer() {
  km_core_cu const *cached_context  = u"xa\U00010100";
  km_core_cu const *new_app_context = u"a\U00010100";
  setup("k_000___null_keyboard.kmx", cached_context);
  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  test_assert(is_identical_context(new_app_context));
  teardown();
}

void
test_context_set_if_needed__surrogate_pairs_unchanged_and_markers() {
  km_core_cu const *cached_context  = u"a\U00010100";
  km_core_cu const *new_app_context = u"a\U00010100";
  setup("k_000___null_keyboard.kmx", cached_context);

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};
  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UNCHANGED);
  assert_identical_context_with_markers(km_core_state_context(test_state), citems);
  assert_identical_context_without_markers(km_core_state_app_context(test_state), citems);

  teardown();
}

void
test_context_set_if_needed__surrogate_pairs_app_context_longer_and_markers() {
  km_core_cu const *cached_context  = u"a\U00010100";
  km_core_cu const *new_app_context = u"\U00010200a\U00010100";
  setup("k_000___null_keyboard.kmx", cached_context);

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};
  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {0x10200}},
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};

  assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems);
  assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_citems);

  teardown();
}

void
test_context_set_if_needed__surrogate_pairs_cached_context_longer_and_markers() {
  km_core_cu const *cached_context  = u"\U00010200a\U00010100";
  km_core_cu const *new_app_context = u"a\U00010100";
  setup("k_000___null_keyboard.kmx", cached_context);

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {0x10200}},
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};
  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  assert_equal_status(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};

  assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems);
  assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_citems);

  teardown();
}

void
test_context_set_if_needed() {
  // Scenarios from #10100:
  // 1. cached context identical to app context
  test_context_set_if_needed__identical_context();
  // 1a. cached context has markers and is identical to app context
  test_context_set_if_needed__identical_context_and_markers();
  // 2. cached context same length as app context but content is different
  test_context_set_if_needed__different_context();
  // 3. cached context is shorter than app context, but content is same as far as it goes
  test_context_set_if_needed__app_context_is_longer();
  // 3a. cached context has markers and is shorter than app context,
  // but content is same as far as it goes
  test_context_set_if_needed__cached_context_shorter_and_markers();
  test_context_set_if_needed__cached_context_shorter_and_markers_nfu();
  // 4. cached context is shorter than app context, but content is different
  test_context_set_if_needed__cached_context_cleared();
  // 5. cached context is longer than app context, but content is same as far as it goes
  test_context_set_if_needed__app_context_is_shorter();
  // 5a. cached context has markers and is longer than app context, but
  // content is same as far as it goes
  test_context_set_if_needed__cached_context_longer_and_markers();
  test_context_set_if_needed__cached_context_longer_and_markers_nfu();
  // 6. cached context is longer than app context, but content is different.
  test_context_set_if_needed__application_context_empty();
  // 7. surrogate pairs in context
  test_context_set_if_needed__surrogate_pairs_unchanged();
  test_context_set_if_needed__surrogate_pairs_unchanged_and_markers();
  test_context_set_if_needed__surrogate_pairs_app_context_longer();
  test_context_set_if_needed__surrogate_pairs_cached_context_longer();
  test_context_set_if_needed__surrogate_pairs_app_context_longer_and_markers();
  test_context_set_if_needed__surrogate_pairs_cached_context_longer_and_markers();
}

void
test_context_clear() {
  km_core_cu const *cached_context = u"This is a test";
  setup("k_000___null_keyboard.kmx", cached_context);
  try_status(km_core_state_context_clear(test_state));
  test_assert(!is_identical_context(cached_context));
  test_assert(is_identical_context(u""));
  teardown();
}

//-------------------------------------------------------------------------------------

void test_context_debug_empty() {
  km_core_cu const *cached_context =      u"";
  setup("k_000___null_keyboard.kmx", cached_context);
  auto str = km_core_state_context_debug(test_state, KM_CORE_DEBUG_CONTEXT_CACHED);
  // std::cout << str << std::endl;
  test_assert(std::u16string(str) == u"|| (len: 0) [ ]");
  km_core_cu_dispose(str);
}

void test_context_debug_various() {
  km_core_cu const *cached_context =      u"123\U0001F923";
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
  test_assert(std::u16string(str) == u"|123🤣| (len: 9) [ M(5) U+0031 M(1) U+0032 M(2) U+0033 M(3) M(4) U+1f923 ]");
  km_core_cu_dispose(str);
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
  SOURCE_PATH     Path of kmx folder that contains the kmx files\n";

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
