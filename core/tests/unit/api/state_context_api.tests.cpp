/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Tests for the state_context_* API methods
 */

#include "keyman_core.h"
#include <string>
#include <iostream>

#include "context.hpp"
#include "path.hpp"
#include "mock/mock_processor.hpp"

#include "../helpers/core_test_helpers.h"

//-------------------------------------------------------------------------------------
// Context tests
//-------------------------------------------------------------------------------------

class StateContextApiTests : public testing::Test {
protected:
  km_core_keyboard *test_kb    = nullptr;
  km_core_state *test_state    = nullptr;
  km_core_context_item *citems = nullptr;

  void Initialize(const char *keyboard, const km_core_cu *context, bool setup_app_context = true) {
    km::core::path path = km::core::path(test_dir / ".." / "kmx" / keyboard);
    auto blob = km::tests::load_kmx_file(path.native().c_str());
    const auto mock_extension = ".mock";
    if (strlen(keyboard) > strlen(mock_extension) && strcmp(keyboard + strlen(keyboard) - strlen(mock_extension), mock_extension) == 0) {
      km::core::abstract_processor* kp = new km::core::mock_processor(keyboard);
      test_kb = static_cast<km_core_keyboard*>(kp);
    } else {
      ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(path.stem().c_str(), blob.data(), blob.size(), &test_kb));
    }
    ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));
    ASSERT_STATUS_OK(context_items_from_utf16(context, &citems));
    ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));
    if (setup_app_context) {
      ASSERT_STATUS_OK(km_core_context_set(km_core_state_app_context(test_state), citems));
    }
  }

  void TearDown() override {
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

  void is_identical_context(km_core_cu const *cached_context) {
    size_t buf_size;
    ASSERT_STATUS_OK(km_core_context_get(km_core_state_context(test_state), &citems));
    ASSERT_STATUS_OK(context_items_to_utf16(citems, nullptr, &buf_size));
    km_core_cu *new_cached_context = new km_core_cu[buf_size];
    ASSERT_STATUS_OK(context_items_to_utf16(citems, new_cached_context, &buf_size));
    ASSERT_EQ(std::u16string(cached_context), new_cached_context);
    delete[] new_cached_context;
  }

  void is_different_context(km_core_cu const *cached_context) {
    size_t buf_size;
    ASSERT_STATUS_OK(km_core_context_get(km_core_state_context(test_state), &citems));
    ASSERT_STATUS_OK(context_items_to_utf16(citems, nullptr, &buf_size));
    km_core_cu *new_cached_context = new km_core_cu[buf_size];
    ASSERT_STATUS_OK(context_items_to_utf16(citems, new_cached_context, &buf_size));
    ASSERT_NE(std::u16string(cached_context), new_cached_context);
    delete[] new_cached_context;
  }
};


void assert_identical_context_with_markers(const km_core_context *context, const km_core_context_item *citems) {
  km_core_context_item *citems_new;
  ASSERT_STATUS_OK(km_core_context_get(context, &citems_new));
  for (int i = 0; citems[i].type || citems_new[i].type; i++) {
    ASSERT_EQ(citems_new[i].type, citems[i].type) << "Unexpected type:";
    if (citems[i].type == KM_CORE_CT_CHAR) {
      ASSERT_EQ(citems_new[i].character, citems[i].character) << "Unexpected character:";
    } else {
      ASSERT_EQ(citems_new[i].marker, citems[i].marker) << "Unexpected marker:";
    }
  }
  km_core_context_items_dispose(citems_new);
}

// citems contains markers, but we will skip over them
void assert_identical_context_without_markers(const km_core_context *context, const km_core_context_item *citems) {
  km_core_context_item *citems_new;
  ASSERT_STATUS_OK(km_core_context_get(context, &citems_new));
  for (int i = 0, i_new = 0; citems[i].type || citems_new[i_new].type; i++) {
    if (citems[i].type == KM_CORE_CT_CHAR) {
      ASSERT_EQ(citems_new[i_new].type, citems[i].type) << "Unexpected type:";
      ASSERT_EQ(citems_new[i_new].character, citems[i].character) << "Unexpected character:";
      i_new++;
    } else if(citems[i].type == KM_CORE_CT_END) {
      ASSERT_EQ(citems_new[i_new].type, citems[i].type) << "Unexpected type:";
    }
  }
  km_core_context_items_dispose(citems_new);
}

// Scenarios from #10100:

// 1. cached context identical to app context
TEST_F(StateContextApiTests, TestContextSetIfNeededIdenticalContext) {
  km_core_cu const *cached_context  = u"This is a test";
  km_core_cu const *new_app_context = u"This is a test";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context, false));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UNCHANGED);
  ASSERT_NO_FATAL_FAILURE(is_identical_context(cached_context));
}

// 1a. cached context has markers and is identical to app context
TEST_F(StateContextApiTests, TestContextSetIfNeededIdenticalContextAndMarkers) {
  km_core_cu const *cached_context  = u"123";
  km_core_cu const *new_app_context = u"123";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));

  km_core_context_item const citems[] = {
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};

  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UNCHANGED);

  ASSERT_NO_FATAL_FAILURE(assert_identical_context_with_markers(km_core_state_context(test_state), citems));
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_without_markers(km_core_state_app_context(test_state), citems));
}

// 2. cached context same length as app context but content is different
TEST_F(StateContextApiTests, TestContextSetIfNeededDifferentContext) {
  km_core_cu const *cached_context  = u"This isn't a test";
  km_core_cu const *new_app_context = u"This is a    test";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_different_context(cached_context));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(new_app_context));
}

// 3. cached context is shorter than app context, but content is same as far as it goes
TEST_F(StateContextApiTests,  TestContextSetIfNeededAppContextIsLonger) {
  km_core_cu const *cached_context  = u"This is a test";
  km_core_cu const *new_app_context = u"Longer This is a test";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_different_context(cached_context));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(new_app_context));
}

// 3a. cached context has markers and is shorter than app context,
// but content is same as far as it goes
TEST_F(StateContextApiTests, TestContextSetIfNeededCachedContextShorterAndMarkers) {
  km_core_cu const *cached_context  = u"123";
  km_core_cu const *new_app_context = u"0123";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));

  km_core_context_item const citems[] = {
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);

  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'0'}},
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems));
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_citems));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(new_app_context));
}

TEST_F(StateContextApiTests, TestContextSetIfNeededCachedContextShorterAndMarkersNfu) {
  km_core_cu const *cached_context  = u"bce\u0323\u0302";
  km_core_cu const *new_app_context = u"abcệ";
  ASSERT_NO_FATAL_FAILURE(Initialize("/a/dummy/keyboard.mock", cached_context));

  km_core_context_item const citems[] = {
      {KM_CORE_CT_MARKER, {0}, {1}}, {KM_CORE_CT_CHAR, {0}, {'b'}}, {KM_CORE_CT_MARKER, {0}, {2}},
      {KM_CORE_CT_CHAR, {0}, {'c'}}, {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}},
      {KM_CORE_CT_CHAR, {0}, {'e'}}, {KM_CORE_CT_CHAR, {0}, {u'\u0323'}},
      {KM_CORE_CT_CHAR, {0}, {u'\u0302'}}, KM_CORE_CONTEXT_ITEM_END};
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);

  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {1}}, {KM_CORE_CT_CHAR, {0}, {'b'}}, {KM_CORE_CT_MARKER, {0}, {2}},
      {KM_CORE_CT_CHAR, {0}, {'c'}}, {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}},
      {KM_CORE_CT_CHAR, {0}, {'e'}}, {KM_CORE_CT_CHAR, {0}, {u'\u0323'}},
      {KM_CORE_CT_CHAR, {0}, {u'\u0302'}}, KM_CORE_CONTEXT_ITEM_END};
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems));
  km_core_context_item const expected_app_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_CHAR, {0}, {'b'}},
      {KM_CORE_CT_CHAR, {0}, {'c'}},
      {KM_CORE_CT_CHAR, {0}, {u'ệ'}},
      KM_CORE_CONTEXT_ITEM_END};
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_app_citems));
}
// 4. cached context is shorter than app context, but content is different
TEST_F(StateContextApiTests, TestContextSetIfNeededCachedContextCleared) {
  km_core_cu const *cached_context  = u"";
  km_core_cu const *new_app_context = u"This is a test";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  km_core_state_context_clear(test_state);
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_different_context(cached_context));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(new_app_context));
}

// 5. cached context is longer than app context, but content is same as far as it goes
TEST_F(StateContextApiTests, TestContextSetIfNeededAppContextIsShorter) {
  km_core_cu const *cached_context  = u"This is a test";
  km_core_cu const *new_app_context = u"is a test";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_different_context(cached_context));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(new_app_context));
}

// 5a. cached context has markers and is longer than app context, but
// content is same as far as it goes
TEST_F(StateContextApiTests, TestContextSetIfNeededCachedContextLongerAndMarkers) {
  km_core_cu const *cached_context  = u"0123";
  km_core_cu const *new_app_context = u"123";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'0'}},
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);

  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_MARKER, {0}, {5}}, {KM_CORE_CT_CHAR, {0}, {'1'}}, {KM_CORE_CT_MARKER, {0}, {1}},
      {KM_CORE_CT_CHAR, {0}, {'2'}}, {KM_CORE_CT_MARKER, {0}, {2}}, {KM_CORE_CT_CHAR, {0}, {'3'}},
      {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}}, KM_CORE_CONTEXT_ITEM_END};
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems));
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_citems));
}

TEST_F(StateContextApiTests, TestContextSetIfNeededCachedContextLongerAndMarkersNfu) {
  km_core_cu const *cached_context  = u"abce\u0323\u0302";
  km_core_cu const *new_app_context = u"bcệ";
  ASSERT_NO_FATAL_FAILURE(Initialize("/a/dummy/keyboard.mock", cached_context));

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {1}}, {KM_CORE_CT_CHAR, {0}, {'b'}}, {KM_CORE_CT_MARKER, {0}, {2}},
      {KM_CORE_CT_CHAR, {0}, {'c'}}, {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}},
      {KM_CORE_CT_CHAR, {0}, {'e'}}, {KM_CORE_CT_CHAR, {0}, {u'\u0323'}},
      {KM_CORE_CT_CHAR, {0}, {u'\u0302'}}, KM_CORE_CONTEXT_ITEM_END};
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);

  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_MARKER, {0}, {1}}, {KM_CORE_CT_CHAR, {0}, {'b'}}, {KM_CORE_CT_MARKER, {0}, {2}},
      {KM_CORE_CT_CHAR, {0}, {'c'}}, {KM_CORE_CT_MARKER, {0}, {3}}, {KM_CORE_CT_MARKER, {0}, {4}},
      {KM_CORE_CT_CHAR, {0}, {'e'}}, {KM_CORE_CT_CHAR, {0}, {u'\u0323'}},
      {KM_CORE_CT_CHAR, {0}, {u'\u0302'}}, KM_CORE_CONTEXT_ITEM_END};
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems));

  km_core_context_item const expected_app_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'b'}},
      {KM_CORE_CT_CHAR, {0}, {'c'}},
      {KM_CORE_CT_CHAR, {0}, {u'ệ'}},
      KM_CORE_CONTEXT_ITEM_END};
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_app_citems));
}

// 6. cached context is longer than app context, but content is different.
TEST_F(StateContextApiTests, TestContextSetIfNeededApplicationContextEmpty) {
  km_core_cu const *cached_context  = u"This is a test";
  km_core_cu const *new_app_context = u"";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_different_context(cached_context));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(new_app_context));
}

// 7. surrogate pairs in context
TEST_F(StateContextApiTests, TestContextSetIfNeededSurrogatePairsUnchanged) {
  km_core_cu const *cached_context  = u"a\U00010100";
  km_core_cu const *new_app_context = u"a\U00010100";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UNCHANGED);
  ASSERT_NO_FATAL_FAILURE(is_identical_context(new_app_context));
}

TEST_F(StateContextApiTests, TestContextSetIfNeededSurrogatePairsAppContextLonger) {
  km_core_cu const *cached_context  = u"a\U00010100";
  km_core_cu const *new_app_context = u"xa\U00010100";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_identical_context(new_app_context));
}

TEST_F(StateContextApiTests, TestContextSetIfNeededSurrogatePairsCachedContextLonger) {
  km_core_cu const *cached_context  = u"xa\U00010100";
  km_core_cu const *new_app_context = u"a\U00010100";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_identical_context(new_app_context));
}

TEST_F(StateContextApiTests, TestContextSetIfNeededSurrogatePairsUnchangedAndMarkers) {
  km_core_cu const *cached_context  = u"a\U00010100";
  km_core_cu const *new_app_context = u"a\U00010100";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UNCHANGED);
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_with_markers(km_core_state_context(test_state), citems));
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_without_markers(km_core_state_app_context(test_state), citems));
}

TEST_F(StateContextApiTests, TestContextSetIfNeededSurrogatePairsAppContextLongerAndMarkers) {
  km_core_cu const *cached_context  = u"a\U00010100";
  km_core_cu const *new_app_context = u"\U00010200a\U00010100";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {0x10200}},
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};

  ASSERT_NO_FATAL_FAILURE(assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems));
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_citems));
}

TEST_F(StateContextApiTests, TestContextSetIfNeededSurrogatePairsCachedContextLongerAndMarkers) {
  km_core_cu const *cached_context  = u"\U00010200a\U00010100";
  km_core_cu const *new_app_context = u"a\U00010100";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));

  km_core_context_item const citems[] = {
      {KM_CORE_CT_CHAR, {0}, {0x10200}},
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, new_app_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  km_core_context_item const expected_citems[] = {
      {KM_CORE_CT_CHAR, {0}, {'a'}},
      {KM_CORE_CT_MARKER, {0}, {5}},
      {KM_CORE_CT_CHAR, {0}, {0x10100}},
      KM_CORE_CONTEXT_ITEM_END};

  ASSERT_NO_FATAL_FAILURE(assert_identical_context_with_markers(km_core_state_context(test_state), expected_citems));
  ASSERT_NO_FATAL_FAILURE(assert_identical_context_without_markers(km_core_state_app_context(test_state), expected_citems));
}


TEST_F(StateContextApiTests, TestContextClear) {
  km_core_cu const *cached_context = u"This is a test";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  ASSERT_STATUS_OK(km_core_state_context_clear(test_state));
  ASSERT_NO_FATAL_FAILURE(is_different_context(cached_context));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(u""));
}

//-------------------------------------------------------------------------------------

TEST_F(StateContextApiTests, TestContextDebugEmpty) {
  km_core_cu const *cached_context =      u"";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));
  auto str = km_core_state_context_debug(test_state, KM_CORE_DEBUG_CONTEXT_CACHED);
  // std::cout << str << std::endl;
  ASSERT_EQ(std::u16string(str), u"|| (len: 0) [ ]");
  km_core_cu_dispose(str);
}

TEST_F(StateContextApiTests, TestContextDebugVarious) {
  km_core_cu const *cached_context =      u"123\U0001F923";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_0000___null_keyboard.kmx", cached_context));

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

  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  auto str = km_core_state_context_debug(test_state, KM_CORE_DEBUG_CONTEXT_CACHED);
  // std::cout << str << std::endl;
  ASSERT_EQ(std::u16string(str), u"|123🤣| (len: 9) [ M(5) U+0031 M(1) U+0032 M(2) U+0033 M(3) M(4) U+1f923 ]");
  km_core_cu_dispose(str);
}
