/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Marc Durdin on 2024-01-15
 *
 * Keyman Core - tests for normalization in the context API
 */

#include <string>
#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"

#include "../helpers/core_test_helpers.h"

//-------------------------------------------------------------------------------------
// Context normalization tests
//-------------------------------------------------------------------------------------

class ContextNormalizationTests : public testing::Test {
protected:
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;

  void Initialize(const char *keyboard) {
    km::core::path path = km::core::path::join(test_dir, "fixtures", "keyboards", "17.0", keyboard);
    auto blob = km::tests::load_kmx_file(path.native().c_str());
    ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(path.stem().c_str(), blob.data(), blob.size(), &test_kb));
    ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));
  }

  void TearDown() override {
    if(test_state) {
      km_core_state_dispose(test_state);
      test_state = nullptr;
    }
    if(test_kb) {
      km_core_keyboard_dispose(test_kb);
      test_kb = nullptr;
    }
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

  void is_identical_context(km_core_cu const *cached_context, km_core_debug_context_type context_type) {
    size_t buf_size;
    km_core_context_item * citems = nullptr;

    debug_context(context_type);

    if(context_type == KM_CORE_DEBUG_CONTEXT_APP) {
      ASSERT_STATUS_OK(km_core_context_get(km_core_state_app_context(test_state), &citems));
    } else {
      ASSERT_STATUS_OK(km_core_context_get(km_core_state_context(test_state), &citems));
    }
    ASSERT_STATUS_OK(context_items_to_utf16(citems, nullptr, &buf_size));
    km_core_cu* new_cached_context = new km_core_cu[buf_size];
    ASSERT_STATUS_OK(context_items_to_utf16(citems, new_cached_context, &buf_size));

    km_core_context_items_dispose(citems);

    ASSERT_EQ(std::u16string(cached_context), new_cached_context);
    delete[] new_cached_context;
  }
};

TEST_F(ContextNormalizationTests, TestContextNormalizationAlreadyNfd) {
  km_core_cu const *app_context_nfd = u"A\u0300";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_001_tiny.kmx"));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, app_context_nfd), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_identical_context(app_context_nfd, KM_CORE_DEBUG_CONTEXT_APP));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(app_context_nfd, KM_CORE_DEBUG_CONTEXT_CACHED));
}

TEST_F(ContextNormalizationTests, TestContextNormalizationBasic) {
  km_core_cu const *application_context = u"This is a test À";
  km_core_cu const *cached_context =      u"This is a test A\u0300";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_001_tiny.kmx"));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, application_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_identical_context(application_context, KM_CORE_DEBUG_CONTEXT_APP));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(cached_context, KM_CORE_DEBUG_CONTEXT_CACHED));
}

TEST_F(ContextNormalizationTests, TestContextNormalizationHefty) {
                                          // Latin     Latin            "ṩ"                   "Å"                 Tirhuta U+114bc -> U+114B9 U+114B0
  km_core_cu const *application_context = u"À"       u"é̖"             u"\u1e69"              u"\u212b"           u"\U000114BC";
  km_core_cu const *cached_context =      u"A\u0300" u"e\u0316\u0301" u"\u0073\u0323\u0307"  u"\u0041\u030a"     u"\U000114B9\U000114B0";
  ASSERT_NO_FATAL_FAILURE(Initialize("k_001_tiny.kmx"));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, application_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_identical_context(application_context, KM_CORE_DEBUG_CONTEXT_APP));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(cached_context, KM_CORE_DEBUG_CONTEXT_CACHED));
}


TEST_F(ContextNormalizationTests, TestContextNormalizationInvalidUnicode) {
  GTEST_SKIP();

  // TODO: see #10392 we need to strip illegal chars: test_context_normalization_invalid_unicode(); // -- unpaired surrogate, illegals
                                          // unpaired surrogate     illegal
  km_core_cu const application_context[] = { 0xDC01, 0x0020, 0x0020, 0xFFFF, 0x0000 };
  km_core_cu const cached_context[] =      { 0xDC01, 0x0020, 0x0020, 0xFFFF, 0x0000 };
  ASSERT_NO_FATAL_FAILURE(Initialize("k_001_tiny.kmx"));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, application_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_identical_context(application_context, KM_CORE_DEBUG_CONTEXT_APP));
  ASSERT_NO_FATAL_FAILURE(is_identical_context(cached_context, KM_CORE_DEBUG_CONTEXT_CACHED));
}

TEST_F(ContextNormalizationTests, TestContextNormalizationLoneTrailingSurrogate) {
                                   // unpaired trail surrogate
  km_core_cu const application_context[] = { 0xDC01, 0x0020, 0x0020, 0x0000 };
  km_core_cu const cached_context[] = /* skipped*/ { 0x0020, 0x0020, 0x0000 };
  ASSERT_NO_FATAL_FAILURE(Initialize("k_001_tiny.kmx"));
  ASSERT_EQ(km_core_state_context_set_if_needed(test_state, application_context), KM_CORE_CONTEXT_STATUS_UPDATED);
  ASSERT_NO_FATAL_FAILURE(is_identical_context(application_context+1, KM_CORE_DEBUG_CONTEXT_APP)); // first code unit is skipped
  ASSERT_NO_FATAL_FAILURE(is_identical_context(cached_context, KM_CORE_DEBUG_CONTEXT_CACHED));
}
