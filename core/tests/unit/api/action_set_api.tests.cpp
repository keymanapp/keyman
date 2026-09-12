/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Marc Durdin on 2023-10-23
 *
 * Keyman Core - tests for the set_actions API function
 */

#include <string>

#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"

#include "../helpers/core_test_helpers.h"

//-------------------------------------------------------------------------------------

struct TestData {
  const char* test_name;
  const char* keyboard_name;
  std::vector<km_core_action_item> action_items;
  km_core_actions actions;
};

std::string GenerateTestName(const testing::TestParamInfo<TestData>& info) {
  return info.param.test_name;
}

const km_core_option_item test_option = {
  u"key",
  u"value",
  KM_CORE_OPT_KEYBOARD
};

/*TODO: const*/ km_core_option_item test_options[] = {
  { u"key", u"value", KM_CORE_OPT_KEYBOARD },
  KM_CORE_OPTIONS_END
};

km_core_usv test_empty_output[] = {0};

class SetActionApiTest : public testing::TestWithParam<TestData> {
protected:
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;
  km_core_context_item * citems = nullptr;

  void Initialize(TestData const& data) {
    km::core::path path = km::core::path::join(test_dir, data.keyboard_name);

    auto blob = km::tests::load_kmx_file(path.native().c_str());
    ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(path.stem().c_str(), blob.data(), blob.size(), &test_kb));
    ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));
    ASSERT_STATUS_OK(context_items_from_utf16(u""/*data.context*/, &citems));
    ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));
    ASSERT_STATUS_OK(km_core_context_set(km_core_state_app_context(test_state), citems));
  }

  void TearDown() override {
    if(citems) {
      km_core_context_items_dispose(citems);
      citems = nullptr;
    }
    if(test_state) {
      km_core_state_dispose(test_state);
      test_state = nullptr;
    }
    if(test_kb) {
      km_core_keyboard_dispose(test_kb);
      test_kb = nullptr;
    }
  }
};

TEST_P(SetActionApiTest, HandlesValidInputs) {
  auto data = GetParam();
  ASSERT_NO_FATAL_FAILURE(Initialize(data));
  test_state->set_actions(data.actions);
  auto set_actions = test_state->actions();

  int n = 0;
  for(auto act = set_actions.begin(); act != set_actions.end(); act++, n++) {
    ASSERT_EQ(act->type, data.action_items[n].type);
    // TODO: all other fields
    switch(act->type) {
      case KM_CORE_IT_ALERT:
        break;
      case KM_CORE_IT_BACK:
        EXPECT_EQ(act->backspace.expected_type, data.action_items[n].backspace.expected_type);
        EXPECT_EQ(act->backspace.expected_value, data.action_items[n].backspace.expected_value);
        break;
      case KM_CORE_IT_CAPSLOCK:
        EXPECT_EQ(act->capsLock, data.action_items[n].capsLock);
        break;
      case KM_CORE_IT_CHAR:
        EXPECT_EQ(act->character, data.action_items[n].character);
        break;
      case KM_CORE_IT_EMIT_KEYSTROKE:
        break;
      case KM_CORE_IT_END:
        break;
      case KM_CORE_IT_INVALIDATE_CONTEXT:
        break;
      case KM_CORE_IT_MARKER:
        EXPECT_EQ(act->marker, data.action_items[n].marker);
        break;
      case KM_CORE_IT_PERSIST_OPT:
        EXPECT_EQ(act->option->scope, data.action_items[n].option->scope);
        EXPECT_EQ(std::u16string(act->option->key), data.action_items[n].option->key);
        EXPECT_EQ(std::u16string(act->option->value), data.action_items[n].option->value);
        break;
      default:
        // Invalid action type
        EXPECT_TRUE(false);
    }
  }
}


const TestData values[] = {
  {
    "TestTwoBackspaces", "../kmx/k_0000___null_keyboard.kmx",
    { // km_core_action_item[]
      km::tests::bksp_action_item(KM_CORE_BT_CHAR, 0),
      km::tests::bksp_action_item(KM_CORE_BT_CHAR, 0),
      km::tests::end_action_item()
    },
    { // km_core_actions
      2, // unsigned int code_points_to_delete;
      test_empty_output, // km_core_usv* output;
      test_empty_env_opts, // km_core_option_item* persist_options;
      KM_CORE_FALSE, // km_core_bool do_alert;
      KM_CORE_FALSE, // km_core_bool emit_keystroke;
      KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
      nullptr // km_core_usv* deleted_context;
    }
  },
  {
    "TestCharacter",  "../kmx/k_0000___null_keyboard.kmx",
    {
      km::tests::char_action_item(u'A'),
      km::tests::end_action_item()
    },
    {
      0, // unsigned int code_points_to_delete;
      (km_core_usv*) U"A", // km_core_usv* output;
      test_empty_env_opts, // km_core_option_item* persist_options;
      KM_CORE_FALSE, // km_core_bool do_alert;
      KM_CORE_FALSE, // km_core_bool emit_keystroke;
      KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
      nullptr // km_core_usv* deleted_context;
    }
  },
  {
    "TestAlert",  "../kmx/k_0000___null_keyboard.kmx",
    {
      km::tests::alert_action_item(),
      km::tests::end_action_item()
    },
    {
      0, // unsigned int code_points_to_delete;
      test_empty_output, // km_core_usv* output;
      test_empty_env_opts, // km_core_option_item* persist_options;
      KM_CORE_TRUE, // km_core_bool do_alert;
      KM_CORE_FALSE, // km_core_bool emit_keystroke;
      KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
      nullptr // km_core_usv* deleted_context;
    }
  },
  {
    "TestEmitKeystroke",  "../kmx/k_0000___null_keyboard.kmx",
    {
      km::tests::emit_keystroke_action_item(),
      km::tests::end_action_item()
    },
    {
      0, // unsigned int code_points_to_delete;
      test_empty_output, // km_core_usv* output;
      test_empty_env_opts, // km_core_option_item* persist_options;
      KM_CORE_FALSE, // km_core_bool do_alert;
      KM_CORE_TRUE, // km_core_bool emit_keystroke;
      KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
      nullptr // km_core_usv* deleted_context;
    }
  },
  {
    "TestInvalidateContext",  "../kmx/k_0000___null_keyboard.kmx",
    // note, this generates a no-op, DELETE?
    {
      // invalidate_context_action_item(),
      km::tests::end_action_item()
    },
    {
      0, // unsigned int code_points_to_delete;
      test_empty_output, // km_core_usv* output;
      test_empty_env_opts, // km_core_option_item* persist_options;
      KM_CORE_FALSE, // km_core_bool do_alert;
      KM_CORE_FALSE, // km_core_bool emit_keystroke;
      KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
      nullptr // km_core_usv* deleted_context;
    }
  },
  {
    "TestPersistOpt",  "../kmx/k_0000___null_keyboard.kmx",
    {
      km::tests::persist_opt_action_item(&test_option),
      km::tests::end_action_item()
    },

    {
      0, // unsigned int code_points_to_delete;
      test_empty_output, // km_core_usv* output;
      test_options, // km_core_option_item* persist_options;
      KM_CORE_FALSE, // km_core_bool do_alert;
      KM_CORE_FALSE, // km_core_bool emit_keystroke;
      KM_CORE_CAPS_UNCHANGED, // new_caps_lock_state;
      nullptr // km_core_usv* deleted_context;
    }
  },
  {
    "TestCapsLock",  "../kmx/k_0000___null_keyboard.kmx",
    {
      km::tests::caps_action_item(KM_CORE_CAPS_ON),
      // km::tests::invalidate_context_action_item(),
      km::tests::end_action_item()
    },

    {
      0, // unsigned int code_points_to_delete;
      test_empty_output, // km_core_usv* output;
      test_empty_env_opts, // km_core_option_item* persist_options;
      KM_CORE_FALSE, // km_core_bool do_alert;
      KM_CORE_FALSE, // km_core_bool emit_keystroke;
      KM_CORE_CAPS_ON, // new_caps_lock_state;
      nullptr // km_core_usv* deleted_context;
    }
  }
};

INSTANTIATE_TEST_SUITE_P(KeymanCore, SetActionApiTest, testing::ValuesIn(values), GenerateTestName);

