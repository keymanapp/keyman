/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Tim Eves on 2018-10-30
 *
 * Keyman Core - state API unit tests
 */

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include "keyman_core.h"

#include "path.hpp"
#include "state.hpp"
#include "mock/mock_processor.hpp"

#include "../helpers/core_test_helpers.h"

#if defined(__GNUC__) || defined(__clang__)
#define PRAGMA(X)                   _Pragma(#X)
#define DISABLE_WARNING_PUSH        PRAGMA(GCC diagnostic push)
#define DISABLE_WARNING_POP         PRAGMA(GCC diagnostic pop)
#define DISABLE_WARNING(W)          PRAGMA(GCC diagnostic ignored #W)
#define DISABLE_WARNING_TYPE_LIMITS DISABLE_WARNING(-Wtype-limits)
#else
#define DISABLE_WARNING_PUSH
#define DISABLE_WARNING_POP
#define DISABLE_WARNING_TYPE_LIMITS
#endif

namespace
{
  km_core_option_item test_env_opts[] =
  {
    {u"hello",     u"world", 0},
    KM_CORE_OPTIONS_END
  };

constexpr km_core_option_item const expected_persist_opt = {
  u"test_keyboard_option",
  u"F2 pressed test save.",
  KM_CORE_OPT_KEYBOARD
};

extern "C"
{
  uint8_t test_imx_callback(km_core_state *state, uint32_t imx_id, void *callback_object){

  // does nothing;
  return 1;
  }
};



  inline
  bool action_options_equal(km_core_option_item const * lhs,
                            km_core_option_item const * rhs)
  {
    if (lhs == rhs) return true;
    if (!lhs || !rhs) return false;

    while (lhs->key && rhs->key) {
      if (lhs->scope != rhs->scope) return false;
      if (std::u16string(lhs->key) != std::u16string(rhs->key)) return false;
      if (std::u16string(lhs->value) != std::u16string(rhs->value)) return false;
      ++lhs;
      ++rhs;
    }

    return lhs->key == nullptr && rhs->key == nullptr;
  }

  inline
  bool expect_action_struct(
    km_core_actions const & actions,
    unsigned int expected_code_points_to_delete,
    km_core_usv const * expected_output,
    km_core_option_item const * expected_persist_options,
    km_core_bool expected_do_alert,
    km_core_bool expected_emit_keystroke,
    km_core_caps_state expected_new_caps_lock_state,
    km_core_usv const * expected_deleted_context
  ) {
    bool all_passed = true;

    std::cout << "\n=== Comparing action_struct fields ===" << std::endl;

    // code_points_to_delete
    std::cout << "code_points_to_delete: " << actions.code_points_to_delete
              << " (expected: " << expected_code_points_to_delete << ")";
    if (actions.code_points_to_delete != expected_code_points_to_delete) {
      std::cout << " [FAIL]" << std::endl;
      all_passed = false;
    } else {
      std::cout << " [PASS]" << std::endl;
    }

    // output
    std::cout << "output: " << (actions.output ? std::u32string(actions.output) : U"(null)")
              << " expected: " << (expected_output ? std::u32string(expected_output) : U"(null)") << std::endl;
    bool output_equal = false;
    if (expected_output == actions.output) { // nullptr or same pointer
      output_equal = true;
    } else if (!expected_output || !actions.output) {
      output_equal = false;
    } else if (expected_output && actions.output) {
      if (std::u32string(actions.output) != std::u32string(expected_output)) {
        output_equal = false;
      } else {
        output_equal = true;
      }
    }
    if (!output_equal) {
      std::cout << " [FAIL]" << std::endl;
      all_passed = false;
    } else {
      std::cout << " [PASS]" << std::endl;
    }

    // persist_options
    std::cout << "persist_options comparison:" << std::endl;
    if (!action_options_equal(actions.persist_options, expected_persist_options)) {
      std::cout << "  actual: ";
      if (actions.persist_options) {
        for (auto opt = actions.persist_options; opt->scope; ++opt) {
          std::cout << "[scope=" << opt->scope << ", key=" << opt->key << ", value=" << opt->value << "] ";
        }
      } else {
        std::cout << "(null)";
      }
      std::cout << std::endl;
      std::cout << "  expected: ";
      if (expected_persist_options) {
        for (auto opt = expected_persist_options; opt->key; ++opt) {
          std::cout << "[scope=" << opt->scope << ", key=" << opt->key << ", value=" << opt->value << "] ";
        }
      } else {
        std::cout << "(null)";
      }
      std::cout << " [FAIL]" << std::endl;
      all_passed = false;
    } else {
      std::cout << "  [PASS]" << std::endl;
    }

    // do_alert
    std::cout << "do_alert: " << (int)actions.do_alert
              << " (expected: " << (int)expected_do_alert << ")";
    if (actions.do_alert != expected_do_alert) {
      std::cout << " [FAIL]" << std::endl;
      all_passed = false;
    } else {
      std::cout << " [PASS]" << std::endl;
    }

    // emit_keystroke
    std::cout << "emit_keystroke: " << (int)actions.emit_keystroke
              << " (expected: " << (int)expected_emit_keystroke << ")";
    if (actions.emit_keystroke != expected_emit_keystroke) {
      std::cout << " [FAIL]" << std::endl;
      all_passed = false;
    } else {
      std::cout << " [PASS]" << std::endl;
    }

    // new_caps_lock_state
    std::cout << "new_caps_lock_state: " << (int)actions.new_caps_lock_state
              << " (expected: " << (int)expected_new_caps_lock_state << ")";
    if (actions.new_caps_lock_state != expected_new_caps_lock_state) {
      std::cout << " [FAIL]" << std::endl;
      all_passed = false;
    } else {
      std::cout << " [PASS]" << std::endl;
    }

    // deleted_context
    std::cout << "deleted_context: " << (actions.deleted_context ? std::u32string(actions.deleted_context) : U"(null)")
              << " (expected: " << (expected_deleted_context ? std::u32string(expected_deleted_context) : U"(null)") << ")";
    if (expected_deleted_context != actions.deleted_context) {
      if (std::u32string(actions.deleted_context) != std::u32string(expected_deleted_context)) {
        std::cout << " [FAIL]" << std::endl;
        all_passed = false;
      } else {
        std::cout << " [PASS]" << std::endl;
      }
    } else {
        std::cout << " [PASS]" << std::endl;
    }
    bool deleted_equal = false;
    if (expected_deleted_context == actions.deleted_context) { // nullptr or same pointer
      deleted_equal = true;
    } else if (!expected_deleted_context || !actions.deleted_context) {
      deleted_equal = false;
    } else if (expected_deleted_context && actions.deleted_context) {
      if (std::u32string(actions.deleted_context) != std::u32string(expected_deleted_context)) {
        deleted_equal = false;
      } else {
        deleted_equal = true;
      }
    }
    if (!deleted_equal) {
      std::cout << " [FAIL]" << std::endl;
      all_passed = false;
    } else {
      std::cout << " [PASS]" << std::endl;
    }


    std::cout << "=== End comparison ===" << std::endl << std::endl;

    return all_passed;
  }

} // namespace

TEST(StateApiTests, TestStateApi) {

  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr,
               * test_clone = nullptr;
  test_kb = (km_core_keyboard *)new km::core::mock_processor(km::core::path("dummy.mock"));

  // Simple sanity tests.
  ASSERT_STATUS_OK(km_core_state_create(test_kb, test_env_opts, &test_state));
  ASSERT_STATUS_OK(km_core_state_clone(test_state, &test_clone));
  // Check sub objects have been copied and not shared.
  ASSERT_NE(km_core_state_context(test_state), km_core_state_context(test_clone));

  size_t n_actions = 0;
  ASSERT_FALSE(km_core_state_action_items(test_state, &n_actions) == nullptr
      && n_actions != 0);

  // Check registering platform engine callback
  km_core_state_imx_register_callback(test_state, test_imx_callback, nullptr);
  km_core_state_imx_deregister_callback(test_state);

  // Lets add data and do some basic checks of options and km_core_context
  km_core_context_item *citems = nullptr;
  ASSERT_STATUS_OK(context_items_from_utf16(u"Hello 😁", &citems));
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));
  km_core_context_items_dispose(citems);
  ASSERT_EQ(km_core_context_length(km_core_state_context(test_state)), 7);
  ASSERT_EQ(km_core_context_length(km_core_state_context(test_clone)), 0);

  // Overwrite some data.
  km_core_option_item new_opt[] = {
    {u"hello", u"globe", KM_CORE_OPT_ENVIRONMENT},
    KM_CORE_OPTIONS_END};
  ASSERT_STATUS_OK(km_core_state_options_update(test_clone, new_opt));

  // Test the engine
  auto attrs = km_core_get_engine_attrs(test_state);

  DISABLE_WARNING_PUSH
  DISABLE_WARNING_TYPE_LIMITS
  // Check the lib supplies our required interface.
  ASSERT_FALSE(attrs->current - attrs->age > KM_CORE_LIB_CURRENT
      || attrs->current < KM_CORE_LIB_CURRENT);
  ASSERT_FALSE(attrs->max_context < 16);
  DISABLE_WARNING_POP

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_S,
                                  KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_TRUE(km::tests::action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('S')}}, {KM_CORE_IT_END}}));
  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_I,
                                  KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_TRUE(km::tests::action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('I')}}, {KM_CORE_IT_END}}));
  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_L, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_TRUE(km::tests::action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('l')}}, {KM_CORE_IT_END}}));

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_BKSP, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_TRUE(km::tests::action_items(test_state, {{KM_CORE_IT_BACK, {0,}, {0}}, {KM_CORE_IT_END}}));
  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_L,
                                  KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  ASSERT_TRUE(km::tests::action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('L')}}, {KM_CORE_IT_END}}));


  // Without the calling `km_core_state_context_set_if_needed` action struct has a delete for 'L'?
  // Issue raised to investigate further: https://github.com/keymanapp/keyman/issues/15962
  km_core_cu const *state_context = get_context_as_string(km_core_state_context(test_state));
  km_core_state_context_set_if_needed(test_state, state_context);
  delete [] state_context;

  ASSERT_STATUS_OK(km_core_process_event(test_state, KM_CORE_VKEY_F2, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));

  km_core_action_item action = {KM_CORE_IT_PERSIST_OPT, {0,}, };
  action.option = &expected_persist_opt;
  ASSERT_TRUE(km::tests::action_items(test_state, {action, {KM_CORE_IT_END}}));



  // Test the action_struct values for the active and cloned states are
  //  independent and match their respective expected values.
  const unsigned int expected_state_code_points_to_delete = 0;
  const km_core_usv * expected_state_output = U"";
  const km_core_bool expected_state_do_alert = KM_CORE_FALSE;
  const km_core_bool expected_state_emit_keystroke = KM_CORE_FALSE;
  const km_core_caps_state expected_state_new_caps_lock_state = KM_CORE_CAPS_UNCHANGED;
  km_core_option_item expected_options[] = {expected_persist_opt, KM_CORE_OPTIONS_END };
  const km_core_usv * expected_deleted_text = U"";
  // Cloned expected values
  const unsigned int clone_state_code_points_to_delete = 0;
  const km_core_usv * clone_state_output = U"";
  const km_core_bool clone_state_do_alert = KM_CORE_FALSE;
  const km_core_bool clone_state_emit_keystroke = KM_CORE_FALSE;
  const km_core_caps_state clone_state_new_caps_lock_state = KM_CORE_CAPS_UNCHANGED;
  km_core_option_item clone_state_options[] = {KM_CORE_OPTIONS_END};
  const km_core_usv * clone_state_deleted_text = nullptr;

  const auto & state_actions = test_state->action_struct();
  const auto & clone_actions = test_clone->action_struct();

  ASSERT_TRUE (expect_action_struct(state_actions,
    expected_state_code_points_to_delete,
    expected_state_output,
    expected_options,
    expected_state_do_alert,
    expected_state_emit_keystroke,
    expected_state_new_caps_lock_state,
    expected_deleted_text
  ));

  ASSERT_TRUE (expect_action_struct(clone_actions,
    clone_state_code_points_to_delete,
    clone_state_output,
    clone_state_options,
    clone_state_do_alert,
    clone_state_emit_keystroke,
    clone_state_new_caps_lock_state,
    clone_state_deleted_text
  ));

  // Destroy them
  km_core_state_dispose(test_state);
  km_core_state_dispose(test_clone);
  km_core_keyboard_dispose(test_kb);
}



