/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the context API family of functions.
  Create Date:  30 Oct 2018
  Authors:      Tim Eves (TSE)
*/
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include "keyman_core.h"

#include "path.hpp"
#include "state.hpp"
#include "action_items.hpp"
#include "mock/mock_processor.hpp"

#include <test_assert.h>

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
  std::string get_json_doc(km_core_state const& state)
  {
    size_t sz = 0;
    try_status(km_core_state_to_json(&state, nullptr, &sz));
    std::string buf(sz-1, 0);
    try_status(km_core_state_to_json(&state, &buf[0], &sz));

    return buf;
  }

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

  km_core_option_item test_env_opts[] =
  {
    {u"hello",     u"world", 0},
    KM_CORE_OPTIONS_END
  };

constexpr char const *doc1_expected = u8"\
{\n\
    \"$schema\" : \"keyman/core/docs/introspection.schema\",\n\
    \"keyboard\" : {\n\
        \"id\" : \"dummy\",\n\
        \"version\" : \"3.145\",\n\
        \"rules\" : []\n\
    },\n\
    \"context\" : [\n\
        \"H\",\n\
        \"e\",\n\
        \"l\",\n\
        \"l\",\n\
        \"o\",\n\
        \" \",\n\
        \"😁\",\n\
        \"S\",\n\
        \"I\",\n\
        \"L\"\n\
    ],\n\
    \"actions\" : [\n\
        { \"persist\" : { \"keyboard\" : { \"__test_point\" : \"F2 pressed test save.\" } } }\n\
    ]\n\
}\n";

constexpr char const *doc2_expected = u8"\
{\n\
    \"$schema\" : \"keyman/core/docs/introspection.schema\",\n\
    \"keyboard\" : {\n\
        \"id\" : \"dummy\",\n\
        \"version\" : \"3.145\",\n\
        \"rules\" : []\n\
    },\n\
    \"context\" : [],\n\
    \"actions\" : []\n\
}\n";


constexpr km_core_option_item const expected_persist_opt = {
  u"__test_point",
  u"F2 pressed test save.",
  KM_CORE_OPT_KEYBOARD
};

constexpr km_core_option_item const clone_persist_opt = {
  u"__test_clone",
  u"Not in original",
  KM_CORE_OPT_KEYBOARD
};

constexpr km_core_option_item const test_point_3_opt = {
  u"__test_point_3",
  u"F3 pressed test save 1.",
  KM_CORE_OPT_KEYBOARD
};

constexpr km_core_option_item const test_point_4_opt = {
  u"__test_point_4",
  u"F3 pressed test save 2.",
  KM_CORE_OPT_KEYBOARD
};

extern "C"
{
  uint8_t test_imx_callback(km_core_state *state, uint32_t imx_id, void *callback_object){

  // does nothing;
  return 1;
  }
};

} // namespace

int main(int argc, char * argv[])
{
  auto arg_color = std::string(argc > 1 ? argv[1] : "") == "--color";
  console_color::enabled = console_color::isaterminal() || arg_color;

  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr,
               * test_clone = nullptr,
               * test_clone_2 = nullptr;
  test_kb = (km_core_keyboard *)new km::core::mock_processor(km::core::path("dummy.mock"));

  // Simple sanity tests.
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_core_state_clone(test_state, &test_clone));
  // Check sub objects have been copied and not shared.
  if (km_core_state_context(test_state) == km_core_state_context(test_clone))
    return __LINE__;
  size_t n_actions = 0;
  if (km_core_state_action_items(test_state, &n_actions) == nullptr
      && n_actions != 0)
    return __LINE__;
  // Check registering platform engine callback
  km_core_state_imx_register_callback(test_state, test_imx_callback, nullptr);
  km_core_state_imx_deregister_callback(test_state);

  // Lets add data and do some basic checks of options and km_core_context
  km_core_context_item *citems = nullptr;
  try_status(context_items_from_utf16(u"Hello 😁", &citems));
  try_status(km_core_context_set(km_core_state_context(test_state), citems));
  km_core_context_items_dispose(citems);
  if(km_core_context_length(km_core_state_context(test_state)) != 7)
    return __LINE__;
  if(km_core_context_length(km_core_state_context(test_clone)) != 0)
    return __LINE__;

  // Overwrite some data.
  km_core_option_item new_opt[] = {
    {u"hello", u"globe", KM_CORE_OPT_ENVIRONMENT},
    KM_CORE_OPTIONS_END};
  try_status(km_core_state_options_update(test_clone, new_opt));

  // Test the engine
  auto attrs = km_core_get_engine_attrs(test_state);

  DISABLE_WARNING_PUSH
  DISABLE_WARNING_TYPE_LIMITS
  // Check the lib supplies our required interface.
  if (attrs->current - attrs->age > KM_CORE_LIB_CURRENT
      || attrs->current < KM_CORE_LIB_CURRENT) return __LINE__;
  if (attrs->max_context < 16) return __LINE__;
  DISABLE_WARNING_POP

  try_status(km_core_process_event(test_state, KM_CORE_VKEY_S,
                                  KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  test_assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('S')}}, {KM_CORE_IT_END}}));
  try_status(km_core_process_event(test_state, KM_CORE_VKEY_I,
                                  KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  test_assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('I')}}, {KM_CORE_IT_END}}));
  try_status(km_core_process_event(test_state, KM_CORE_VKEY_L, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  test_assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('l')}}, {KM_CORE_IT_END}}));

  try_status(km_core_process_event(test_state, KM_CORE_VKEY_BKSP, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  test_assert(action_items(test_state, {{KM_CORE_IT_BACK, {0,}, {0}}, {KM_CORE_IT_END}}));
  try_status(km_core_process_event(test_state, KM_CORE_VKEY_L,
                                  KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  test_assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('L')}}, {KM_CORE_IT_END}}));

  // Without the calling `km_core_state_context_set_if_needed` action struct has a delete for 'L'?
  // Issue raised to investigate further: https://github.com/keymanapp/keyman/issues/15962
  km_core_cu const *state_context = get_context_as_string(km_core_state_context(test_state));
  km_core_state_context_set_if_needed(test_state, state_context);
  delete [] state_context;

  try_status(km_core_process_event(test_state, KM_CORE_VKEY_F2, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));

  km_core_action_item action = {KM_CORE_IT_PERSIST_OPT, {0,}, };
  action.option = &expected_persist_opt;
  test_assert(action_items(test_state, {action, {KM_CORE_IT_END}}));

  // Test debug dump
  auto doc1 = get_json_doc(*test_state),
       doc2 = get_json_doc(*test_clone);

  std::cout << doc1 << std::endl;
  std::cout << doc2 << std::endl;

  // These should not be equal.
  if (doc1 == doc2)           return __LINE__;
  // These should be.
  if (doc1 != doc1_expected)  return __LINE__;
  if (doc2 != doc2_expected)  return __LINE__;

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

  test_assert (expect_action_struct(state_actions,
    expected_state_code_points_to_delete,
    expected_state_output,
    expected_options,
    expected_state_do_alert,
    expected_state_emit_keystroke,
    expected_state_new_caps_lock_state,
    expected_deleted_text
  ));

  test_assert (expect_action_struct(clone_actions,
    clone_state_code_points_to_delete,
    clone_state_output,
    clone_state_options,
    clone_state_do_alert,
    clone_state_emit_keystroke,
    clone_state_new_caps_lock_state,
    clone_state_deleted_text
  ));

// Add two actions before cloning the state again
try_status(km_core_process_event(test_state, KM_CORE_VKEY_F3, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
try_status(km_core_state_clone(test_state, &test_clone_2));

// Now put an option in the test_clone_2 state only
km_core_action_item action_clone = {KM_CORE_IT_PERSIST_OPT, {0,}, };
action_clone.option = &clone_persist_opt;
if (test_clone_2->actions().back().type == KM_CORE_IT_END) {
      test_clone_2->actions().pop_back();
}
km_core_state_queue_action_items(test_clone_2, &action_clone);
test_clone_2->actions().commit();

  // Test debug dump
auto doc3 = get_json_doc(*test_state), doc4 = get_json_doc(*test_clone_2);
std::cout << "doc3:" << std::endl;
std::cout << doc3 << std::endl;
std::cout << "doc4:" << std::endl;
std::cout << doc4 << std::endl;
if (doc3 == doc4)           return __LINE__;

km_core_action_item action_tp3 = {KM_CORE_IT_PERSIST_OPT, {0,}, };
action_tp3.option = &test_point_3_opt;

km_core_action_item action_tp4 = {KM_CORE_IT_PERSIST_OPT, {0,}, };
action_tp4.option = &test_point_4_opt;

test_assert(action_items(test_state, {action_tp3, action_tp4, {KM_CORE_IT_END}}));
// Check that test_clone_2 has the same persisted options plus the extra queued option.
test_assert(action_items(test_clone_2, {action_tp3, action_tp4, action_clone, {KM_CORE_IT_END}}));

  // Destroy them
   km_core_state_dispose(test_state);
   km_core_state_dispose(test_clone);
   km_core_state_dispose(test_clone_2);
   km_core_keyboard_dispose(test_kb);

  return 0;
}
