/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Marc Durdin on 2023-10-23
 *
 * Keyman Core - tests for actions API family of functions
 */

#include <gtest/gtest.h>
#include <string>
#include "keyman_core.h"

#include "path.hpp"
#include "action.hpp"

#include "../helpers/emscripten_filesystem.h"
#include "../helpers/action_items.hpp"

//-------------------------------------------------------------------------------------

TEST(ActionItemListToActionsObject, TwoBackspacesWithSingleContextCharDeletesOnlyOneChar) {
  const km_core_action_item action_items[] = {
    km::tests::char_action_item('D'),
    km::tests::bksp_action_item(KM_CORE_BT_CHAR, 'D'),
    km::tests::bksp_action_item(KM_CORE_BT_CHAR, 'E'),
    km::tests::end_action_item()
  };

  km_core_actions actions;
  ASSERT_TRUE(km::core::action_item_list_to_actions_object(action_items, &actions));

  EXPECT_EQ(actions.code_points_to_delete, 1);
  EXPECT_EQ(std::u32string(actions.output), U"");
  ASSERT_TRUE(actions.persist_options != nullptr);
  EXPECT_EQ(actions.persist_options[0].key, nullptr);
  EXPECT_EQ(actions.persist_options[0].value, nullptr);
  EXPECT_EQ(actions.persist_options[0].scope, KM_CORE_OPT_UNKNOWN);
  EXPECT_EQ(actions.do_alert, KM_CORE_FALSE);
  EXPECT_EQ(actions.emit_keystroke, KM_CORE_FALSE);
  EXPECT_EQ(actions.new_caps_lock_state, KM_CORE_CAPS_UNCHANGED);
  EXPECT_EQ(actions.deleted_context, nullptr);

  km::core::actions_dispose(actions);
}

//-------------------------------------------------------------------------------------

TEST(ActionItemListToActionsObject, InterleavedMarkersAreHandledCorrectly) {
  const km_core_action_item action_items[] = {
    km::tests::char_action_item('A'),
    km::tests::marker_action_item(1),
    km::tests::char_action_item('B'),
    km::tests::marker_action_item(2),
    km::tests::char_action_item('C'),
    km::tests::bksp_action_item(KM_CORE_BT_CHAR, 'C'),
    km::tests::bksp_action_item(KM_CORE_BT_MARKER, 2),
    km::tests::char_action_item('D'),
    km::tests::end_action_item()
  };

  km_core_actions actions;
  ASSERT_TRUE(km::core::action_item_list_to_actions_object(action_items, &actions));

  EXPECT_EQ(actions.code_points_to_delete, 0);
  EXPECT_EQ(std::u32string(actions.output), U"ABD");
  ASSERT_TRUE(actions.persist_options != nullptr);
  EXPECT_EQ(actions.persist_options[0].key, nullptr);
  EXPECT_EQ(actions.persist_options[0].value, nullptr);
  EXPECT_EQ(actions.persist_options[0].scope, KM_CORE_OPT_UNKNOWN);
  EXPECT_EQ(actions.do_alert, KM_CORE_FALSE);
  EXPECT_EQ(actions.emit_keystroke, KM_CORE_FALSE);
  EXPECT_EQ(actions.new_caps_lock_state, KM_CORE_CAPS_UNCHANGED);
  EXPECT_EQ(actions.deleted_context, nullptr);

  km::core::actions_dispose(actions);
}

//-------------------------------------------------------------------------------------

TEST(ActionItemListToActionsObject, AlertGeneratesDoAlert) {
  const km_core_action_item action_items[] = {
    km::tests::alert_action_item(),
    km::tests::end_action_item()
  };

  km_core_actions actions;
  ASSERT_TRUE(km::core::action_item_list_to_actions_object(action_items, &actions));

  EXPECT_EQ(actions.code_points_to_delete, 0);
  EXPECT_EQ(std::u32string(actions.output), U"");
  ASSERT_TRUE(actions.persist_options != nullptr);
  EXPECT_EQ(actions.persist_options[0].key, nullptr);
  EXPECT_EQ(actions.persist_options[0].value, nullptr);
  EXPECT_EQ(actions.persist_options[0].scope, KM_CORE_OPT_UNKNOWN);
  EXPECT_EQ(actions.do_alert, KM_CORE_TRUE);
  EXPECT_EQ(actions.emit_keystroke, KM_CORE_FALSE);
  EXPECT_EQ(actions.new_caps_lock_state, KM_CORE_CAPS_UNCHANGED);
  EXPECT_EQ(actions.deleted_context, nullptr);

  km::core::actions_dispose(actions);
}

//-------------------------------------------------------------------------------------

TEST(ActionItemListToActionsObject, EmitKeystrokeGeneratesEmitKeystroke) {
  const km_core_action_item action_items[] = {
    km::tests::emit_keystroke_action_item(),
    km::tests::end_action_item()
  };

  km_core_actions actions;
  ASSERT_TRUE(km::core::action_item_list_to_actions_object(action_items, &actions));

  EXPECT_EQ(actions.code_points_to_delete, 0);
  EXPECT_EQ(std::u32string(actions.output), U"");
  ASSERT_TRUE(actions.persist_options != nullptr);
  EXPECT_EQ(actions.persist_options[0].key, nullptr);
  EXPECT_EQ(actions.persist_options[0].value, nullptr);
  EXPECT_EQ(actions.persist_options[0].scope, KM_CORE_OPT_UNKNOWN);
  EXPECT_EQ(actions.do_alert, KM_CORE_FALSE);
  EXPECT_EQ(actions.emit_keystroke, KM_CORE_TRUE);
  EXPECT_EQ(actions.new_caps_lock_state, KM_CORE_CAPS_UNCHANGED);
  EXPECT_EQ(actions.deleted_context, nullptr);

  km::core::actions_dispose(actions);
}

//-------------------------------------------------------------------------------------

TEST(ActionItemListToActionsObject, InvalidateContextIsIgnored) {

  // note, this generates a no-op
  const km_core_action_item action_items[] = {
    km::tests::invalidate_context_action_item(),
    km::tests::end_action_item()
  };

  km_core_actions actions;
  ASSERT_TRUE(km::core::action_item_list_to_actions_object(action_items, &actions));

  EXPECT_EQ(actions.code_points_to_delete, 0);
  EXPECT_EQ(std::u32string(actions.output), U"");
  ASSERT_TRUE(actions.persist_options != nullptr);
  EXPECT_EQ(actions.persist_options[0].key, nullptr);
  EXPECT_EQ(actions.persist_options[0].value, nullptr);
  EXPECT_EQ(actions.persist_options[0].scope, KM_CORE_OPT_UNKNOWN);
  EXPECT_EQ(actions.do_alert, KM_CORE_FALSE);
  EXPECT_EQ(actions.emit_keystroke, KM_CORE_FALSE);
  EXPECT_EQ(actions.new_caps_lock_state, KM_CORE_CAPS_UNCHANGED);
  EXPECT_EQ(actions.deleted_context, nullptr);

  km::core::actions_dispose(actions);
}

//-------------------------------------------------------------------------------------

TEST(ActionItemListToActionsObject, PersistOptGeneratesPersistOption) {
  const km_core_option_item option = {
    u"key",
    u"value",
    KM_CORE_OPT_KEYBOARD
  };

  const km_core_action_item action_items[] = {
    km::tests::persist_opt_action_item(&option),
    km::tests::end_action_item()
  };

  km_core_actions actions;
  ASSERT_TRUE(km::core::action_item_list_to_actions_object(action_items, &actions));

  EXPECT_EQ(actions.code_points_to_delete, 0);
  EXPECT_EQ(std::u32string(actions.output), U"");
  ASSERT_TRUE(actions.persist_options != nullptr);
  EXPECT_EQ(std::u16string(actions.persist_options[0].key), u"key");
  EXPECT_EQ(std::u16string(actions.persist_options[0].value), u"value");
  EXPECT_EQ(actions.persist_options[0].scope, KM_CORE_OPT_KEYBOARD);
  EXPECT_EQ(actions.do_alert, KM_CORE_FALSE);
  EXPECT_EQ(actions.emit_keystroke, KM_CORE_FALSE);
  EXPECT_EQ(actions.new_caps_lock_state, KM_CORE_CAPS_UNCHANGED);
  EXPECT_EQ(actions.deleted_context, nullptr);

  // verify that data is copied
  EXPECT_NE(actions.persist_options[0].key, option.key);
  EXPECT_NE(actions.persist_options[0].value, option.value);

  // verify that we have a KM_CORE_OPTIONS_END term
  EXPECT_EQ(actions.persist_options[1].key, nullptr);
  EXPECT_EQ(actions.persist_options[1].value, nullptr);
  EXPECT_EQ(actions.persist_options[1].scope, KM_CORE_OPT_UNKNOWN);

  km::core::actions_dispose(actions);
}

/**
 * Note: this is just to print size of structures for cross-platform reference
 */
TEST(ActionItemListToActionsObject, MetaPrintSizeofTest) {
  km_core_actions act = {0};
  std::cout << "sizeof(km_core_actions): " << sizeof(km_core_actions) << std::endl;
  std::cout << "&km_core_actions.code_points_to_delete: " << ((intptr_t)(&act.code_points_to_delete)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.output: " << ((intptr_t)(&act.output)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.persist_options: " << ((intptr_t)(&act.persist_options)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.do_alert: " << ((intptr_t)(&act.do_alert)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.emit_keystroke: " << ((intptr_t)(&act.emit_keystroke)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.new_caps_lock_state: " << ((intptr_t)(&act.new_caps_lock_state)-(intptr_t)(&act)) << std::endl;
  std::cout << "&km_core_actions.deleted_context: " << ((intptr_t)(&act.deleted_context)-(intptr_t)(&act)) << std::endl;
  SUCCEED();
}
