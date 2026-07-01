/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Helpers for comparing context and creating standard environment
 */

#include "action_items.hpp"
#include "context.hpp"
#include "core_test_helpers.h"

km_core_option_item test_empty_env_opts[] = {
  KM_CORE_OPTIONS_END
};

namespace km::tests {

void compare_context(km_core_context *app_context, const km_core_cu* expected_final_app_context) {
  // Compare context items -- to ensure no markers have leaked into app context
  km_core_context_item *actual_final_app_context_items = nullptr, *expected_final_app_context_items = nullptr;
  ASSERT_STATUS_OK(km_core_context_get(app_context, &actual_final_app_context_items));
  ASSERT_STATUS_OK(context_items_from_utf16(expected_final_app_context, &expected_final_app_context_items));

  for(int i = 0; actual_final_app_context_items[i].type != KM_CORE_CT_END || expected_final_app_context_items[i].type != KM_CORE_CT_END; i++) {
    ASSERT_EQ(actual_final_app_context_items[i].type, expected_final_app_context_items[i].type);
    // union so testing character is sufficient to do both char + marker types
    ASSERT_EQ(actual_final_app_context_items[i].character, expected_final_app_context_items[i].character);
  }

  km_core_context_items_dispose(actual_final_app_context_items);
  km_core_context_items_dispose(expected_final_app_context_items);
}

};
