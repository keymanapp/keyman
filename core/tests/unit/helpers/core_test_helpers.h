#include <gtest/gtest.h>

#include "emscripten_filesystem.h"
#include "load_kmx_file.hpp"
#include "action_items.hpp"
#include "debug_items.hpp"
#include "printers.hpp"

#define ASSERT_STATUS_OK(a) ASSERT_EQ((a), KM_CORE_STATUS_OK)

namespace km::tests {

void compare_context(km_core_context *app_context, const km_core_cu* expected_final_app_context);

};

/**
 * argv[0] - path to test folder for locating fixtures
 */
extern km::core::path test_dir;

/**
 * Standard list of empty test environment options
 */
extern km_core_option_item test_empty_env_opts[];
