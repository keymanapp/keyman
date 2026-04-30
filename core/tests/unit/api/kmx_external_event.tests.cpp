/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Ross Cruickshank on 2023-07-28
 *
 * Keyman Core - Tests for external event processing for the kmx processor
 */

#include <map>
#include <iostream>

#include <kmx/kmx_processevent.h>

#include "path.hpp"
#include "state.hpp"

#include "../helpers/core_test_helpers.h"

/**
 * This test will test the infrastructure around the external event processing
 * The functions tested are:
 * - km_core_event with the event KM_CORE_EVENT_KEYBOARD_ACTIVATED
 */

using namespace km::core::kmx;

TEST(ExternalEventTests, TestExternalEvent) {

  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;

  km::core::path full_path = km::core::path(test_dir / "k_0702___caps_always_off.kmx");

  auto blob = km::tests::load_kmx_file(full_path.native().c_str());
  ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(full_path.stem().c_str(), blob.data(), blob.size(), &test_kb));

  // Setup state, environment
  ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));

  // Set up external events to test the processing.
  uint32_t event = KM_CORE_EVENT_KEYBOARD_ACTIVATED;
  // For this particular test we want to have the capslock state as on.
  // then after the km_core_event the caps lock should be off as the load
  // keyboard is a caps always off keyboard

  ASSERT_STATUS_OK(km_core_event(test_state, event, nullptr));
  // The action to turn capslock off must be in the actions list.
  ASSERT_TRUE(km::tests::action_items(test_state, {{KM_CORE_IT_CAPSLOCK, {0,}, {0}}, {KM_CORE_IT_END}}));

  km_core_state_dispose(test_state);
  km_core_keyboard_dispose(test_kb);

}
