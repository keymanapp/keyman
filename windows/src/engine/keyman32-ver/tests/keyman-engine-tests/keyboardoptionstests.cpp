
#include "pch.h"
#include <locale>
#include <codecvt>
#include <string>
#include <iostream>


// Test SetupCoreEnvironment and also test km_kbp_state_options_update
TEST(KEYBOARDOPTIONS, SetupCoreEnvironment) {
  LPINTKEYBOARDINFO kp = new INTKEYBOARDINFO;
  memset(kp, 0, sizeof(INTKEYBOARDINFO));

  km_kbp_option_item *core_env_opts = nullptr;

  km_kbp_path_name dummyPath = L"dummyActions.mock";

  EXPECT_EQ(SetupCoreEnvironment(&core_env_opts), TRUE);
  EXPECT_EQ(km_kbp_keyboard_load(dummyPath, &kp->lpCoreKeyboard), KM_KBP_STATUS_OK);
  EXPECT_EQ(km_kbp_state_create(kp->lpCoreKeyboard, core_env_opts, &kp->lpCoreKeyboardState), KM_KBP_STATUS_OK);

  km_kbp_option_item *expected_items = new km_kbp_option_item[5];
  // These are taken from SetupCoreEnvironment in CoreEnvironment.cpp
  expected_items[0].scope = KM_KBP_OPT_ENVIRONMENT;
  expected_items[0].key   = KM_KBP_KMX_ENV_BASELAYOUT;
  expected_items[0].value = reinterpret_cast<km_kbp_cp *>(Globals::get_BaseKeyboardName());

  expected_items[1].scope = KM_KBP_OPT_ENVIRONMENT;
  expected_items[1].key   = KM_KBP_KMX_ENV_BASELAYOUTALT;
  expected_items[1].value = reinterpret_cast<km_kbp_cp *>(Globals::get_BaseKeyboardNameAlt());

  expected_items[2].scope = KM_KBP_OPT_ENVIRONMENT;
  expected_items[2].key   = KM_KBP_KMX_ENV_SIMULATEALTGR;
  expected_items[2].value = Globals::get_SimulateAltGr() ? u"1" : u"0";

  expected_items[3].scope = KM_KBP_OPT_ENVIRONMENT;
  expected_items[3].key   = KM_KBP_KMX_ENV_BASELAYOUTGIVESCTRLRALTFORRALT;
  expected_items[3].value = KeyboardGivesCtrlRAltForRAlt() ? u"1" : u"0";

  expected_items[4] = KM_KBP_OPTIONS_END;
  km_kbp_cp const* retValue = nullptr;
  std::u16string value      = u"";
  std::u16string expectedValue = u"";

  for (int i = 0; i < 4; ++i) {
    EXPECT_EQ(km_kbp_state_option_lookup(kp->lpCoreKeyboardState, KM_KBP_OPT_ENVIRONMENT, expected_items[i].key, &retValue), KM_KBP_STATUS_OK);
    value = retValue;
    expectedValue = expected_items[i].value;
    EXPECT_TRUE(expectedValue == value);
  }

  // Now verify update items call works when items already exist
  expected_items[0].value = u"updated";
  expected_items[3].value = u"updated";

  EXPECT_EQ(km_kbp_state_options_update(kp->lpCoreKeyboardState, expected_items), KM_KBP_STATUS_OK);

  for (int i = 0; i < 4; ++i) {
    EXPECT_EQ(
        km_kbp_state_option_lookup(kp->lpCoreKeyboardState, KM_KBP_OPT_ENVIRONMENT, expected_items[i].key, &retValue),
        KM_KBP_STATUS_OK);
    value         = retValue;
    expectedValue = expected_items[i].value;
    EXPECT_TRUE(expectedValue == value);
  }

  delete[] expected_items;

  DeleteCoreEnvironment(core_env_opts);
  ReleaseStateMemoryCore(&kp->lpCoreKeyboardState);
  ReleaseKeyboardMemoryCore(&kp->lpCoreKeyboard);
  delete kp;
}
