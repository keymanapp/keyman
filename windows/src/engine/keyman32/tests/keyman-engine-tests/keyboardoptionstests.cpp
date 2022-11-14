
#include "pch.h"
#include <locale>
#include <codecvt>
#include <string>
#include <iostream>

// Test UpdateKeyboardOptionsCore, also uses SaveKeyboardOptionsCore
TEST(KEYBOARDOPTIONS, UpdateKeyboardOptionsCore) {
  LPINTKEYBOARDINFO kp  = new INTKEYBOARDINFO;
  memset(kp, 0, sizeof(INTKEYBOARDINFO));

  km_kbp_option_item test_env_opts[] = {
      {u"__test_point", u"not tiggered", KM_KBP_OPT_KEYBOARD}, {u"hello", u"-", KM_KBP_OPT_ENVIRONMENT}, KM_KBP_OPTIONS_END};

  km_kbp_path_name dummyPath = L"dummyActions.mock";
  EXPECT_EQ(km_kbp_keyboard_load(dummyPath, &kp->lpCoreKeyboard), KM_KBP_STATUS_OK);
  EXPECT_EQ(km_kbp_state_create(kp->lpCoreKeyboard, test_env_opts, &kp->lpCoreKeyboardState), KM_KBP_STATUS_OK);

  kp->lpCoreKeyboardOptions           = SaveKeyboardOptionsCore(kp);
  // No Change
  EXPECT_FALSE(UpdateKeyboardOptionsCore(kp->lpCoreKeyboardState, kp->lpCoreKeyboardOptions));
  std::u16string value = kp->lpCoreKeyboardOptions[0].value;
  std::u16string expectedValue = u"not tiggered";
  EXPECT_TRUE(value == expectedValue);

  km_kbp_option_item update_key_opts[] = {{u"__test_point", u"triggered", KM_KBP_OPT_KEYBOARD}, KM_KBP_OPTIONS_END};
  EXPECT_EQ(km_kbp_state_options_update(kp->lpCoreKeyboardState, update_key_opts), KM_KBP_STATUS_OK);
  // Change value to triggered
  EXPECT_TRUE(UpdateKeyboardOptionsCore(kp->lpCoreKeyboardState, kp->lpCoreKeyboardOptions));
  value         = kp->lpCoreKeyboardOptions[0].value;
  expectedValue = u"triggered";
  EXPECT_TRUE(value == expectedValue);
  DisposeKeyboardOptionsCore(&kp->lpCoreKeyboardOptions);
  ReleaseStateMemoryCore(&kp->lpCoreKeyboardState);
  ReleaseKeyboardMemoryCore(&kp->lpCoreKeyboard);
  delete kp;

}

// Test SaveKeyboardOptionsCore and RestoreKeyboardOptionsCORE
TEST(KEYBOARDOPTIONS, SaveRestoreKeyboardOptionsCore) {

  LPINTKEYBOARDINFO kp = new INTKEYBOARDINFO;
  memset(kp, 0, sizeof(INTKEYBOARDINFO));

  km_kbp_option_item test_env_opts[] = {
      {u"__test_point", u"not tiggered", KM_KBP_OPT_KEYBOARD}, {u"hello", u"-", KM_KBP_OPT_ENVIRONMENT}, KM_KBP_OPTIONS_END};
  km_kbp_path_name dummyPath = L"dummyActions.mock";
  EXPECT_EQ(km_kbp_keyboard_load(dummyPath, &kp->lpCoreKeyboard), KM_KBP_STATUS_OK);
  EXPECT_EQ(km_kbp_state_create(kp->lpCoreKeyboard, test_env_opts, &kp->lpCoreKeyboardState), KM_KBP_STATUS_OK);

  km_kbp_option_item *SavedKBDOptions = SaveKeyboardOptionsCore(kp);

  std::u16string value         = SavedKBDOptions[0].value;
  std::u16string expectedValue = u"not tiggered";
  km_kbp_option_item update_key_opts[] = {{u"__test_point", u"triggered", KM_KBP_OPT_KEYBOARD}, KM_KBP_OPTIONS_END};
  EXPECT_EQ(km_kbp_state_options_update(kp->lpCoreKeyboardState, update_key_opts), KM_KBP_STATUS_OK);

  km_kbp_option_item *NewKBDOptions = SaveKeyboardOptionsCore(kp);
  value = NewKBDOptions[0].value;
  expectedValue = u"triggered";
  EXPECT_TRUE(value == expectedValue);
  km_kbp_cp const *retValue = nullptr;
  EXPECT_TRUE(RestoreKeyboardOptionsCore(kp->lpCoreKeyboardState, SavedKBDOptions));
  EXPECT_EQ(km_kbp_state_option_lookup(kp->lpCoreKeyboardState, KM_KBP_OPT_KEYBOARD, test_env_opts[0].key, &retValue), KM_KBP_STATUS_OK);

  value         = retValue;
  expectedValue                     = u"not tiggered";
  EXPECT_TRUE(value == expectedValue);

  DisposeKeyboardOptionsCore(&NewKBDOptions);
  DisposeKeyboardOptionsCore(&SavedKBDOptions);

  ReleaseStateMemoryCore(&kp->lpCoreKeyboardState);
  ReleaseKeyboardMemoryCore(&kp->lpCoreKeyboard);
  delete kp;
}

// Test SetupCoreEnvironment
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
  delete[] expected_items;

  DeleteCoreEnvironment(core_env_opts);
  ReleaseStateMemoryCore(&kp->lpCoreKeyboardState);
  ReleaseKeyboardMemoryCore(&kp->lpCoreKeyboard);
  delete kp;
}
