
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

  delete NewKBDOptions;
  delete SavedKBDOptions;
  delete kp;
}
