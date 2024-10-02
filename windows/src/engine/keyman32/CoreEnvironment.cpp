#include "pch.h"

#define WINDOWS_PLATFORM_ENV u"windows hardware desktop native"

BOOL SetupCoreEnvironment(km_core_option_item **core_environment) {
  km_core_option_item *items = new km_core_option_item[6];

  items[0].scope = KM_CORE_OPT_ENVIRONMENT;
  items[0].key = KM_CORE_KMX_ENV_BASELAYOUT;
  items[0].value = reinterpret_cast<km_core_cu*>(Globals::get_BaseKeyboardName());

  items[1].scope = KM_CORE_OPT_ENVIRONMENT;
  items[1].key = KM_CORE_KMX_ENV_BASELAYOUTALT;
  items[1].value = reinterpret_cast<km_core_cu*>(Globals::get_BaseKeyboardNameAlt());

  items[2].scope = KM_CORE_OPT_ENVIRONMENT;
  items[2].key = KM_CORE_KMX_ENV_SIMULATEALTGR;
  items[2].value = Globals::get_SimulateAltGr() ? u"1" : u"0";

  items[3].scope = KM_CORE_OPT_ENVIRONMENT;
  items[3].key = KM_CORE_KMX_ENV_BASELAYOUTGIVESCTRLRALTFORRALT;
  items[3].value = KeyboardGivesCtrlRAltForRAlt() ? u"1" : u"0";

  items[4].scope = KM_CORE_OPT_ENVIRONMENT;
  items[4].key = KM_CORE_KMX_ENV_PLATFORM;
  items[4].value = WINDOWS_PLATFORM_ENV;

  items[5] = KM_CORE_OPTIONS_END;

  *core_environment = items;
  return TRUE;
}

void DeleteCoreEnvironment(km_core_option_item *core_environment) {
  // All keys and values are pointers to owned memory, so only need to delete
  // the array of options
  delete [] core_environment;
}
