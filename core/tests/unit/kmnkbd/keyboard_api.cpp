/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the Keyboard API family of functions.
  Create Date:  30 Oct 2018
  Authors:      Tim Eves (TSE)
*/
#include <string>

#include "keyman_core.h"
#include "path.hpp"

//#include "keyboard.hpp"

namespace
{
  km::core::path const test_kb_path = "/a/dummy/keyboard.mock";
}

#define   try_status(expr) \
{auto __s = (expr); if (__s != KM_CORE_STATUS_OK) return 100*__LINE__+__s;}

int main(int, char *[])
{
  km_core_keyboard * test_kb = nullptr;
  km_core_keyboard_attrs const * kb_attrs = nullptr;
  km_core_keyboard_key * kb_key_list = nullptr;
  km_core_keyboard_imx * kb_imx_list = nullptr;

  try_status(km_core_keyboard_load(test_kb_path.c_str(), &test_kb));
  try_status(km_core_keyboard_get_attrs(test_kb, &kb_attrs));
  try_status(km_core_keyboard_get_key_list(test_kb,&kb_key_list));
  try_status(km_core_keyboard_get_imx_list(test_kb,&kb_imx_list));

  if (kb_attrs->folder_path != test_kb_path.parent())
    return __LINE__;

  km_core_keyboard_dispose(test_kb);
  km_core_keyboard_key_list_dispose(kb_key_list);
  km_core_keyboard_imx_list_dispose(kb_imx_list);

  return 0;
}
