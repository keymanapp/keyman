/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the Keyboard API family of functions.
  Create Date:  30 Oct 2018
  Authors:      Tim Eves (TSE)
*/
#include <string>

#include <keyman/keyboardprocessor.h>
#include "path.hpp"

//#include "keyboard.hpp"

namespace
{
  km::kbp::path const test_kb_path = "/a/dummy/keyboard.mock";
}

#define   try_status(expr) \
{auto __s = (expr); if (__s != KM_KBP_STATUS_OK) return 100*__LINE__+__s;}

int main(int, char *[])
{
  km_kbp_keyboard * test_kb = nullptr;
  km_kbp_keyboard_attrs const * kb_attrs = nullptr;

  try_status(km_kbp_keyboard_load(test_kb_path.c_str(), &test_kb));
  try_status(km_kbp_keyboard_get_attrs(test_kb, &kb_attrs));
  if (kb_attrs->folder_path != test_kb_path.parent())
    return __LINE__;

  km_kbp_keyboard_dispose(test_kb);

  return 0;
}
