/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the Keyboard API family of functions.
  Create Date:  30 Oct 2018
  Authors:      Tim Eves (TSE)
*/
#include <string>

#include <keyboardprocessor.h>
#include <experimental/filesystem>

namespace std {
  namespace filesystem = std::experimental::filesystem;
}


//#include "keyboard.hpp"

namespace
{
  std::filesystem::path const test_kb_path = "/a/dummy/keyboard.mock";
}

#define   try_status(expr) \
{auto __s = (expr); if (__s != KM_KBP_STATUS_OK) return 100*__LINE__+__s;}

int main(int, char *[])
{
  km_kbp_keyboard * test_kb = nullptr;

  try_status(km_kbp_keyboard_load(test_kb_path.string().c_str(), &test_kb));
  auto kb_attrs = km_kbp_keyboard_get_attrs(test_kb);
  if (kb_attrs->folder_path != test_kb_path.parent_path().string())
    return __LINE__;

  km_kbp_keyboard_dispose(test_kb);

  return 0;
}
