/*
  Copyright:    © 2018 SIL International.
  Description:  Implementation of the keyboard API functions using internal
                data structures and functions.
  Create Date:  1 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      1  Sep 2018 - TSE - Initial implementation.
                5  Oct 2018 - TSE - Refactor out adaptor and internal classes
                                    into keyboard.hpp
*/
#include <cassert>
#include <algorithm>
#include <experimental/filesystem>
#include <iterator>
#include <sstream>
#include <unordered_map>
#include <string>
#include <vector>

#include <keyman/keyboardprocessor.h>
#include <json.hpp>

#include "keyboard.hpp"
#include "option.hpp"



km_kbp_status
km_kbp_keyboard_load(km_kbp_path_name kb_path,
                                   km_kbp_keyboard **keyboard)
{
  assert(keyboard);
  if (!keyboard)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  //auto stat = std::filesystem::status(kb_path);
  //
  // if (stat.type() != std::filesystem::file_type::regular)
  //   return KM_KBP_STATUS_INVALID_ARGUMENT;

  try
  {
    *keyboard = static_cast<km_kbp_keyboard *>(new km::kbp::keyboard(kb_path));
  }
  catch (std::bad_alloc) 
  {
    return KM_KBP_STATUS_NO_MEM;
  }
  return KM_KBP_STATUS_OK;
}

void
km_kbp_keyboard_dispose(km_kbp_keyboard *keyboard)
{
  delete keyboard;
}

km_kbp_status
km_kbp_keyboard_get_attrs(km_kbp_keyboard const *keyboard,
                          km_kbp_keyboard_attrs const **out)
{
  assert(keyboard); assert(out);
  if (!keyboard || !out)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  *out = keyboard;
  return KM_KBP_STATUS_OK;
}
