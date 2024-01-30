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
#include <vector>

#include "keyman_core.h"

#include "keyboard.hpp"
#include "processor.hpp"
#include "kmx/kmx_processor.hpp"
#include "ldml/ldml_processor.hpp"
#include "mock/mock_processor.hpp"

using namespace km::core;

namespace
{
  abstract_processor * processor_factory(path const & kb_path) {
    // Some legacy packages may include upper-case file extensions
    // TODO-LDML: move file io out of core and into engine
    if (kb_path.suffix() == ".kmx" || kb_path.suffix() == ".KMX") {
      std::vector<uint8_t> buf;
      if(ldml_processor::is_kmxplus_file(kb_path, buf)) {
        abstract_processor * result = new ldml_processor(kb_path, buf);
        return result;
      }
      return new kmx_processor(kb_path);
    }
    else if (kb_path.suffix() == ".mock") {
      return new mock_processor(kb_path);
    }
    else {
      return new null_processor();
    }
  }


}
km_core_status
km_core_keyboard_load(km_core_path_name kb_path, km_core_keyboard **keyboard)
{
  assert(keyboard);
  if (!keyboard)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  try
  {
    abstract_processor *kp = processor_factory(kb_path);
    km_core_status status = kp->validate();
    if (status != KM_CORE_STATUS_OK) {
      delete kp;
      return status;
    }
    *keyboard = static_cast<km_core_keyboard *>(kp);
  }
  catch (std::bad_alloc &)
  {
    return KM_CORE_STATUS_NO_MEM;
  }
  return KM_CORE_STATUS_OK;
}

void
km_core_keyboard_dispose(km_core_keyboard *keyboard)
{
  delete keyboard;
}

km_core_status
km_core_keyboard_get_attrs(km_core_keyboard const *keyboard,
                          km_core_keyboard_attrs const **out)
{
  assert(keyboard); assert(out);
  if (!keyboard || !out)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  *out = &keyboard->keyboard();
  return KM_CORE_STATUS_OK;
}

km_core_status
km_core_keyboard_get_key_list(km_core_keyboard const *keyboard,
                             km_core_keyboard_key **out)
{
  assert(keyboard); assert(out);
  if (!keyboard || !out)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  *out = keyboard->get_key_list();
  return KM_CORE_STATUS_OK;
}

void km_core_keyboard_key_list_dispose(km_core_keyboard_key *key_list)
{
  delete[] key_list;
}

km_core_status km_core_keyboard_get_imx_list(
  km_core_keyboard const *keyboard,
  km_core_keyboard_imx** imx_list
) {
  assert(keyboard); assert(imx_list);
  if (!keyboard || !imx_list) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }

  *imx_list = keyboard->get_imx_list();
  return KM_CORE_STATUS_OK;
}

void km_core_keyboard_imx_list_dispose(km_core_keyboard_imx *imx_list)
{
  if(!imx_list) {
    return;
  }

  km_core_keyboard_imx *imx_rule_it = imx_list;
  for (; imx_rule_it->library_name; ++imx_rule_it) {
    delete [] imx_rule_it->library_name; // from u16dup
    delete [] imx_rule_it->function_name; // from u16dup
  }
  delete[] imx_list;
}
