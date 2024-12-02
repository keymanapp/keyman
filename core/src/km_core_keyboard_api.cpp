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
#include "kmx/kmx_processor.hpp"
#include "ldml/ldml_processor.hpp"
#include "mock/mock_processor.hpp"
#include "processor.hpp"
#include "utfcodec.hpp"

using namespace km::core;

namespace
{
  abstract_processor* processor_factory(path const & kb_name, const std::vector<uint8_t> & buf) {
    if (ldml_processor::is_handled(buf)) {
      return new ldml_processor(kb_name, buf);
    }
    if (kmx_processor::is_handled(buf)) {
      return new kmx_processor(kb_name, buf);
    }
    if (mock_processor::is_handled(buf)) {
      return new mock_processor(kb_name);
    }
    return new null_processor();
  }

}  // namespace

km_core_status
keyboard_load_from_blob_internal(
  const km_core_path_name kb_name,
  const std::vector<uint8_t> & buf,
  km_core_keyboard** keyboard
) {
  assert(keyboard);
  if (!keyboard) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }

  *keyboard = nullptr;
  try {
    abstract_processor* kp = processor_factory(kb_name, buf);
    km_core_status status  = kp->validate();
    if (status != KM_CORE_STATUS_OK) {
      delete kp;
      return status;
    }
    *keyboard = static_cast<km_core_keyboard*>(kp);
  } catch (std::bad_alloc&) {
    return KM_CORE_STATUS_NO_MEM;
  }
  return KM_CORE_STATUS_OK;
}

km_core_status
km_core_keyboard_load_from_blob(
  const km_core_path_name kb_name,
  const void* blob,
  const size_t blob_size,
  km_core_keyboard** keyboard
) {
  assert(keyboard);
  if (!keyboard || !blob) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }

  std::vector<uint8_t> buf((uint8_t*)blob, (uint8_t*)blob + blob_size);
  return keyboard_load_from_blob_internal(kb_name, buf, keyboard);
}

// TODO-web-core: Remove this code when we remove the deprecated km_core_keyboard_load method
// BEGIN DEPRECATED
#include <fstream>
std::vector<uint8_t> load_kmx_file(path const& kb_path) {
  std::vector<uint8_t> data;
  std::ifstream file(static_cast<std::string>(kb_path), std::ios::binary | std::ios::ate);
  if (!file.good()) {
    return std::vector<uint8_t>();
  }
  const std::streamsize size = file.tellg();
  if (size >= KMX_MAX_ALLOWED_FILE_SIZE) {
    return std::vector<uint8_t>();
  }

  file.seekg(0, std::ios::beg);

  data.resize((size_t)size);
  if (!file.read((char*)data.data(), size)) {
    return std::vector<uint8_t>();
  }

  file.close();
  return data;
}

KMN_DEPRECATED_API
km_core_status
km_core_keyboard_load(km_core_path_name kb, km_core_keyboard **keyboard)
{
  assert(keyboard);
  if (!keyboard || !kb) {
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  }

  path const kb_path(kb);
  try
  {
    abstract_processor* kp = nullptr;
    km_core_status status = KM_CORE_STATUS_OK;
    // Some legacy packages may include upper-case file extensions
    if (kb_path.suffix() == ".kmx" || kb_path.suffix() == ".KMX") {
      std::vector<uint8_t> buf = load_kmx_file(kb_path);
      status = keyboard_load_from_blob_internal(kb_path.stem().c_str(), buf, (km_core_keyboard**)&kp);
      if (status != KM_CORE_STATUS_OK) {
        return status;
      }
    } else if (kb_path.suffix() == ".mock") {
      kp = new mock_processor(kb_path);
    } else {
      kp = new null_processor();
    }
    status = kp->validate();
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
// END DEPRECATED

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
