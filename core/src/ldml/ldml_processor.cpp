/*
  Copyright:    © SIL International.
  Description:  This is an implementation of the LDML keyboard spec 3.0.
  Create Date:  5 Aug 2023
  Authors:      Marc Durdin (MD)
*/

#include <fstream>
#include "ldml/ldml_processor.hpp"
#include "state.hpp"
#include "kmx_file.h"
#include "kmx/kmx_plus.h"
#include "ldml/keyboardprocessor_ldml.h"

// extern "C" {
// #include "../../../common/windows/cpp/src/ConvertUTF.c"
// }

// HACK
enum ConversionResult {
  conversionOK,
  conversionNotOk
};
enum ConversionFlags {
  strictConversion
};

typedef KMX_WCHAR UTF16;
typedef KMX_DWORD UTF32;

/**
 * This is a temporary patch for now.
 * API surface somewhat modelled after ConvertUTF.h
 */
ConversionResult ConvertUTF32toUTF16(
		UTF32** sourceStart, const UTF32* sourceEnd,
		UTF16** targetStart, const UTF16* targetEnd, const ConversionFlags /*flags*/) {
    if(sourceEnd > (*sourceStart+1)) return conversionNotOk; // Don't support >1 char
    if(**sourceStart & 0xFFFF0000) {
      // Don't support supplemental chars yet
      return conversionNotOk;
    }
    *((*targetStart)++) = (UTF16)((*(*sourceStart)++) & 0xFFFF); // BMP

    assert(*sourceStart==sourceEnd);
    assert(*targetStart <= targetEnd);
    return(conversionOK);
}


namespace {
  constexpr km_kbp_attr const engine_attrs = {
    256,
    KM_KBP_LIB_CURRENT,
    KM_KBP_LIB_AGE,
    KM_KBP_LIB_REVISION,
    KM_KBP_TECH_LDML,
    "SIL International"
  };
}

/**
 * @brief Usage: `KMXPLUS_PRINTF(("str: %s\n", "something"));`
 * Note double parens
 * \def KMXPLUS_DEBUG
 */
#if KMXPLUS_DEBUG
#include <iostream>
#define KMXPLUS_NOTEQUAL(expect, actual) std::cerr << __FILE__ << ":" << __LINE__ << ": ASSERT FAILED: " \
              << #actual << "=" << (actual) << ", expected " << (expect) << std::endl
#define KMXPLUS_PRINTLN(msg) std::cerr  << __FILE__ << ":" << __LINE__ << ": " msg << std::endl;
#else
#define KMXPLUS_NOTEQUAL(expect, actual)
#define KMXPLUS_PRINTLN(msg)
#endif

namespace km {
namespace kbp {

ldml_processor::ldml_processor(path const & kb_path, const std::vector<uint8_t> &data)
: abstract_processor(
    keyboard_attributes(kb_path.stem(), KM_KBP_LMDL_PROCESSOR_VERSION, kb_path.parent(), {})
  ), _valid(false), vkey_to_string()
{

/**
 * Assert something, just in this function
 * If fails, print a message and exit as invalid
 * \def KMXPLUS_ASSERT
 */
#define KMXPLUS_ASSERT(expect,actual) if ((expect) != (actual)) { \
    KMXPLUS_NOTEQUAL(expect, actual); \
    _valid = false; \
    return; \
  }
  const kmx::COMP_KMXPLUS_SECT *sect = nullptr;
  const kmx::COMP_KMXPLUS_STRS *strs = nullptr;
  const kmx::COMP_KMXPLUS_KEYS *keys = nullptr;

  // TODO-LDML: load the file from the buffer (KMXPlus format)
  // Note: kb_path is essentially debug metadata here
  KMXPLUS_ASSERT(true, data.size() > sizeof(kmx::COMP_KEYBOARD_EX));

  // Locate the structs here, but still retain ptrs to the raw structs.
  const kmx::PCOMP_KEYBOARD comp_keyboard = (kmx::PCOMP_KEYBOARD)data.data();
  KMXPLUS_ASSERT(true, !!(comp_keyboard->dwFlags & KF_KMXPLUS));
  const kmx::COMP_KEYBOARD_EX* ex = reinterpret_cast<const kmx::COMP_KEYBOARD_EX*>(comp_keyboard);

  // validate size and offset
  KMXPLUS_ASSERT(true, ex->kmxplus.dwKMXPlusSize >  LDML_LENGTH_SECT);
  KMXPLUS_ASSERT(true, ex->kmxplus.dwKMXPlusSize <  (data.size() - sizeof(kmx::COMP_KEYBOARD_EX)));
  KMXPLUS_ASSERT(true, ex->kmxplus.dpKMXPlus     >= sizeof(kmx::COMP_KEYBOARD_EX));
  KMXPLUS_ASSERT(true, ex->kmxplus.dpKMXPlus     <  data.size());

  // calculate pointers to start and end of kmxplus
  const uint8_t * const kmxplusdata   = data.data() + ex->kmxplus.dpKMXPlus;      // Start of + data
  // const uint8_t* kmxpluslimit  = kmxplusdata    + ex->kmxplus.dwKMXPlusSize;  // End of   + data

  // Now load sections.

  // This will validate (and possibly print) all data.
  // TODO-LDML: we will be replacing this validation with validation-as-we-go below.
  KMXPLUS_ASSERT(true, kmx::validate_kmxplus_data(kmxplusdata));

  // Get out the SECT header
  {
    const KMX_DWORD offset = 0;
    sect = kmx::as_kmxplus_sect(kmxplusdata+offset);
    KMXPLUS_ASSERT(true, sect != nullptr);
    // Specified data size fits in total
    KMXPLUS_ASSERT(true, (offset+sect->header.size) < ex->kmxplus.dwKMXPlusSize);
  }

  // Fill out the other sections we need.
  {
    const KMX_DWORD offset = sect->find(LDML_SECTIONID_STRS);
    KMXPLUS_ASSERT(true, offset != 0);
    KMXPLUS_ASSERT(true, offset < ex->kmxplus.dwKMXPlusSize);
    strs = kmx::as_kmxplus_strs(kmxplusdata+offset);
    KMXPLUS_ASSERT(true, strs != nullptr);
    // Specified data size fits in total
    KMXPLUS_ASSERT(true, (offset+strs->header.size) < ex->kmxplus.dwKMXPlusSize);
  }

  {
    const KMX_DWORD offset = sect->find(LDML_SECTIONID_KEYS);
    KMXPLUS_ASSERT(true, offset != 0);
    keys = kmx::as_kmxplus_keys(kmxplusdata+offset);
    KMXPLUS_ASSERT(true, keys != nullptr);
    // Specified data size fits in total
    KMXPLUS_ASSERT(true, (offset+keys->header.size) < ex->kmxplus.dwKMXPlusSize);

    // read all keys into array
    for (KMX_DWORD i=0; i<keys->count; i++) {
      const kmx::COMP_KMXPLUS_KEYS_ENTRY &entry = keys->entries[i];
      KMX_DWORD len = 0;
      KMX_WCHAR out[BUFSIZ];
      if (entry.flags && LDML_KEYS_FLAGS_EXTEND) {
        KMXPLUS_ASSERT(false, nullptr == strs->get(entry.to, out, BUFSIZ));
        for(len=0; len<BUFSIZ && out[len]; len++);
      } else {
        UTF32 buf32[2];
        buf32[0] = entry.to; // UTF-32
        buf32[1] = 0; // to avoid UMR warning
        UTF32 *sourceStart = &buf32[0];
        const UTF32 *sourceEnd = &buf32[1]; // Reference off the end. NULL to avoid UMR.
        UTF16 *targetStart = (UTF16*)out;
        const UTF16 *targetEnd = (UTF16*)out+BUFSIZ-1;
        ConversionResult result = ::ConvertUTF32toUTF16(&sourceStart, sourceEnd, &targetStart, targetEnd, strictConversion);
        KMXPLUS_ASSERT(conversionOK, result);
        *targetStart = 0;
        len = 1; // TODO=LDML calculate
        // len = (targetStart - out);
        KMXPLUS_ASSERT(true, len>=1 && len <= 2);
      }
      KMXPLUS_ASSERT(true, len>0 && len < BUFSIZ);
      KMXPLUS_ASSERT(0, out[len]); // null termination
      ldml_vkey_id vkey_id((km_kbp_virtual_key)entry.vkey, (uint16_t)entry.mod);
      vkey_to_string[vkey_id] = std::u16string(out, len); // assign the string
    }
  }
  KMXPLUS_PRINTLN("_valid = true");

#undef KMXPLUS_ASSERT
  _valid = true;
}

bool ldml_processor::is_kmxplus_file(path const & kb_path, std::vector<uint8_t>& data) {
// TODO-LDML: we should refactor all the core components to delegate file loading
//            to the Engine, which requires an API change, but this makes delivery
//            of keyboard files more flexible under more WASM.

  std::ifstream file(static_cast<std::string>(kb_path), std::ios::binary | std::ios::ate);
  if(!file.good()) {
    return false;
  }
  const std::streamsize size = file.tellg();
  if(size >= KMX_MAX_ALLOWED_FILE_SIZE) {
    return false;
  }

  file.seekg(0, std::ios::beg);

  data.resize((size_t)size);
  if(!file.read((char *) data.data(), size)) {
    return false;
  }

  file.close();

  const kmx::PCOMP_KEYBOARD comp_keyboard = (kmx::PCOMP_KEYBOARD)data.data();

  if(comp_keyboard->dwIdentifier != KMX_DWORD(FILEID_COMPILED)) {
    return false;
  }

  if(comp_keyboard->dwFileVersion < VERSION_160 || (comp_keyboard->dwFlags & KF_KMXPLUS) == 0) {
    return false;
  }

  // A KMXPlus file is in the buffer (although more validation is required and will
  // be done in the constructor)
  return true;
}

km_kbp_status
ldml_processor::process_queued_actions(
  km_kbp_state *state
) {
  assert(state);
  if (!state)
    return KM_KBP_STATUS_INVALID_ARGUMENT;
  // TODO Implement
  return KM_KBP_STATUS_OK;
}

bool ldml_processor::queue_action(
  km_kbp_state * state,
  km_kbp_action_item const* action_item
)
{
  assert(state);
  assert(action_item);
  if ((!state) || (!action_item))
    return false;
  return false;
}

km_kbp_status
ldml_processor::process_event(
  km_kbp_state *state,
  km_kbp_virtual_key vk,
  uint16_t modifier_state,
  uint8_t is_key_down,
  uint16_t /*event_flags*/ // TODO-LDML: unused... for now...
) {
  assert(state);
  if (!state)
    return KM_KBP_STATUS_INVALID_ARGUMENT;

  if (!is_key_down) {
    // TODO: Implement caps lock handling
    state->actions().clear();
    state->actions().commit();
    return KM_KBP_STATUS_OK;
  }

  try {
    // At the start of every process_event always clear the action_items
    state->actions().clear();

    switch (vk) {
    case KM_KBP_VKEY_BKSP:
      state->context().pop_back();
      state->actions().push_backspace(KM_KBP_BT_UNKNOWN); // Assuming we don't know the character
      break;
    default:
      // Look up the key
      const ldml_vkey_id vkey_id(vk, modifier_state);
      const auto key = vkey_to_string.find(vkey_id);
      if (key == vkey_to_string.end()) {
        // not found
        state->actions().commit(); // finish up and
        return KM_KBP_STATUS_OK; // Nothing to do- no key
      }
      const std::u16string &str = key->second;
      for(size_t i=0; i<str.length(); i++) {
        state->context().push_character(str[i]);
        state->actions().push_character(str[i]);
      }
    }
    state->actions().commit();
  } catch (std::bad_alloc &) {
    state->actions().clear();
    return KM_KBP_STATUS_NO_MEM;
  }

  return KM_KBP_STATUS_OK;
}

km_kbp_attr const & ldml_processor::attributes() const {
  return engine_attrs;
}

km_kbp_keyboard_key  * ldml_processor::get_key_list() const {
  km_kbp_keyboard_key* key_list = new km_kbp_keyboard_key(KM_KBP_KEYBOARD_KEY_LIST_END);
  return key_list;
}

km_kbp_keyboard_imx  * ldml_processor::get_imx_list() const {
  km_kbp_keyboard_imx* imx_list = new km_kbp_keyboard_imx(KM_KBP_KEYBOARD_IMX_END);
  return imx_list;
}

km_kbp_context_item * ldml_processor::get_intermediate_context() {
  km_kbp_context_item *citems = new km_kbp_context_item(KM_KBP_CONTEXT_ITEM_END);
  return citems;
}

km_kbp_status ldml_processor::validate() const {
  return _valid ? KM_KBP_STATUS_OK : KM_KBP_STATUS_INVALID_KEYBOARD;
}

} // namespace kbp
} // namespace km
