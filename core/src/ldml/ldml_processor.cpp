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

namespace km {
namespace kbp {

ldml_processor::ldml_processor(path const & kb_path, const std::vector<uint8_t> &data)
: abstract_processor(
    keyboard_attributes(kb_path.stem(), KM_KBP_LMDL_PROCESSOR_VERSION, kb_path.parent(), {})
  ), rawdata(data) // TODO-LDML: parse the data, don't just copy it
{
  // TODO-LDML: load the file from the buffer (KMXPlus format)
  // Note: kb_path is essentially debug metadata here
  assert(data.size() != 0);
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

  // Dump data. This also does some validation.
  dump_kmxplus_data(comp_keyboard);

  // A KMXPlus file is in the buffer (although more validation is required)
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
      // TODO-LDML: temporary code here
      // Don't want to do this work each time
      const kmx::PCOMP_KEYBOARD comp_keyboard = (kmx::PCOMP_KEYBOARD)rawdata.data();
      assert(comp_keyboard->dwFlags & KF_KMXPLUS);
      const kmx::COMP_KEYBOARD_EX* ex = reinterpret_cast<const kmx::COMP_KEYBOARD_EX*>(comp_keyboard);

      // printf("KMXPlus offset 0x%X, KMXPlus size 0x%X\n", ex->kmxplus.dpKMXPlus, ex->kmxplus.dwKMXPlusSize);
      const uint8_t* kmxplusdata = rawdata.data() + ex->kmxplus.dpKMXPlus;
      // Get out the SECT header
      const kmx::COMP_KMXPLUS_SECT *sect = kmx::as_kmxplus_sect(kmxplusdata);
      assert(sect != nullptr);
      assert(sect->header.ident == LDML_SECTIONID_SECT);
      KMX_DWORD offset;
      // Fill out the other sections we need.
      offset = sect->find(LDML_SECTIONID_STRS);
      assert(offset != 0); // or else section not found
      const kmx::COMP_KMXPLUS_STRS *strs = kmx::as_kmxplus_strs(kmxplusdata+offset);
      assert(strs->header.ident == LDML_SECTIONID_STRS);
      offset = sect->find(LDML_SECTIONID_KEYS);
      assert(offset != 0); // or else section not found
      const kmx::COMP_KMXPLUS_KEYS *keys = kmx::as_kmxplus_keys(kmxplusdata+offset);
      assert(keys->header.ident == LDML_SECTIONID_KEYS);
      // Look up the key
      const kmx::COMP_KMXPLUS_KEYS_ENTRY *key =  keys->find(vk, modifier_state);
      if (!key) {
        state->actions().commit(); // finish up and
        return KM_KBP_STATUS_OK; // Nothing to do- no key
      }
      // Prepare output chars
      KMX_DWORD len = 0;
      KMX_WCHAR out[BUFSIZ];
      if (key->flags && LDML_KEYS_FLAGS_EXTEND) {
        // It's a string.
        assert(nullptr != strs->get(key->to, out, BUFSIZ));
        // u_strlen()
        for(len=0; len<BUFSIZ && out[len]; len++);
      } else {
        UTF32 buf32[2];
        buf32[0] = key->to; // UTF-32
        buf32[1] = 0; // to avoid UMR warning
        UTF32 *sourceStart = &buf32[0];
        const UTF32 *sourceEnd = &buf32[1]; // Reference off the end. NULL to avoid UMR.
        UTF16 *targetStart = (UTF16*)out;
        const UTF16 *targetEnd = (UTF16*)out+BUFSIZ-1;
        ConversionResult result = ::ConvertUTF32toUTF16(&sourceStart, sourceEnd, &targetStart, targetEnd, strictConversion);
        assert(result == conversionOK);
        *targetStart = 0;
        len = 1; // TODO=LDML calculate
        // len = (targetStart - out);
        assert(len>=1 && len <= 2);
      }
      assert(len>0);
      assert(len<BUFSIZ);
      assert(out[len] == 0); // null termination

      for(KMX_DWORD i=0; i<len; i++) {
        state->context().push_character(out[i]);
        state->actions().push_character(out[i]);
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

km_kbp_status ldml_processor::validate() const { return KM_KBP_STATUS_OK; }

} // namespace kbp
} // namespace km
