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
#include "kmx/kmx_xstring.h"
#include "ldml/keyboardprocessor_ldml.h"
#include "kmx/kmx_file_validator.hpp"
#include "kmx/kmx_processevent.h" // for DebugLog

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

using km::kbp::kmx::ShouldDebug; // for DebugLog

namespace km {
namespace kbp {


ldml_processor::ldml_processor(path const & kb_path, const std::vector<uint8_t> &data)
: abstract_processor(
    keyboard_attributes(kb_path.stem(), KM_KBP_LMDL_PROCESSOR_VERSION, kb_path.parent(), {})
  ), _valid(false), transforms(), keys()
{

  if(data.size() <= sizeof(kmx::COMP_KEYBOARD_EX)) {
    DebugLog("data.size %zu too small", data.size());
    return;
  }

//   // Locate the structs here, but still retain ptrs to the raw structs.
  kmx::KMX_FileValidator* comp_keyboard = (kmx::KMX_FileValidator*)data.data();

  // Perform the standard validation
  if(!comp_keyboard->VerifyKeyboard(data.size())) {
    DebugLog("VerifyKeyboard() returned false");
    return;
  }

  kmx::kmx_plus kplus(comp_keyboard, data.size()); // slices to a COMP_KEYBOARD

  if(!kplus.is_valid()) {
    DebugLog("kmx_plus.is_valid is false");
    return;
  }

  // Now, if we have keys, use them.
  if (kplus.keys != nullptr) {
    // read all keys into array
    for (KMX_DWORD i=0; i<kplus.keys->count; i++) {
      std::u16string str;
      const kmx::COMP_KMXPLUS_KEYS_ENTRY &entry = kplus.keys->entries[i];
      if (entry.flags && LDML_KEYS_FLAGS_EXTEND) {
        if (nullptr == kplus.strs) {
          DebugLog("for keys: kplus.strs == nullptr"); // need a string table to get strings
          return;
        }
        str = kplus.strs->get(entry.to);
      } else {
        str = entry.get_string();
      }
      keys.add((km_kbp_virtual_key)entry.vkey, (uint16_t)entry.mod, str);
    }
  } // else: no keys! but still valid. Just, no keys.
  if (kplus.tran != nullptr) {
    if (nullptr == kplus.elem) {
      DebugLog("for tran: kplus.elem == nullptr");
      return;
    }
    if (nullptr == kplus.strs) {
      DebugLog("for tran: kplus.strs == nullptr"); // need a string table to get strings
      return;
    }
    for (KMX_DWORD i = 0; i < kplus.tran->count; i++) {
      const kmx::COMP_KMXPLUS_TRAN_ENTRY &entry = kplus.tran->entries[i];
      // 'to' is always a string, unlike keys
      const std::u16string tostr = kplus.strs->get(entry.to);
      // now, fetch the 'from'
      ldml::string_list list;

      // TODO-LDML: before=
      // TODO-LDML: error=

      // process From
      const KMX_DWORD elemNo = entry.from;
      if (elemNo >= kplus.elem->count) {
        DebugLog("tran[%d].from = %d, out of range for elem->count=%d", i, elemNo, kplus.elem->count);
        return;
      }

      // TODO-LDML: refactor this
      KMX_DWORD elemListLength                       = 0;
      const kmx::COMP_KMXPLUS_ELEM_ELEMENT *elemList = kplus.elem->getElementList(elemNo, elemListLength);
      if (elemList == nullptr) {
        DebugLog("tran[%d].from = %d, could not load element list", i, elemNo);
        return;
      }
      if (elemListLength == 0) {
        DebugLog("tran[%d].from = %d, element list has 0 length", i, elemNo);
        return;
      }

      // now we have an array of elements
      for (KMX_DWORD e = 0; e < elemListLength; e++) {
        const kmx::COMP_KMXPLUS_ELEM_ELEMENT &element = elemList[e];
        std::u16string str;
        if (element.flags && LDML_ELEM_FLAGS_UNICODE_SET) {
          str = kplus.strs->get(element.element);
          // TODO-LDML: actually a UnicodeSet here
        } else {
          str = element.get_string();  // Get single UTF-16
        }
        list.push_back(str);  // add the string to the end
      }
      // Now, add the list to the map
      transforms.add(list, tostr);
    }
  }
  // Only valid if we reach here
  DebugLog("_valid = true");
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
      {
        KMX_DWORD last_char = 0UL;
        // attempt to get the last char
        auto end = state->context().rbegin();
        if(end != state->context().rend()) {
          if((*end).type == KM_KBP_CT_CHAR) {
            last_char = (*end).character;
          }
        }
        if (last_char == 0UL) {
          state->actions().push_backspace(KM_KBP_BT_UNKNOWN);
        } else {
          state->actions().push_backspace(KM_KBP_BT_CHAR, last_char);
        }
        state->context().pop_back();
      }
      break;
    default:
      // from kmx_processor.cpp
      // Construct a context buffer from the items up until the last KM_KBP_CT_MARKER marker
      ldml::string_list ctxt;
      auto cp = state->context();
      // We're only interested in as much of the context as is a KM_KBP_CT_CHAR.
      // This will stop at the KM_KBP_CT_MARKER type.
      uint8_t last_type = KM_KBP_BT_UNKNOWN;
      for (auto c = cp.rbegin(); c != cp.rend(); c++) {
        last_type = c->type;
        if (last_type != KM_KBP_BT_CHAR) {
          break;
        }
        km::kbp::kmx::char16_single buf;
        const int len = km::kbp::kmx::Utf32CharToUtf16(c->character, buf);
        const std::u16string str(buf.ch, len);
        ctxt.push_front(str); // prepend to string
      }
      if (last_type != KM_KBP_BT_MARKER) {
        // There was no beginning-of-translation marker.
        //Add one.
        state->context().push_marker(0x0);
        state->actions().push_marker(0x0);
      }
      // Look up the key
      const std::u16string str = keys.lookup(vk, modifier_state);
      if (str.empty()) {
        // not found
        state->actions().commit(); // finish up and
        return KM_KBP_STATUS_OK; // Nothing to do- no key
      }
      const std::u32string str32 = kmx::u16string_to_u32string(str);
      for(size_t i=0; i<str32.length(); i++) {
        state->context().push_character(str32[i]);
        state->actions().push_character(str32[i]);
      }
      // add the newly added char
      ctxt.push_back(str);
      // Now process transforms
      std::u16string outputString;
      // Process the transforms
      const size_t matchedContext = transforms.matchContext(ctxt, outputString);

      if (matchedContext > 0) {
        // Found something.
        // Now, clear out the old context
        for (size_t i = 0; i < matchedContext; i++) {
          state->context().pop_back();  // Pop off last
          auto deletedChar = ctxt[ctxt.size() - i - 1][0];
          state->actions().push_backspace(KM_KBP_BT_CHAR, deletedChar);  // Cause prior char to be removed
        }
        // Now, add in the updated text
        const std::u32string outstr32 = kmx::u16string_to_u32string(outputString);
        for (size_t i = 0; i < outstr32.length(); i++) {
          state->context().push_character(outstr32[i]);
          state->actions().push_character(outstr32[i]);
        }
        // Add a marker so we don't retransform this text.
        state->context().push_marker(0x0);
        state->actions().push_marker(0x0);
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
