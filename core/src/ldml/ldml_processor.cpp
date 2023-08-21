/*
  Copyright:    © SIL International.
  Description:  This is an implementation of the LDML keyboard spec 3.0.
  Create Date:  5 Aug 2022
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
#include "debuglog.h"
#include <assert.h>

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

// using km::kbp::kmx::ShouldDebug; // for DebugLog

namespace km {
namespace kbp {


ldml_processor::ldml_processor(path const & kb_path, const std::vector<uint8_t> &data)
: abstract_processor(
    keyboard_attributes(kb_path.stem(), KM_KBP_LMDL_PROCESSOR_VERSION, kb_path.parent(), {})
  ), _valid(false), transforms(), bksp_transforms(), keys()
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
  if (kplus.key2 != nullptr) {
    // read all keys into array
    for (KMX_DWORD i=0; i<kplus.key2->kmapCount; i++) {
      std::u16string str;
      auto kmapEntry = kplus.key2Helper.getKmap(i);
      assert(kmapEntry != nullptr);
      // now look up the key
      auto keyEntry = kplus.key2Helper.getKeys(kmapEntry->key);
      assert(keyEntry != nullptr);

      // TODO-LDML: LDML_KEYS_KEY_FLAGS_NOTRANSFORM
      if (keyEntry->flags & LDML_KEYS_KEY_FLAGS_EXTEND) {
        if (nullptr == kplus.strs) {
          DebugLog("for keys: kplus.strs == nullptr"); // need a string table to get strings
          assert(false);
          return;
        }
        str = kplus.strs->get(keyEntry->to);
      } else {
        str = keyEntry->get_to_string();
      }
      keys.add((km_kbp_virtual_key)kmapEntry->vkey, (uint16_t)kmapEntry->mod, str);
    }
  } // else: no keys! but still valid. Just, no keys.

  // load transforms
  if (kplus.tran != nullptr && kplus.tran->groupCount > 0) {
    transforms.reset(km::kbp::ldml::transforms::load(kplus, kplus.tran, kplus.tranHelper));
    if (!transforms) {
      DebugLog("Failed to load tran transforms");
      return; // failed to load
    }
  }

  // load bksp transforms
  if (kplus.bksp != nullptr && kplus.bksp->groupCount > 0) {
    bksp_transforms.reset(km::kbp::ldml::transforms::load(kplus, kplus.bksp, kplus.bkspHelper));
    if (!bksp_transforms) {
      DebugLog("Failed to load bksp transforms");
      return; // failed to load
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
        if (!!bksp_transforms) {
          // TODO-LDML: process bksp
          // std::u16string outputString;
          // // don't bother if no backspace transforms!
          // // TODO-LDML: unroll ctxt into a str
          // std::u16string ctxtstr;
          // for (size_t i = 0; i < ctxt.size(); i++) {
          //   ctxtstr.append(ctxt[i]);
          // }
          // const size_t matchedContext = transforms->apply(ctxtstr, outputString);
        }
        KMX_DWORD last_char = 0UL;
        // attempt to get the last char
        auto end = state->context().rbegin();
        if(end != state->context().rend()) {
          if((*end).type == KM_KBP_CT_CHAR) {
            last_char = (*end).character;
          }
        }
        if (last_char == 0UL) {
          /*
            We couldn't find a character at end of context (context is empty),
            so we'll pass the backspace keystroke on to the app to process; the
            app might want to use backspace to move between contexts or delete
            a text box, etc. Or it might be a legacy app and we've had our caret
            dumped in somewhere unknown, so we will have to depend on the app to
            be sensible about backspacing because we know nothing.
          */
          state->actions().push_backspace(KM_KBP_BT_UNKNOWN);
        } else {
          state->actions().push_backspace(KM_KBP_BT_CHAR, last_char);
          state->context().pop_back();
        }
      }
      break;
    default:
      {
        // adapted from kmx_processor.cpp

        /** a copy of the current/changed context, for transform use */
        ldml::string_list ctxt;

        // Construct a context buffer of all the KM_KBP_BT_CHAR items
        // Extract the context into 'ctxt' for transforms to process
        if (!!transforms) {
          // if no transforms, no reason to do this extraction (ctxt will remain empty)
          auto &cp = state->context();
          // We're only interested in as much of the context as is a KM_KBP_BT_CHAR.
          uint8_t last_type = KM_KBP_BT_UNKNOWN;
          for (auto c = cp.rbegin(); c != cp.rend(); c++) {
            last_type = c->type;
            if (last_type != KM_KBP_BT_CHAR) {
              // not a char, stop here
              // TODO-LDML: markers?
              break;
            }
            ctxt.emplace_front(1, c->character);
            // extract UTF-32 to 1 or 2 UTF-16 chars in a string
          }
        }

        // Look up the key
        const std::u16string str = keys.lookup(vk, modifier_state);
        if (str.empty()) {
          // not found, so pass the keystroke on to the Engine
          state->actions().push_invalidate_context();
          state->actions().push_emit_keystroke();
          break; // ----- commit and exit
        }
        // found the correct string - push it into the context and actions
        const std::u32string str32 = kmx::u16string_to_u32string(str);
        for (const auto &ch : str32) {
          state->context().push_character(ch);
          state->actions().push_character(ch);
        }
        // Now process transforms
        // Process the transforms
        if (!!transforms) {
          // add the newly added char to ctxt
          ctxt.push_back(str32);

          std::u32string outputString;

          // TODO-LDML: unroll ctxt into a str. Would be better to have transforms be able to process a vector
          std::u32string ctxtstr;
          for (const auto &ch : ctxt) {
            ctxtstr.append(ch);
          }
          // check if the context matched, and if so how much (at the end)
          const size_t matchedContext = transforms->apply(ctxtstr, outputString);

          if (matchedContext > 0) {
            // Found something.
            // Now, clear out the old context
            for (size_t i = 0; i < matchedContext; i++) {
              state->context().pop_back();  // Pop off last
              auto deletedChar = ctxt[ctxt.size() - i - 1][0];
              state->actions().push_backspace(KM_KBP_BT_CHAR, deletedChar);  // Cause prior char to be removed
            }
            // Now, add in the updated text
            for (const auto &ch : outputString) {
              state->context().push_character(ch);
              state->actions().push_character(ch);
            }
          }
        }
      }
    }
    // end of normal processing: commit and exit
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
