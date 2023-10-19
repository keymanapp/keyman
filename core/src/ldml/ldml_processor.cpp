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
  constexpr km_core_attr const engine_attrs = {
    256,
    KM_CORE_LIB_CURRENT,
    KM_CORE_LIB_AGE,
    KM_CORE_LIB_REVISION,
    KM_CORE_TECH_LDML,
    "SIL International"
  };
}

// using km::kbp::kmx::ShouldDebug; // for DebugLog

namespace km {
namespace kbp {


ldml_processor::ldml_processor(path const & kb_path, const std::vector<uint8_t> &data)
: abstract_processor(
    keyboard_attributes(kb_path.stem(), KM_CORE_LMDL_PROCESSOR_VERSION, kb_path.parent(), {})
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
      keys.add((km_core_virtual_key)kmapEntry->vkey, (uint16_t)kmapEntry->mod, str);
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

km_core_status
ldml_processor::process_queued_actions(
  km_core_state *state
) {
  assert(state);
  if (!state)
    return KM_CORE_STATUS_INVALID_ARGUMENT;
  // TODO Implement
  return KM_CORE_STATUS_OK;
}

bool ldml_processor::queue_action(
  km_core_state * state,
  km_core_action_item const* action_item
)
{
  assert(state);
  assert(action_item);
  if ((!state) || (!action_item))
    return false;
  return false;
}

km_core_status
ldml_processor::process_event(
  km_core_state *state,
  km_core_virtual_key vk,
  uint16_t modifier_state,
  uint8_t is_key_down,
  uint16_t /*event_flags*/ // TODO-LDML: unused... for now...
) {
  assert(state);
  if (!state)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  if (!is_key_down) {
    // TODO: Implement caps lock handling
    state->actions().clear();
    state->actions().commit();
    return KM_CORE_STATUS_OK;
  }

  try {
    // At the start of every process_event always clear the action_items
    state->actions().clear();

    switch (vk) {
    // Special handling for backspace VK
    case KM_CORE_VKEY_BKSP:
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
          if((*end).type == KM_CORE_CT_CHAR) {
            last_char = (*end).character;
            // TODO-LDML: markers!
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
          state->actions().push_backspace(KM_CORE_BT_UNKNOWN);
        } else {
          state->actions().push_backspace(KM_CORE_BT_CHAR, last_char);
          state->context().pop_back();
        }
      }
      break;
    default:
    // all other VKs
      {
        // Look up the key
        const std::u16string str = keys.lookup(vk, modifier_state);

        if (str.empty()) {
          // no key was found, so pass the keystroke on to the Engine
          state->actions().push_invalidate_context();
          state->actions().push_emit_keystroke();
          break; // ----- commit and exit
        }

        // found a string - push it into the context and actions
        // we convert it here instead of using the emit_text() overload
        // so that we don't have to reconvert it inside the transform code.
        const std::u32string str32 = kmx::u16string_to_u32string(str);

        if (!transforms) {
          // No transforms: just emit the string.
          emit_text(state, str32);
        } else {
          // Process transforms here
          /**
           * a copy of the current/changed context, for transform use.
           *
           */
          std::u32string ctxtstr;
          (void)context_to_string(state, ctxtstr);
          // add the newly added key output to ctxtstr
          ctxtstr.append(str32);

          /** the output buffer for transforms */
          std::u32string outputString;

          // apply the transform, get how much matched (at the end)
          const size_t matchedContext = transforms->apply(ctxtstr, outputString);

          if (matchedContext == 0) {
            // No match, just emit the original string
            emit_text(state, str32);
          } else {
            // We have a match.

            ctxtstr.resize(ctxtstr.length() - str32.length());
            /** how many chars of the context we need to clear */
            auto charsToDelete = matchedContext - str32.length(); /* we don't need to clear the output of the current key */

            /** how many context items need to be removed */
            size_t contextRemoved = 0;
            for (auto c = state->context().rbegin(); charsToDelete > 0 && c != state->context().rend(); c++, contextRemoved++) {
              /** last char of context */
              km_core_usv lastCtx = ctxtstr.back();
              uint8_t type = c->type;
              assert(type == KM_CORE_BT_CHAR || type == KM_CORE_BT_MARKER);
              if (type == KM_CORE_BT_CHAR) {
                // single char, drop it
                charsToDelete--;
                assert(c->character == lastCtx);
                ctxtstr.pop_back();
                state->actions().push_backspace(KM_CORE_BT_CHAR, lastCtx);  // Cause prior char to be removed
              } else if (type == KM_CORE_BT_MARKER) {
                // it's a marker, 'worth' 3 uchars
                assert(charsToDelete >= 3);
                assert(lastCtx == c->marker); // end of list
                charsToDelete -= 3;
                // pop off the three-part sentinel string
                ctxtstr.pop_back();
                ctxtstr.pop_back();
                ctxtstr.pop_back();
                // push a special backspace to delete the marker
                state->actions().push_backspace(KM_CORE_BT_MARKER, c->marker);
              }
            }
            // now, pop the right number of context items
            for (size_t i = 0; i < contextRemoved; i++) {
              // we don't pop during the above loop because the iterator gets confused
              state->context().pop_back();
            }
            // Now, add in the updated text. This will convert UC_SENTINEL, etc back to marker actions.
            emit_text(state, outputString);
            // If we needed it further. we could update ctxtstr here:
            //    ctxtstr.append(outputString);
            // ... but it is no longer needed at this point.
          } // end of transform match
        } // end of processing transforms
      } // end of processing a 'normal' vk
    } // end of switch
    // end of normal processing: commit and exit
    state->actions().commit();
  } catch (std::bad_alloc &) {
    state->actions().clear();
    return KM_CORE_STATUS_NO_MEM;
  }

  return KM_CORE_STATUS_OK;
}

km_core_attr const & ldml_processor::attributes() const {
  return engine_attrs;
}

km_core_keyboard_key  * ldml_processor::get_key_list() const {
  km_core_keyboard_key* key_list = new km_core_keyboard_key(KM_CORE_KEYBOARD_KEY_LIST_END);
  return key_list;
}

km_core_keyboard_imx  * ldml_processor::get_imx_list() const {
  km_core_keyboard_imx* imx_list = new km_core_keyboard_imx(KM_CORE_KEYBOARD_IMX_END);
  return imx_list;
}

km_core_context_item * ldml_processor::get_intermediate_context() {
  km_core_context_item *citems = new km_core_context_item(KM_CORE_CONTEXT_ITEM_END);
  return citems;
}

km_core_status ldml_processor::validate() const {
  return _valid ? KM_CORE_STATUS_OK : KM_CORE_STATUS_INVALID_KEYBOARD;
}

void
ldml_processor::emit_text(km_core_state *state, const std::u16string &str) {
  const std::u32string str32 = kmx::u16string_to_u32string(str);
  emit_text(state, str32);
}

void
ldml_processor::emit_text(km_core_state *state, const std::u32string &str) {
  for (auto it = str.begin(); it < str.end(); it++) {
    const auto ch = *it;
    // If we are at the start of a sequence:
    if (ch == LDML_UC_SENTINEL) {
      it++; // consume LDML_UC_SENTINEL
      // TODO-LDML: Might assert if a malformed sequence is included- "should not happen"?
      assert(it < str.end());
      // verify that the next char is LDML_MARKER_CODE
      assert(*it == LDML_MARKER_CODE);
      it++; // consume LDML_MARKER_CODE
      assert(it < str.end());
      const auto marker_no = *it;
      emit_marker(state, marker_no);
    } else {
      emit_text(state, ch);
    }
  }
}

void
ldml_processor::emit_text(km_core_state *state, km_core_usv ch) {
  assert(ch != LDML_UC_SENTINEL);
  state->context().push_character(ch);
  state->actions().push_character(ch);
}

void
ldml_processor::emit_marker(km_core_state *state, KMX_DWORD marker_no) {
  assert(km::kbp::kmx::is_valid_marker(marker_no));
  state->actions().push_marker(marker_no);
  state->context().push_marker(marker_no);
}

size_t
ldml_processor::context_to_string(km_core_state *state, std::u32string &str) {
    str.clear();
    auto &cp      = state->context();
    size_t ctxlen = 0; // TODO-LDML: is this needed?
    uint8_t last_type = KM_CORE_BT_UNKNOWN;
    for (auto c = cp.rbegin(); c != cp.rend(); c++, ctxlen++) {
      last_type = c->type;
      if (last_type == KM_CORE_BT_CHAR) {
        str.insert(0, 1, c->character);
      } else if (last_type == KM_CORE_BT_MARKER) {
        assert(km::kbp::kmx::is_valid_marker(c->marker));
        prepend_marker(str, c->marker);
      } else {
        break;
      }
    }
    return ctxlen; // consumed the entire context buffer.
}


} // namespace kbp
} // namespace km
