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
  ), _valid(false), vkey_to_string(), simple_transforms()
{

// TODO-LDML: move these asserts into kmx_plus

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

#define KMXPLUS_ASSERT_NOTNULL(p) if ((p) == nullptr) { \
    KMXPLUS_NOTEQUAL("!nullptr", p); \
    _valid = false; \
    return; \
  }

  KMXPLUS_ASSERT(true, data.size() > sizeof(kmx::COMP_KEYBOARD_EX));

//   // Locate the structs here, but still retain ptrs to the raw structs.
  kmx::KMX_FileValidator* comp_keyboard = (kmx::KMX_FileValidator*)data.data();

  // Perform the standard validation
  KMXPLUS_ASSERT(TRUE, comp_keyboard->VerifyKeyboard(data.size()));

  kmx::kmx_plus kplus(comp_keyboard, data.size()); // slices to a COMP_KEYBOARD

  KMXPLUS_ASSERT(true, kplus.is_valid());

  // Now, if we have keys, use them.
  if (kplus.keys != nullptr) {
    // read all keys into array
    for (KMX_DWORD i=0; i<kplus.keys->count; i++) {
      std::u16string str;
      const kmx::COMP_KMXPLUS_KEYS_ENTRY &entry = kplus.keys->entries[i];
      if (entry.flags && LDML_KEYS_FLAGS_EXTEND) {
        KMXPLUS_ASSERT_NOTNULL(kplus.strs); // need a string table to get strings
        str = kplus.strs->get(entry.to);
      } else {
        str = entry.get_string();
      }
      ldml_vkey_id vkey_id((km_kbp_virtual_key)entry.vkey, (uint16_t)entry.mod);
      vkey_to_string[vkey_id] = str; // assign the string
    }
  } // else: no keys! but still valid. Just, no keys.
  if (kplus.tran != nullptr) {
    KMXPLUS_ASSERT_NOTNULL(kplus.elem);  // fail if elems arent' there
    KMXPLUS_ASSERT_NOTNULL(kplus.strs);  // gonna need strs
    for (KMX_DWORD i = 0; i < kplus.tran->count; i++) {
      const kmx::COMP_KMXPLUS_TRAN_ENTRY &entry = kplus.tran->entries[i];
      // 'to' is always a string, unlike keys
      const std::u16string tostr = kplus.strs->get(entry.to);
      // now, fetch the 'from'
      ldml_string_list list;

      // TODO-LDML: before=
      // TODO-LDML: error=

      // process From
      const KMX_DWORD elemNo = entry.from;
      KMXPLUS_ASSERT(true, elemNo < kplus.elem->count);

      // TODO-LDML: refactor this
      KMX_DWORD elemListLength                       = 0;
      const kmx::COMP_KMXPLUS_ELEM_ELEMENT *elemList = kplus.elem->getElementList(elemNo, elemListLength);
      KMXPLUS_ASSERT_NOTNULL(elemList);  // either invalid OR zero length
      KMXPLUS_ASSERT(true, elemListLength > 0);   // not a zero length

      // now we have an array of elements
      for (KMX_DWORD e = 0; e < elemListLength; e++) {
        const kmx::COMP_KMXPLUS_ELEM_ELEMENT &element = elemList[e];
        std::u16string str;
        if (element.flags && LDML_ELEM_FLAGS_UNICODE_SET) {
          str = kplus.strs->get(element.element);
          // TODO-LDML: actually a UnicodeSet here?!
        } else {
          str = element.get_string();  // Get single UTF-16
        }
        list.push_back(str);  // add the string to the end
      }
      // Now, add the list to the map
      simple_transforms.insert({list, tostr});
    }
  }
  KMXPLUS_PRINTLN("_valid = true");

#undef KMXPLUS_ASSERT
#undef KMXPLUS_ASSERT_NOTNULL
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
      // TODO-LDML: backspace transform
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
    // TODO-LDML: transform
    if(simple_transforms.size() > 0) { // shortcut if no transforms
      // from kmx_processor.cpp
      // Construct a context buffer from the items
      ldml_string_list ctxt;
      auto cp = state->context();
      // We're only interested in as much of the context as is a KM_KBP_CT_CHAR.
      for (auto c = cp.begin(); c != cp.end() && c->type == KM_KBP_CT_CHAR; c++) {
        km::kbp::kmx::char16_single buf;
        const int len = km::kbp::kmx::Utf32CharToUtf16(c->character, buf);
        const std::u16string str(buf.ch, len);
        ctxt.push_back(str);
      }

      // int longest_index            = -1;  // index to longest match in simple_transforms
      unsigned long longest_length = 0;  // length of longest match
      // auto i                       = 0;   // counter
      std::u16string result;
      // bool partial       = false;      // true if the longest was a partial match
      for (auto it = simple_transforms.begin(); it != simple_transforms.end(); it++/*,i++*/) {
        auto list = it->first;
        // see if it matches
        if (longest_length >= list.size()) {
          continue; // already have a longer match
        }
        if (list.size() > ctxt.size()) {
          continue; // not enough ctxt for a match
        }
        // Now, check for match
        bool is_match = true;
        for (unsigned long j = 0; j < list.size(); j++) {
          const auto found  = ctxt.at(ctxt.size() - j - 1);
          const auto expect = list.at(list.size() - j - 1);
          if (found != expect) {
            // mismatch, break
            is_match = false;
            break;
          }
        }
        if (!is_match) {
          continue;
        }
        // Found a match
        longest_length = list.size();
        // longest_index  = i;
        result = it->second;
      }
      if (longest_length > 0) {
        // Found something.
        // Reset actions.  TODO-LDML: wrong for backspace
        // state->actions().clear();
        // Now, clear out the old context
        for (unsigned long i=0; i<longest_length; i++) {
          state->context().pop_back(); // Pop off last
          auto deletedChar = ctxt[ctxt.size() - i - 1][0];
          state->actions().push_backspace(KM_KBP_BT_CHAR, deletedChar);  // Cause prior char to be removed
        }
        // Now, add in the updated text
        for(size_t i=0; i<result.length(); i++) {
          state->context().push_character(result[i]);
          state->actions().push_character(result[i]);
        }
      }
    }

    // TODO-LDML: reorder
    // TODO-LDML: final
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
