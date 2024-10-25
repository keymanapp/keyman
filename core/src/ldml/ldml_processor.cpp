/*
  Copyright:    © SIL International.
  Description:  This is an implementation of the LDML keyboard spec 3.0.
  Create Date:  5 Aug 2022
  Authors:      Marc Durdin (MD)
*/

#include <algorithm>
#include "ldml/ldml_processor.hpp"
#include "ldml/ldml_transforms.hpp"
#include "ldml/ldml_markers.hpp"
#include "state.hpp"
#include "kmx_file.h"
#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
#include "kmx/kmx_processevent.h"
#include "kmx/kmx_processor.hpp"
#include "ldml/keyman_core_ldml.h"
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

// using km::core::kmx::ShouldDebug; // for DebugLog

namespace km {
namespace core {

ldml_processor::ldml_processor(std::u16string const& kb_name, const std::vector<uint8_t>& data)
    : abstract_processor(keyboard_attributes(kb_name, KM_CORE_LMDL_PROCESSOR_VERSION, {})),
      _valid(false), transforms(), bksp_transforms(), keys(), normalization_disabled(false) {
  if(data.size() <= sizeof(kmx::COMP_KEYBOARD_EX)) {
    DebugLog("data.size %zu too small", data.size());
    return;
  }

  // Locate the structs here, but still retain ptrs to the raw structs.
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
      keys.add((km_core_virtual_key)kmapEntry->vkey, kmapEntry->mod, str);
    }
  } // else: no keys! but still valid. Just, no keys.

  // load transforms
  if (kplus.tran != nullptr && kplus.tran->groupCount > 0) {
    transforms.reset(km::core::ldml::transforms::load(kplus, kplus.tran, kplus.tranHelper));
    if (!transforms) {
      DebugLog("Failed to load tran transforms");
      return; // failed to load
    }
  }

  // load bksp transforms
  if (kplus.bksp != nullptr && kplus.bksp->groupCount > 0) {
    bksp_transforms.reset(km::core::ldml::transforms::load(kplus, kplus.bksp, kplus.bkspHelper));
    if (!bksp_transforms) {
      DebugLog("Failed to load bksp transforms");
      return; // failed to load
    }
  }
  if (kplus.meta != nullptr) {
    if (kplus.meta->settings & LDML_META_SETTINGS_NORMALIZATION_DISABLED) {
      normalization_disabled = true;
    }
  }
  // Only valid if we reach here
  DebugLog("_valid = true");
  _valid = true;
}

/**
 * Returns true if the data is a KMX+ file.
 *
 * @param data  the keyboard blob
 * @return true if the processor can handle the keyboard, otherwise false.
 */
bool ldml_processor::is_handled(const std::vector<uint8_t>& data) {
  // Check if it's a blob from a KMX file
  if (!kmx_processor::is_handled(data)) {
    return false;
  }

  const kmx::PCOMP_KEYBOARD comp_keyboard = (kmx::PCOMP_KEYBOARD)data.data();
  if (comp_keyboard->dwFileVersion < VERSION_160 || (comp_keyboard->dwFlags & KF_KMXPLUS) == 0) {
    return false;
  }

  // The buffer contains KMXPlus data (although more validation is required and will
  // be done in the constructor)
  return true;
}

km_core_status
ldml_processor::process_queued_actions(
  km_core_state *_kmn_unused(state)
) {
  /* Not implemented. Only used by IMX. */
  assert(false);
  return KM_CORE_STATUS_NOT_IMPLEMENTED;
}

bool ldml_processor::queue_action(
  km_core_state * _kmn_unused(state),
  km_core_action_item const* _kmn_unused(action_item)
)
{
  /* Not implemented. Only used by IMX. */
  assert(false);
  return false;
}

km_core_status
ldml_processor::process_event(
    km_core_state *state,
    km_core_virtual_key vk,
    uint16_t modifier_state,
    uint8_t is_key_down,
    uint16_t event_flags
) {
  assert(state);
  if (!state)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  // this will hold the parameters, and our response
  ldml_event_state ldml_state(state, vk, modifier_state, is_key_down, event_flags);
  ldml_state.clear();

  try {
    if (!is_key_down) {
      process_key_up(ldml_state);
    } else {
      switch (vk) {
      // Currently, only one VK gets spoecial treatment.
      // Special handling for backspace VK
      case KM_CORE_VKEY_BKSP:
        process_backspace(ldml_state);
        break;
      default:
        // all other VKs
        process_key_down(ldml_state);
      } // end of switch
    } // end of normal processing

    // all key-up and key-down events end up here.
    // commit the ldml state into the core state
    ldml_state.commit();
    return KM_CORE_STATUS_OK;
  } catch (std::bad_alloc &) {
    // out of memory, clean up and get out
    ldml_state.clear();
    // no actions will be set
    return KM_CORE_STATUS_NO_MEM;
  }
}

void
ldml_processor::process_key_up(ldml_event_state &ldml_state)
    const {
  // TODO-LDML: Implement caps lock handling
  ldml_state.clear();
}

void
ldml_processor::process_backspace(ldml_event_state &ldml_state) const {
  if (!!bksp_transforms) {
    // process with an empty string via the bksp transforms
    auto matchedContext = process_output(ldml_state, std::u32string(), bksp_transforms.get());

    if (matchedContext > 0) {
      return; // The transform took care of the backspacing.
    }  // else, fall through to default processing below.
  }

  ldml_state.emit_backspace();
}

void ldml_event_state::emit_backspace() {
  // this is called from user-initiated backspace, not internal backspacing.

  // Find out what the last actual character was and remove it.
  // attempt to get the last char
  // TODO-LDML: emoji backspace
  auto end = state->context().rbegin();
  while (end != state->context().rend()) {
    if (end->type == KM_CORE_CT_CHAR) {
      actions.code_points_to_delete++;
      state->context().pop_back();
      return;
    }
    // else loop again
    assert(end->type != KM_CORE_CT_END);  // inappropriate here.
    state->context().pop_back();
 }
  /*
    We couldn't find a character at end of context (context is empty),
    so we'll pass the backspace keystroke on to the app to process; the
    app might want to use backspace to move between contexts or delete
    a text box, etc. Or it might be a legacy app and we've had our caret
    dumped in somewhere unknown, so we will have to depend on the app to
    be sensible about backspacing because we know nothing.
  */
  actions.emit_keystroke = KM_CORE_TRUE;
}

void
ldml_processor::process_key_down(ldml_event_state &ldml_state) const {
  // Look up the key
  bool found = false;
  const std::u16string key_str = keys.lookup(ldml_state.get_vk(), ldml_state.get_modifier_state(), found);

  if (!found) {
    // no key was found, so pass the keystroke on to the Engine
    ldml_state.emit_passthrough_keystroke();
  } else if (!key_str.empty()) {
    process_key_string(ldml_state, key_str);
  } // else no action: It's a gap or gap-like key.
}

void
ldml_processor::process_key_string(ldml_event_state &ldml_state, const std::u16string &key_str) const {
  // We know that key_str is not empty per the caller.
  assert(!key_str.empty());

  // we convert the keys str to UTF-32 here instead of using the emit_text() overload
  // so that we don't have to reconvert it inside the transform code.
  std::u32string key_str32 = kmx::u16string_to_u32string(key_str);
  (void)process_output(ldml_state, key_str32, transforms.get());
}

size_t ldml_processor::process_output(ldml_event_state &ldml_state, const std::u32string &key_str, ldml::transforms *with_transforms) const {
  // previous context string
  std::u32string old_ctxt;
  (void)ldml_state.context_to_string(old_ctxt, true);
  // new context string (NFD)
  std::u32string new_ctxt = old_ctxt;
  // add the newly added key output to new_ctxt
  new_ctxt.append(key_str);

  // Note:
  //  The normalize functions will assert() and DebugLog() if there is a problem,
  //  so we do not need to assert their status here unless we're going to do something
  //  different with control flow.
  if (!normalization_disabled) {
    (void)ldml::normalize_nfd_markers(old_ctxt);
    (void)ldml::normalize_nfd_markers(new_ctxt);
  }

  /** how many chars of the new_ctxt to replace? This is our return value. */
  const size_t new_ctxt_matched = apply_transforms(new_ctxt, with_transforms);

  if (new_ctxt_matched == 0 && key_str.empty()) {
    // no transforms matched, and no key string came in
    // just return.
    assert(old_ctxt == new_ctxt);
  } else {
    // apply difference
    ldml_state.emit_difference(old_ctxt, new_ctxt);
  }

  return new_ctxt_matched;
}

size_t ldml_processor::apply_transforms(std::u32string &new_ctxt, ldml::transforms *with_transforms) const {
  if (with_transforms == nullptr) {
    return 0; // nothing to do
  }
  /** transformed output string */
  std::u32string transform_output;
  size_t new_ctxt_matched = with_transforms->apply(new_ctxt, transform_output);

  // drop last 'matchedContext':
  new_ctxt.resize(new_ctxt.length() - new_ctxt_matched);
  // TODO-LDML: should be able to do a normalization-safe append here. Instead, we append and re-normalize.
  new_ctxt.append(transform_output);
  if (!normalization_disabled) {
    (void)ldml::normalize_nfd_markers(new_ctxt);
  }
  // new_ctxt is now up-to-date and ready to apply.
  return new_ctxt_matched;
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
ldml_event_state::emit_difference(const std::u32string &old_ctxt, const std::u32string &new_ctxt) {
  // find common prefix.

  // For example, if the context previously had "aaBBBBB" and it is changing to "aaCCC" then we will have:
  // - old_ctxtstr_changed = "BBBBB"
  // - new_ctxtstr_changed = "CCC"
  // So the BBBBB needs to be removed and then CCC added.
  auto ctxt_prefix = mismatch(old_ctxt.begin(), old_ctxt.end(), new_ctxt.begin(), new_ctxt.end());

  // is the 'mismatch' at the end (i.e., no mismatch)?
  if(ctxt_prefix.first == old_ctxt.end() && ctxt_prefix.second == new_ctxt.end()) {
    return; // Optimization: We can just exit, there's nothing to do.
  }

  // handle a special case where we're simply changing from one marker to another.
  // Example:
  // 0. old_ctxtstr_changed ends with  … U+FFFF U+0008 | U+0001 …
  // 1. ctxtstr_cleanedup   ends with  … U+FFFF U+0008 | U+0002 …
  // Pipe symbol shows where the difference starts.
  // As you can see, the different starts in the MIDDLE of a marker sequence.
  // so, old_ctxtstr_changed will start with U+0001
  // and new_ctxtstr_changed will start with U+0002
  // remove_text will only be able to delete up to and through the U+0001
  // and it will need to emit a push_backspace(KM_CORE_BT_MARKER,…) due to the
  // marker change.
  // We can detect this because the unchanged_prefix will end with u+FFFF U+0008
  //
  // k_212* and k_213* hit this case.
  std::u32string common_prefix(old_ctxt.begin(), ctxt_prefix.first);
  if (common_prefix.length() >= 2) {
    auto iter = common_prefix.rbegin();
    if (*(iter++) == LDML_MARKER_CODE && *(iter++) == UC_SENTINEL) {
      // adjust the iterator so that the "U+FFFF U+0008" is not a part of the common prefix.
      ctxt_prefix.first -= 2;  // backup the 'mismatch' point to before the FFFF
      ctxt_prefix.second -= 2; // backup the 'mismatch' point to before the FFFF
      // Now, old_ctxtstr_changed and new_ctxtstr_changed will start with U+FFFF U+0008 …
    }
  }

  /** The part of the old string to be removed */
  std::u32string old_ctxtstr_changed(ctxt_prefix.first, old_ctxt.end());
  /** The new context to be added */
  std::u32string new_ctxtstr_changed(ctxt_prefix.second, new_ctxt.end());

  // FIRST drop the old suffix. Note: this mutates old_ctxtstr_changed.
  // see remove_text() docs, this PUSHes actions, POPs context items, and TRIMS the string.
  remove_text(old_ctxtstr_changed, old_ctxtstr_changed.length());
  assert(old_ctxtstr_changed.length() == 0);
  // old_ctxtstr_changed is now empty because it's been removed.
  // context is "aa" in the above example.

  // THEN add the new suffix, "CCC" in the above example
  emit_text(new_ctxtstr_changed);
  // context is now "aaCCC"
}

void
ldml_event_state::remove_text(std::u32string &str, size_t length) {
  // str is the string to remove, so it should be at least as long as length
  assert(length <= str.length());
  /** track how many context items have been removed, via push_backspace() */
  size_t contextRemoved = 0;
  for (auto c = state->context().rbegin(); length > 0 && c != state->context().rend(); c++, contextRemoved++) {
    /** last char of context */
    km_core_usv lastCtx = str.back();
    uint8_t type        = c->type;
    assert(type == KM_CORE_BT_CHAR || type == KM_CORE_BT_MARKER);
    if (type == KM_CORE_BT_CHAR) {
      // single char, drop it
      length--;
      assert(c->character == lastCtx);
      str.pop_back();
      // Cause prior char to be removed
      actions.code_points_to_delete++;
    } else if (type == KM_CORE_BT_MARKER) {
      assert(length >= 3);
      // #3 - the marker.
      assert(lastCtx == c->marker);
      str.pop_back();
      length--;
      // #2 - the code
      assert(str.back() == LDML_MARKER_CODE);
      str.pop_back();
      length--;
      // #1 - the sentinel
      assert(str.back() == UC_SENTINEL);
      str.pop_back();
      // Nothing in actions
      length--;
    }
  }
  assert(length == 0);
  // now, pop the context items
  for (size_t i = 0; i < contextRemoved; i++) {
    // we don't pop during the above loop because the iterator gets confused
    state->context().pop_back();
  }
}

void
ldml_event_state::emit_text(const std::u16string &str) {
  const std::u32string str32 = kmx::u16string_to_u32string(str);
  emit_text(str32);
}

void
ldml_event_state::emit_text(const std::u32string &str) {
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
      emit_marker(marker_no);
    } else {
      emit_text(ch);
    }
  }
}

void
ldml_event_state::emit_text( km_core_usv ch) {
  assert(ch != LDML_UC_SENTINEL);
  state->context().push_character(ch);
  text.push_back(ch);
}

void
ldml_event_state::emit_marker( KMX_DWORD marker_no) {
  assert(km::core::kmx::is_valid_marker(marker_no));
  // No markers in the action struct!
  state->context().push_marker(marker_no);
}

void ldml_event_state::emit_passthrough_keystroke() {
  // assert we haven't already requested a keystroke
  assert(actions.emit_keystroke != KM_CORE_TRUE);
  actions.emit_keystroke = KM_CORE_TRUE;
}

size_t
ldml_event_state::context_to_string(std::u32string &str, bool include_markers) {
    str.clear();
    auto &cp          = state->context();
    size_t ctxlen     = 0;  // TODO-LDML: not used by callers?
    uint8_t last_type = KM_CORE_BT_UNKNOWN;
    for (auto c = cp.rbegin(); c != cp.rend(); c++, ctxlen++) {
      last_type = c->type;
      if (last_type == KM_CORE_BT_CHAR) {
        str.insert(0, 1, c->character);
      } else if (last_type == KM_CORE_BT_MARKER) {
        assert(km::core::kmx::is_valid_marker(c->marker));
        if (include_markers) {
          ldml::prepend_marker(str, c->marker);
        }
      } else {
        break;
      }
    }
    return ctxlen; // consumed the entire context buffer.
}

static const km_core_option_item NULL_OPTIONS[] = {KM_CORE_OPTIONS_END};

ldml_event_state::ldml_event_state(
    km_core_state *s,
    km_core_virtual_key v,
    uint16_t m,
    uint8_t i,
    uint16_t e) {
  this->state          = s;
  this->vk             = v;
  this->modifier_state = m;
  this->is_key_down    = i;
  this->event_flags    = e;

  actions.persist_options = new km_core_option_item[1];
  actions.persist_options[0] = NULL_OPTIONS[0];

  // initialize actions
  clear();
}

void ldml_event_state::commit() {
  // thi is only set for the duration of the next call.
  actions.output = text.c_str();
  // current implementation copies actions.output and doesn't retain it
  state->set_actions(actions);
  // clean up
  actions.output = nullptr;
  // clear struct
  clear();
  // commit. TODO: should this be necessary? #9999
  state->actions().commit();
}

void ldml_event_state::clear() {
  // reset text
  text.clear();
  // reset state obj
  actions.output                = nullptr;
  actions.code_points_to_delete = 0;
  actions.do_alert              = KM_CORE_FALSE;
  actions.emit_keystroke        = KM_CORE_FALSE;
  actions.new_caps_lock_state   = KM_CORE_CAPS_UNCHANGED;
}

void ldml_event_state::context_clear() {
  // clear the context
  state->context().clear();
  // clear our own state
  clear();
}

ldml_event_state::~ldml_event_state() {
  delete [] actions.persist_options;
}

} // namespace core
} // namespace km
