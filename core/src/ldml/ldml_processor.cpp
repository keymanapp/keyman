/*
  Copyright:    © SIL International.
  Description:  This is an implementation of the LDML keyboard spec 3.0.
  Create Date:  5 Aug 2022
  Authors:      Marc Durdin (MD)
*/

#include <fstream>
#include <algorithm>
#include "ldml/ldml_processor.hpp"
#include "state.hpp"
#include "kmx_file.h"
#include "kmx/kmx_plus.h"
#include "kmx/kmx_xstring.h"
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
    uint16_t _kmn_unused(event_flags)
) {
  assert(state);
  if (!state)
    return KM_CORE_STATUS_INVALID_ARGUMENT;

  try {
    // At the start of every process_event always clear the action_items
    state->actions().clear();

    if (!is_key_down) {
      process_key_up(state, vk, modifier_state);
    } else {
      switch (vk) {
      // Currently, only one VK gets spoecial treatment.
      // Special handling for backspace VK
      case KM_CORE_VKEY_BKSP:
        process_backspace(state);
        break;
      default:
        // all other VKs
        process_key_down(state, vk, modifier_state);
      } // end of switch
    } // end of normal processing

    // all key-up and key-down events end up here.
    state->actions().commit();  // always commit
    return KM_CORE_STATUS_OK;
  } catch (std::bad_alloc &) {
    // out of memory, clean up and get out
    state->actions().clear();
    return KM_CORE_STATUS_NO_MEM;
  }
}

void
ldml_processor::process_key_up(km_core_state *state, km_core_virtual_key _kmn_unused(vk), uint16_t _kmn_unused(modifier_state))
    const {
  // TODO-LDML: Implement caps lock handling
  state->actions().clear();  // TODO-LDML: Why is clear here?
}

void
ldml_processor::process_backspace(km_core_state *state) const {
  if (!!bksp_transforms) {
    // process with an empty string
    auto matchedContext = process_output(state, std::u32string(), bksp_transforms.get());

    if (matchedContext > 0) {
      return; // The transform took care of the backspacing.
    }  // else, fall through to default processing below.
  }

  // Find out what the last actual character was and remove it.
  // attempt to get the last char
  // TODO-LDML: emoji backspace
  auto end = state->context().rbegin();
  if (end != state->context().rend()) {
    if ((*end).type == KM_CORE_CT_CHAR) {
      state->actions().push_backspace(KM_CORE_BT_CHAR, (*end).character);
      state->context().pop_back();
      return;
    } else if ((*end).type == KM_CORE_BT_MARKER) {
      state->actions().push_backspace(KM_CORE_BT_MARKER, (*end).marker);
      state->context().pop_back();
    }
  }
  /*
    We couldn't find a character at end of context (context is empty),
    so we'll pass the backspace keystroke on to the app to process; the
    app might want to use backspace to move between contexts or delete
    a text box, etc. Or it might be a legacy app and we've had our caret
    dumped in somewhere unknown, so we will have to depend on the app to
    be sensible about backspacing because we know nothing.
  */
  state->actions().push_backspace(KM_CORE_BT_UNKNOWN);
}

void
ldml_processor::process_key_down(km_core_state *state, km_core_virtual_key vk, uint16_t modifier_state) const {
  // Look up the key
  bool found = false;
  const std::u16string key_str = keys.lookup(vk, modifier_state, found);

  if (!found) {
    // no key was found, so pass the keystroke on to the Engine
    emit_invalidate_passthrough_keystroke(state, vk, modifier_state);
  } else if (!key_str.empty()) {
    // TODO-LDML: Right now we take no action on empty (i.e. gap) keys. Should we take other action?
    process_key_string(state, key_str);
  }
}

void
ldml_processor::process_key_string(km_core_state *state, const std::u16string &key_str) const {
  // We know that key_str is not empty per the caller.
  assert(!key_str.empty());

  // we convert the keys str to UTF-32 here instead of using the emit_text() overload
  // so that we don't have to reconvert it inside the transform code.
  std::u32string key_str32 = kmx::u16string_to_u32string(key_str);
  (void)process_output(state, key_str32, transforms.get());
}

size_t ldml_processor::process_output(km_core_state *state, const std::u32string &str, ldml::transforms *with_transforms) const {
  std::u32string nfd_str = str;
  assert(ldml::normalize_nfd_markers(nfd_str)); // TODO-LDML: else fail?
  // extract context string, in NFD
  std::u32string old_ctxtstr_nfd;
  (void)context_to_string(state, old_ctxtstr_nfd, true);
  assert(ldml::normalize_nfd_markers(old_ctxtstr_nfd)); // TODO-LDML: else fail?

  // context string in NFD
  std::u32string ctxtstr;
  (void)context_to_string(state, ctxtstr, true); // with markers
  // add the newly added key output to ctxtstr
  ctxtstr.append(nfd_str);
  assert(ldml::normalize_nfd_markers(ctxtstr)); // TODO-LDML: else fail?

  /** transform output string */
  std::u32string outputString;
  /** how many chars of the ctxtstr to replace */
  size_t matchedContext = 0; // zero if no transforms

  // begin modifications to the string

  if(with_transforms != nullptr) {
    matchedContext = with_transforms->apply(ctxtstr, outputString);
  } else {
    // no transforms, no output
  }

  // Short Circuit: if no transforms matched, and no new text is being output,
  // just return.
  if (matchedContext == 0 && str.empty()) {
    return matchedContext;
  }

  // drop last 'matchedContext':
  ctxtstr.resize(ctxtstr.length() - matchedContext);
  ctxtstr.append(outputString); // TODO-LDML: should be able to do a normalization-safe append here.
  ldml::marker_map markers;
  assert(ldml::normalize_nfd_markers(ctxtstr, markers)); // TODO-LDML: Need marker-safe normalize here.

  // Ok. We've done all the happy manipulations.

  /** NFD w/ markers */
  std::u32string ctxtstr_cleanedup = ctxtstr;
  assert(ldml::normalize_nfd_markers(ctxtstr_cleanedup));

  // find common prefix.
  // For example, if the context previously had "aaBBBBB" and it is changing to "aaCCC" then we will have:
  // - old_ctxtstr_changed = "BBBBB"
  // - new_ctxtstr_changed = "CCC"
  // So the BBBBB needs to be removed and then CCC added.
  auto ctxt_prefix = mismatch(old_ctxtstr_nfd.begin(), old_ctxtstr_nfd.end(), ctxtstr_cleanedup.begin(), ctxtstr_cleanedup.end());
  /** The part of the old string to be removed */
  std::u32string old_ctxtstr_changed(ctxt_prefix.first,old_ctxtstr_nfd.end());
  /** The new context to be added */
  std::u32string new_ctxtstr_changed(ctxt_prefix.second,ctxtstr_cleanedup.end());

  // FIRST drop the old suffix. Note: this mutates old_ctxtstr_changed.
  // see remove_text() docs, this PUSHes actions, POPs context items, and TRIMS the string.
  remove_text(state, old_ctxtstr_changed, old_ctxtstr_changed.length());
  assert(old_ctxtstr_changed.length() == 0);
  // old_ctxtstr_changed is now empty because it's been removed.
  // context is "aa" in the above example.

  // THEN add the new suffix, "CCC" in the above example
  emit_text(state, new_ctxtstr_changed);
  // context is now "aaCCC"

  return matchedContext;
}

void
ldml_processor::remove_text(km_core_state *state, std::u32string &str, size_t length) {
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
      state->actions().push_backspace(KM_CORE_BT_CHAR, c->character);
    } else if (type == KM_CORE_BT_MARKER) {
      // It's a marker.
      // need to be able to drop 3 chars
      assert(length >= 3);
      length -= 3;
      // #3 - the marker.
      assert(lastCtx == c->marker);
      str.pop_back();
      // #2 - the code
      assert(str.back() == LDML_MARKER_CODE);
      str.pop_back();
      // #1 - the sentinel
      assert(str.back() == UC_SENTINEL);
      str.pop_back();
      // cause marker to be removed
      state->actions().push_backspace(KM_CORE_BT_MARKER, c->marker);
    }
  }
  // now, pop the context items
  for (size_t i = 0; i < contextRemoved; i++) {
    // we don't pop during the above loop because the iterator gets confused
    state->context().pop_back();
  }
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
  assert(km::core::kmx::is_valid_marker(marker_no));
  state->actions().push_marker(marker_no);
  state->context().push_marker(marker_no);
}

// from VKScanCodes.cpp
static const int ldml_VKContextReset[256] = {
	1,   //L"K_?00",				// &H0
	1,   //L"K_LBUTTON",			// &H1
	1,   //L"K_RBUTTON",			// &H2
	1,   //L"K_CANCEL",		   	// &H3
	1,   //L"K_MBUTTON",			// &H4
	1,   //L"K_?05",				// &H5
	1,   //L"K_?06",				// &H6
	1,   //L"K_?07",				// &H7
	1,   //L"K_BKSP",	    		// &H8
	1,   //L"K_TAB",	    		// &H9
	1,   //L"K_?0A",				// &HA
	1,   //L"K_?0B",				// &HB
	1,   //L"K_KP5",		    	// &HC
	1,   //L"K_ENTER",				// &HD
	1,   //L"K_?0E",				// &HE
	1,   //L"K_?0F",				// &HF
	0,   //L"K_SHIFT",				// &H10
	0,   //L"K_CONTROL",			// &H11
	0,   //L"K_ALT",				// &H12
	1,   //L"K_PAUSE",				// &H13
	0,   //L"K_CAPS",				// &H14
	1,   //L"K_KANJI?15",			// &H15
	1,   //L"K_KANJI?16",			// &H16
	1,   //L"K_KANJI?17",			// &H17
	1,   //L"K_KANJI?18",			// &H18
	1,   //L"K_KANJI?19",			// &H19
	1,   //L"K_?1A",				// &H1A
	1,   //L"K_ESC",				// &H1B
	1,   //L"K_KANJI?1C",			// &H1C
	1,   //L"K_KANJI?1D",			// &H1D
	1,   //L"K_KANJI?1E",			// &H1E
	1,   //L"K_KANJI?1F",			// &H1F
	0,   //L"K_SPACE",				// &H20
	1,   //L"K_PGUP",				// &H21
	1,   //L"K_PGDN",				// &H22
	1,   //L"K_END",				// &H23
	1,   //L"K_HOME",				// &H24
	1,   //L"K_LEFT",				// &H25
	1,   //L"K_UP",				// &H26
	1,   //L"K_RIGHT",				// &H27
	1,   //L"K_DOWN",				// &H28
	1,   //L"K_SEL",				// &H29
	1,   //L"K_PRINT",				// &H2A
	1,   //L"K_EXEC",				// &H2B
	1,   //L"K_PRTSCN",			// &H2C
	0,   //L"K_INS",				// &H2D
	1,   //L"K_DEL",				// &H2E
	1,   //L"K_HELP",				// &H2F
	0,   //L"K_0",					// &H30
	0,   //L"K_1",					// &H31
	0,   //L"K_2",					// &H32
	0,   //L"K_3",					// &H33
	0,   //L"K_4",					// &H34
	0,   //L"K_5",					// &H35
	0,   //L"K_6",					// &H36
	0,   //L"K_7",					// &H37
	0,   //L"K_8",					// &H38
	0,   //L"K_9",					// &H39
	0,   //L"K_?3A",				// &H3A
	0,   //L"K_?3B",				// &H3B
	0,   //L"K_?3C",				// &H3C
	0,   //L"K_?3D",				// &H3D
	0,   //L"K_?3E",				// &H3E
	0,   //L"K_?3F",				// &H3F
	0,   //L"K_?40",				// &H40

	0,   //L"K_A",					// &H41
	0,   //L"K_B",					// &H42
	0,   //L"K_C",					// &H43
	0,   //L"K_D",					// &H44
	0,   //L"K_E",					// &H45
	0,   //L"K_F",					// &H46
	0,   //L"K_G",					// &H47
	0,   //L"K_H",					// &H48
	0,   //L"K_I",					// &H49
	0,   //L"K_J",					// &H4A
	0,   //L"K_K",					// &H4B
	0,   //L"K_L",					// &H4C
	0,   //L"K_M",					// &H4D
	0,   //L"K_N",					// &H4E
	0,   //L"K_O",					// &H4F
	0,   //L"K_P",					// &H50
	0,   //L"K_Q",					// &H51
	0,   //L"K_R",					// &H52
	0,   //L"K_S",					// &H53
	0,   //L"K_T",					// &H54
	0,   //L"K_U",					// &H55
	0,   //L"K_V",					// &H56
	0,   //L"K_W",					// &H57
	0,   //L"K_X",					// &H58
	0,   //L"K_Y",					// &H59
	0,   //L"K_Z",					// &H5A
	0,   //L"K_?5B",				// &H5B
	0,   //L"K_?5C",				// &H5C
	0,   //L"K_?5D",				// &H5D
	0,   //L"K_?5E",				// &H5E
	0,   //L"K_?5F",				// &H5F
	0,   //L"K_NP0",				// &H60
	0,   //L"K_NP1",				// &H61
	0,   //L"K_NP2",				// &H62
	0,   //L"K_NP3",				// &H63
	0,   //L"K_NP4",				// &H64
	0,   //L"K_NP5",				// &H65
	0,   //L"K_NP6",				// &H66
	0,   //L"K_NP7",				// &H67
	0,   //L"K_NP8",				// &H68
	0,   //L"K_NP9",				// &H69
	0,   //L"K_NPSTAR",			// &H6A
	0,   //L"K_NPPLUS",			// &H6B
	0,   //L"K_SEPARATOR",			// &H6C
	0,   //L"K_NPMINUS",			// &H6D
	0,   //L"K_NPDOT",				// &H6E
	0,   //L"K_NPSLASH",			// &H6F
	1,   //L"K_F1",				// &H70
	1,   //L"K_F2",				// &H71
	1,   //L"K_F3",				// &H72
	1,   //L"K_F4",				// &H73
	1,   //L"K_F5",				// &H74
	1,   //L"K_F6",				// &H75
	1,   //L"K_F7",				// &H76
	1,   //L"K_F8",				// &H77
	1,   //L"K_F9",				// &H78
	1,   //L"K_F10",				// &H79
	1,   //L"K_F11",				// &H7A
	1,   //L"K_F12",				// &H7B
	1,   //L"K_F13",				// &H7C
	1,   //L"K_F14",				// &H7D
	1,   //L"K_F15",				// &H7E
	1,   //L"K_F16",				// &H7F
	1,   //L"K_F17",				// &H80
	1,   //L"K_F18",				// &H81
	1,   //L"K_F19",				// &H82
	1,   //L"K_F20",				// &H83
	1,   //L"K_F21",				// &H84
	1,   //L"K_F22",				// &H85
	1,   //L"K_F23",				// &H86
	1,   //L"K_F24",				// &H87

	0,   //L"K_?88",				// &H88
	0,   //L"K_?89",				// &H89
	0,   //L"K_?8A",				// &H8A
	0,   //L"K_?8B",				// &H8B
	0,   //L"K_?8C",				// &H8C
	0,   //L"K_?8D",				// &H8D
	0,   //L"K_?8E",				// &H8E
	0,   //L"K_?8F",				// &H8F

	0,   //L"K_NUMLOCK",			// &H90
	0,   //L"K_SCROLL",			// &H91

	0,   //L"K_?92",				// &H92
	0,   //L"K_?93",				// &H93
	0,   //L"K_?94",				// &H94
	0,   //L"K_?95",				// &H95
	0,   //L"K_?96",				// &H96
	0,   //L"K_?97",				// &H97
	0,   //L"K_?98",				// &H98
	0,   //L"K_?99",				// &H99
	0,   //L"K_?9A",				// &H9A
	0,   //L"K_?9B",				// &H9B
	0,   //L"K_?9C",				// &H9C
	0,   //L"K_?9D",				// &H9D
	0,   //L"K_?9E",				// &H9E
	0,   //L"K_?9F",				// &H9F
	0,   //L"K_?A0",				// &HA0
	0,   //L"K_?A1",				// &HA1
	0,   //L"K_?A2",				// &HA2
	0,   //L"K_?A3",				// &HA3
	0,   //L"K_?A4",				// &HA4
	0,   //L"K_?A5",				// &HA5
	0,   //L"K_?A6",				// &HA6
	0,   //L"K_?A7",				// &HA7
	0,   //L"K_?A8",				// &HA8
	0,   //L"K_?A9",				// &HA9
	0,   //L"K_?AA",				// &HAA
	0,   //L"K_?AB",				// &HAB
	0,   //L"K_?AC",				// &HAC
	0,   //L"K_?AD",				// &HAD
	0,   //L"K_?AE",				// &HAE
	0,   //L"K_?AF",				// &HAF
	0,   //L"K_?B0",				// &HB0
	0,   //L"K_?B1",				// &HB1
	0,   //L"K_?B2",				// &HB2
	0,   //L"K_?B3",				// &HB3
	0,   //L"K_?B4",				// &HB4
	0,   //L"K_?B5",				// &HB5
	0,   //L"K_?B6",				// &HB6
	0,   //L"K_?B7",				// &HB7
	0,   //L"K_?B8",				// &HB8
	0,   //L"K_?B9",				// &HB9

	0,   //L"K_COLON",				// &HBA
	0,   //L"K_EQUAL",				// &HBB
	0,   //L"K_COMMA",				// &HBC
	0,   //L"K_HYPHEN",			// &HBD
	0,   //L"K_PERIOD",			// &HBE
	0,   //L"K_SLASH",				// &HBF
	0,   //L"K_BKQUOTE",			// &HC0

	0,   //L"K_?C1",				// &HC1
	0,   //L"K_?C2",				// &HC2
	0,   //L"K_?C3",				// &HC3
	0,   //L"K_?C4",				// &HC4
	0,   //L"K_?C5",				// &HC5
	0,   //L"K_?C6",				// &HC6
	0,   //L"K_?C7",				// &HC7
	0,   //L"K_?C8",				// &HC8
	0,   //L"K_?C9",				// &HC9
	0,   //L"K_?CA",				// &HCA
	0,   //L"K_?CB",				// &HCB
	0,   //L"K_?CC",				// &HCC
	0,   //L"K_?CD",				// &HCD
	0,   //L"K_?CE",				// &HCE
	0,   //L"K_?CF",				// &HCF
	0,   //L"K_?D0",				// &HD0
	0,   //L"K_?D1",				// &HD1
	0,   //L"K_?D2",				// &HD2
	0,   //L"K_?D3",				// &HD3
	0,   //L"K_?D4",				// &HD4
	0,   //L"K_?D5",				// &HD5
	0,   //L"K_?D6",				// &HD6
	0,   //L"K_?D7",				// &HD7
	0,   //L"K_?D8",				// &HD8
	0,   //L"K_?D9",				// &HD9
	0,   //L"K_?DA",				// &HDA

	0,   //L"K_LBRKT",				// &HDB
	0,   //L"K_BKSLASH",			// &HDC
	0,   //L"K_RBRKT",				// &HDD
	0,   //L"K_QUOTE",				// &HDE
	0,   //L"K_oDF",				// &HDF
	0,   //L"K_oE0",				// &HE0
	0,   //L"K_oE1",				// &HE1
	0,   //L"K_oE2",				// &HE2
	0,   //L"K_oE3",				// &HE3
	0,   //L"K_oE4",				// &HE4

	0,   //L"K_?E5",				// &HE5

	0,   //L"K_oE6",				// &HE6

	0,   //L"K_?E7",				// &HE7
	0,   //L"K_?E8",				// &HE8

	0,   //L"K_oE9",				// &HE9
	0,   //L"K_oEA",				// &HEA
	0,   //L"K_oEB",				// &HEB
	0,   //L"K_oEC",				// &HEC
	0,   //L"K_oED",				// &HED
	0,   //L"K_oEE",				// &HEE
	0,   //L"K_oEF",				// &HEF
	0,   //L"K_oF0",				// &HF0
	0,   //L"K_oF1",				// &HF1
	0,   //L"K_oF2",				// &HF2
	0,   //L"K_oF3",				// &HF3
	0,   //L"K_oF4",				// &HF4
	0,   //L"K_oF5",				// &HF5

	0,   //L"K_?F6",				// &HF6
	0,   //L"K_?F7",				// &HF7
	0,   //L"K_?F8",				// &HF8
	0,   //L"K_?F9",				// &HF9
	0,   //L"K_?FA",				// &HFA
	0,   //L"K_?FB",				// &HFB
	0,   //L"K_?FC",				// &HFC
	0,   //L"K_?FD",				// &HFD
	0,   //L"K_?FE",				// &HFE
	0,   //L"K_?FF"				// &HFF
};


void ldml_processor::emit_invalidate_passthrough_keystroke(km_core_state *state, km_core_virtual_key vk, uint16_t _kmn_unused(modifier_state)) {
  if (ldml_VKContextReset[vk]) {
    state->actions().push_invalidate_context();
  }
  state->actions().push_emit_keystroke();
}

size_t
ldml_processor::context_to_string(km_core_state *state, std::u32string &str, bool include_markers) {
    str.clear();
    auto &cp      = state->context();
    size_t ctxlen = 0; // TODO-LDML: not used by callers?
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

} // namespace core
} // namespace km
