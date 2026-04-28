/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Marc Durdin on 2022-08-05
 *
 * Keyman Core - Test framework for testing LDML keyboard processor
 */

#include <algorithm>
#include <cctype>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <iterator>
#include <list>
#include <sstream>
#include <string>
#include <type_traits>
#include <set>

#include "path.hpp"
#include "state.hpp"
#include "utfcodec.hpp"

#include "keyman_core.h"
#include "util_normalize.hpp"

#include <kmx/kmx_xstring.h>  // for surrogate pair macros

#include "ldml_test_source.hpp"
#include "debuglog.h"

#include "ldml/ldml_markers.hpp"

#include "processor.hpp"

#include "../helpers/core_test_helpers.h"

void print_context(std::u16string &text_store, km_core_state *&test_state, std::list<km_core_context_item> &test_context);

bool g_beep_found = false;

std::string
string_to_hex(const std::u16string &input) {
  std::ostringstream result;
  result << std::setfill('0') << std::hex << std::uppercase;

  for (size_t i = 0; i < input.length(); i++) {
    unsigned int ch = input[i];
    if (i < input.length() - 1 && Uni_IsSurrogate1(input[i]) && Uni_IsSurrogate2(input[i + 1])) {
      ch = Uni_SurrogateToUTF32(input[i], input[i + 1]);
      i++;
    }

    result << "U+" << std::setw(4) << ch << " ";
  }
  return result.str();
}


void
copy_context_items_to_list(km_core_context_item *citems, std::list<km_core_context_item> &list) {
  list.clear();
  for(km_core_context_item *ci = citems; ci->type != KM_CORE_CT_END; ci++) {
    list.emplace_back(*ci);
  }
}


void
apply_action(
    km_core_state *test_state,
    km_core_action_item const &act,
    std::u16string &text_store,
    std::list<km_core_context_item> &context,
    km::tests::LdmlTestSource &test_source,
    std::list<km_core_context_item> &test_context) {

  // print_context(text_store, test_state, test_context);
  switch (act.type) {
  case KM_CORE_IT_END:
    FAIL();
    break;
  case KM_CORE_IT_ALERT:
    g_beep_found = true;
    std::cout << "  + beep" << std::endl;
    break;
  case KM_CORE_IT_CHAR:
    context.push_back(km_core_context_item{
        KM_CORE_CT_CHAR,
        {
            0,
        },
        {act.character}});
    {
      km::core::kmx::char16_single buf;
      const int len = km::core::kmx::Utf32CharToUtf16(act.character, buf);
      for(int i=0; i<len; i++) {
        text_store.push_back(buf.ch[i]);
      }
    }
    std::cout << "  + char(" << act.character << ")" << std::endl;
    break;
  case KM_CORE_IT_MARKER:
    std::cout << "  + deadkey(" << act.marker << ")" << std::endl;
    context.push_back(km_core_context_item{
        KM_CORE_CT_MARKER,
        {
            0,
        },
        {(uint32_t)act.marker}});
    break;
  case KM_CORE_IT_BACK:
  {
    std::cout << "  + back(" << act.backspace.expected_type << ")" << std::endl;

    // single char removed in context
    km_core_usv ch = 0;
    bool matched_text = false;
    // assume the backspace came from set_action() and there's no further info.
    ASSERT_EQ(act.backspace.expected_type, KM_CORE_BT_CHAR);
    ASSERT_EQ(act.backspace.expected_value, 0);
    // It is valid for a backspace to be received with an empty text store
    // as the user can press backspace with no text in the store and Keyman
    // will pass that back to the client, as the client may do additional
    // processing at start of a text store, e.g. delete from a previous cell
    // in a table. Or, if Keyman has a cached context, then there may be
    // additional text in the text store that Keyman can't see.
    // If there's anything in the text store, pop it off.  Two if a pair.
    if (text_store.length() > 0) {
      ASSERT_FALSE(context.empty());
      ASSERT_FALSE(text_store.empty());
      const auto ch1 = text_store.back();
      text_store.pop_back();
      if (text_store.length() > 0 && Uni_IsSurrogate2(ch1)) {
        const auto ch2 = text_store.back();
        if (Uni_IsSurrogate1(ch2)) {
          // We'll only pop the next character off it is actually a
          // surrogate pair
          ch = Uni_SurrogateToUTF32(ch2, ch1); // reverse order
          text_store.pop_back();
        } else {
          ch = 0xFFFF; // unpaired
        }
      } else {
        ch = ch1; // single char
      }
    } else {
      matched_text = true; // no text to match as context is empty.
    }
    // now, we need to simulate what ldml_processor::emit_backspace() is going to do.
    while (!context.empty()) {
      if (context.back().type == KM_CORE_CT_CHAR) {
        ASSERT_FALSE(matched_text);
        ASSERT_EQ(context.back().character, ch); // expect popped char to be same as what's in context
        matched_text = true;
        context.pop_back();
        break;  // exit on first real char
      }
      ASSERT_NE(context.back().type, KM_CORE_CT_END);  // inappropriate here.
      context.pop_back();
    }
    ASSERT_TRUE(matched_text);
    break;
  }
  case KM_CORE_IT_PERSIST_OPT:
    std::cout << "  + TODO-LDML: persist_opt()" << std::endl;
    break;
  case KM_CORE_IT_INVALIDATE_CONTEXT:
    {
      std::cout << "  + context invalidated (markers cleared)" << std::endl;
      // TODO-LDML: We need the context for tests. So we will simulate recreating
      // the context from the context string.
      km_core_context_item* new_context_items = nullptr;
      // We replace the cached context with the current application context
      ASSERT_STATUS_OK(context_items_from_utf16(text_store.c_str(), &new_context_items));
      copy_context_items_to_list(new_context_items, context);
      // also update the test context
      copy_context_items_to_list(new_context_items, test_context);
      // TODO-LDML: now we need to SET the core context!
      ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), new_context_items));
      km_core_context_items_dispose(new_context_items);
    }
    break;
  case KM_CORE_IT_EMIT_KEYSTROKE:
    std::cout << "  + emit keystroke" << std::endl;
    // TODO-LDML: For now, this is a no-op. We could handle enter, etc.
    break;
  case KM_CORE_IT_CAPSLOCK:
    std::cout << "  + capsLock " << act.capsLock << std::endl;
    test_source.set_caps_lock_on(act.capsLock);
    break;
  default:
    FAIL();  // NOT SUPPORTED
    break;
  }
}


void
apply_actions(
  km_core_state *test_state,
  km_core_actions const *actions,
  std::u16string &text_store,
  std::list<km_core_context_item> &context,
  km::tests::LdmlTestSource &test_source,
  std::list<km_core_context_item> &test_context
) {

  if(actions->do_alert) {
    ASSERT_NO_FATAL_FAILURE(apply_action(test_state, {KM_CORE_IT_ALERT}, text_store, context, test_source, test_context));
  }

  if(actions->code_points_to_delete) {
    for(unsigned int i = 0; i < actions->code_points_to_delete; i++) {
      km_core_action_item act = {KM_CORE_IT_BACK};
      act.backspace = {KM_CORE_BT_CHAR, 0};
      ASSERT_NO_FATAL_FAILURE(apply_action(test_state, act, text_store, context, test_source, test_context));
    }
  }

  if(actions->output) {
    for(auto ch = actions->output; *ch; ch++) {
      km_core_action_item act = {KM_CORE_IT_CHAR};
      act.character = *ch;
      ASSERT_NO_FATAL_FAILURE(apply_action(test_state, act, text_store, context, test_source, test_context));
    }
  }

  if(actions->emit_keystroke) {
      ASSERT_NO_FATAL_FAILURE(apply_action(test_state, {KM_CORE_IT_EMIT_KEYSTROKE}, text_store, context, test_source, test_context));
  }

  // TODO-LDML: other action types - persist, caps lock
}

void
print_context(std::u16string &text_store, km_core_state *&test_state, std::list<km_core_context_item> &test_context) {
  // Compare context and text store at each step - should be identical
  size_t n                     = 0;
  km_core_context_item *citems = nullptr;
  ASSERT_STATUS_OK(km_core_context_get(km_core_state_context(test_state), &citems));
  ASSERT_STATUS_OK(context_items_to_utf16(citems, nullptr, &n));
  km_core_cu *buf = new km_core_cu[n];
  ASSERT_STATUS_OK(context_items_to_utf16(citems, buf, &n));
  std::cout << "context (raw): ";  // output including markers (which aren't in 'buf' here)
  for (auto ci = citems; ci->type != KM_CORE_CT_END; ci++) {
    switch (ci->type) {
    case KM_CORE_CT_CHAR:
      std::cout << "U+" << std::setw(4) << std::hex << ci->character << std::dec << " ";
      break;
    case KM_CORE_CT_MARKER:
      std::cout << "\\m{" << ci->character << "} ";
      break;
    default:
      std::cout << "type#" << ci->type << " ";
    }
  }
  std::cout << std::endl;
  std::cout << "context      : " << string_to_hex(buf) << " [" << buf << "]" << std::endl;

  delete[] buf;

  km_core_context_item *citems_app = nullptr;
  ASSERT_STATUS_OK(km_core_context_get(km_core_state_app_context(test_state), &citems_app));
  ASSERT_STATUS_OK(context_items_to_utf16(citems_app, nullptr, &n));
  buf = new km_core_cu[n];
  ASSERT_STATUS_OK(context_items_to_utf16(citems_app, buf, &n));
  std::cout << "app context  : " << string_to_hex(buf) << " [" << buf << "]" << std::endl;

  std::cout << "test_context : ";
  std::cout.fill('0');
  for (auto i = test_context.begin(); i != test_context.end(); i++) {
    switch (i->type) {
    case KM_CORE_CT_CHAR:
      std::cout << "U+" << std::setw(4) << std::hex << i->character << std::dec << " ";
      break;
    case KM_CORE_CT_MARKER:
      std::cout << "\\m{" << i->character << "} ";
      break;
    default:
      std::cout << "type#" << i->type << " ";
    }
  }
  std::cout << std::endl;
  km_core_context_items_dispose(citems);
  km_core_context_items_dispose(citems_app);
  delete[] buf;
}

/**
 * verify the current context
 */
void
verify_context(std::u16string &text_store, km_core_state *&test_state, std::list<km_core_context_item> &test_context, bool fully_normalized_mode, bool normalization_enabled) {
  // Compare context and text store at each step - should be identical
  ASSERT_NO_FATAL_FAILURE(print_context(text_store, test_state, test_context));

  size_t n                     = 0;
  km_core_context_item *citems = nullptr;
  ASSERT_STATUS_OK(km_core_context_get(km_core_state_context(test_state), &citems));
  ASSERT_STATUS_OK(context_items_to_utf16(citems, nullptr, &n));
  km_core_cu *buf = new km_core_cu[n];
  ASSERT_STATUS_OK(context_items_to_utf16(citems, buf, &n));
  std::u16string buf_final(buf);

  if(fully_normalized_mode) {
    if(normalization_enabled) {
      ASSERT_TRUE(km::core::util::normalize_nfc(buf_final));
    }
    // note: we don't compare test_context with citems because they have different
    // normalization forms in fully_normalized_mode
  } else {
    // Verify that both our local test_context and the core's test_state.context have
    // not diverged
    auto ci = citems;
    for (auto test_ci = test_context.begin();; ci++, test_ci++) {
      // skip over markers, they won't be in test_context
      while (ci->type == KM_CORE_CT_MARKER) {
        ci++;
      }
      // exit if BOTH are at end.
      if (ci->type == KM_CORE_CT_END && test_ci == test_context.end()) {
        break;  // success
      }
      // fail if only ONE is at end
      ASSERT_TRUE(ci->type != KM_CORE_CT_END && test_ci != test_context.end());
      // fail if type and marker don't match.
      ASSERT_TRUE(test_ci->type == ci->type && test_ci->marker == ci->marker);
    }
  }

  km_core_context_items_dispose(citems);

  if (text_store != buf_final) {
    std::cerr << "text store has diverged from buf" << std::endl;
    std::cerr << "text store: " << string_to_hex(text_store) << " [" << text_store << "]" << std::endl;
    std::cerr << "buf_final : " << string_to_hex(buf_final) << " [" << buf << "]" << std::endl;
    std::cerr << "buf       : " << string_to_hex(buf) << " [" << buf << "]" << std::endl;
    FAIL();
  }
  delete[] buf;
}


/**
 * @param actual the list from get_key_list()
 * @param expected optional list with keys to check, can be empty - not exhaistive
 * @returns true if passing
 */
bool
verify_key_list(std::set<km::tests::key_event> &actual, std::set<km::tests::key_event> &expected) {
  bool equals = true;
  // error if any bad modifier keys
  for(const auto &akey : actual) {
    if (akey.modifier_state > KM_CORE_MODIFIER_MASK_CAPS) {
      equals = false;
      std::u16string dump = convert<char, char16_t>(akey.dump());  // akey.dump()
      std::wcout << "- FAIL - key_map had key with bad modifier " << akey.modifier_state << ": " << dump << std::endl;
    }
  }
  // error if any expected keys missing (note expected may be empty)
  for(const auto &ekey : expected) {
    if (actual.count(ekey) == 0) {
      equals = false;
      std::u16string dump = convert<char, char16_t>(ekey.dump());  // akey.dump()
      std::wcout << "- FAIL - key_map had missing key " << dump << std::endl;
    }
  }
  if (equals) {
      std::wcout << " " << actual.size() << " vkeys OK, verified  " << expected.size() << std::endl;
  }
  return equals;
}

/**
 * @param actual_list the list from get_key_list()
 * @param keylist optional string with keys to check, can be empty
 * @param test the LDML test source, for additional data
 * @returns true if passing
 */
bool
verify_key_list(const km_core_keyboard_key *actual_list, const std::u16string &expected_list, const km::tests::LdmlTestSource &test) {
  std::set<km::tests::key_event> actual, expected;
  // convert actual list
  while (actual_list != nullptr && !(actual_list->key == 0 && actual_list->modifier_flag == 0)) {
    km::tests::key_event k(actual_list->key, (uint16_t)actual_list->modifier_flag);
    actual.insert(k);
    actual_list++; // advance pointer
  }
  // parse the expected list
  std::string keylist = convert<char16_t, char>(expected_list);
  while (!keylist.empty() && keylist[0] == '[') {
    const km::tests::key_event k = km::tests::LdmlEmbeddedTestSource::parse_next_key(keylist);
    if (!k.empty()) {
      expected.emplace(k);
    }
  }
  return verify_key_list(actual, expected);
}

void //TODO
run_test(const km::core::path &source, const km::core::path &compiled, km::tests::LdmlTestSource& test_source, bool fully_normalized_mode) {
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;

  const km_core_status expect_load_status = test_source.get_expected_load_status();
  auto blob = km::tests::load_kmx_file(compiled.native().c_str());
  ASSERT_EQ(km_core_keyboard_load_from_blob(compiled.stem().c_str(), blob.data(), blob.size(), &test_kb), expect_load_status);

  if (expect_load_status != KM_CORE_STATUS_OK) {
    std::cout << "Keyboard was expected to be invalid, so exiting " << std::endl;
    return;
  }

  // setup normalization status
  const bool normalization_enabled = test_kb->supports_normalization();
  test_source.set_normalization_disabled(!normalization_enabled);
  std::wcout
    << "* normalization enabled = " << normalization_enabled
    << std::endl << std::endl;

  // Setup state, environment
  ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));

  g_beep_found = false;

  std::list<km_core_context_item> test_context;

  km_core_context_item *citems = nullptr;
  // setup test_context
  std::u16string context;
  ASSERT_NO_FATAL_FAILURE(test_source.get_context(context));
  ASSERT_STATUS_OK(context_items_from_utf16(context.c_str(), &citems));

  if(fully_normalized_mode) {
    km_core_state_context_set_if_needed(test_state, context.c_str());
  } else {
    ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));
  }

  // Make a copy of the setup context for the test
  copy_context_items_to_list(citems, test_context);
  km_core_context_items_dispose(citems);

  // Setup baseline text store
  std::u16string text_store;
  ASSERT_NO_FATAL_FAILURE(test_source.get_context(text_store));

  km::tests::ldml_action action;

  // verify at beginning
  ASSERT_NO_FATAL_FAILURE(verify_context(text_store, test_state, test_context, fully_normalized_mode, normalization_enabled));

  // Run through actions, applying output for each event
  do {
    ASSERT_NO_FATAL_FAILURE(test_source.next_action(action));
    switch (action.type) {
    case km::tests::LDML_ACTION_DONE:
      // We'll get a printout when this loop exits.
      break;
    case km::tests::LDML_ACTION_KEY_EVENT: {
      auto &p = action.k;
      std::cout << std::endl << "- key action: " << km::core::kmx::Debug_VirtualKey_Always(p.vk) << " modifier: ["
                << km::core::kmx::Debug_ModifierName(p.modifier_state) << " 0x" << p.modifier_state << std::dec << "]" << std::endl;
      // Because a normal system tracks caps lock state itself,
      // we mimic that in the tests. We assume caps lock state is
      // updated on key_down before the processor receives the
      // event.
      if (p.vk == KM_CORE_VKEY_CAPS) {
        test_source.toggle_caps_lock_state();
      }

      for (auto key_down = 1; key_down >= 0; key_down--) {
        std::cout << "  - key_down = " << key_down << std::endl;
        // expected error only applies to key down
        ASSERT_STATUS_OK(km_core_process_event(
            test_state, p.vk, p.modifier_state | test_source.caps_lock_state(), key_down,
            KM_CORE_EVENT_FLAG_DEFAULT));  // TODO-LDML: for now. Should send touch and hardware events.

        if (state_should_invalidate_context(test_state, p.vk, p.modifier_state | test_source.caps_lock_state(), key_down,
            KM_CORE_EVENT_FLAG_DEFAULT)) {
          test_context.clear();
          text_store.clear();
        }

        if(fully_normalized_mode) {
          auto actions = km_core_state_get_actions(test_state);
          ASSERT_NO_FATAL_FAILURE(apply_actions(test_state, actions, text_store, test_context, test_source, test_context));
        } else {
          for (auto act = km_core_state_action_items(test_state, nullptr); act->type != KM_CORE_IT_END; act++) {
            ASSERT_NO_FATAL_FAILURE(apply_action(test_state, *act, text_store, test_context, test_source, test_context));
          }
        }
        ASSERT_NO_FATAL_FAILURE(verify_context(text_store, test_state, test_context, fully_normalized_mode, normalization_enabled));
      }
    } break;
    case km::tests::LDML_ACTION_EMIT_STRING: {
      std::cout << "- string emit action: " << action.string << std::endl;
      std::cerr << "TODO-LDML: note, LDML_ACTION_EMIT_STRING is NOT going through keyboard, transforms etc." << std::endl;
      text_store.append(action.string);  // TODO-LDML: not going through keyboard
      // Now, update context?
      km_core_context_item *nitems = nullptr;
      ASSERT_STATUS_OK(context_items_from_utf16(action.string.c_str(), &nitems));
      ASSERT_STATUS_OK(context_append(km_core_state_context(test_state), nitems));
      // update the test_context also.
      for (km_core_context_item *ci = nitems; ci->type != KM_CORE_CT_END; ci++) {
        test_context.emplace_back(*ci);
      }
      km_core_context_items_dispose(nitems);

      ASSERT_NO_FATAL_FAILURE(verify_context(text_store, test_state, test_context, fully_normalized_mode, normalization_enabled));
    } break;
    case km::tests::LDML_ACTION_CHECK_EXPECTED: {
      if (normalization_enabled) {
        if (!fully_normalized_mode) {
          ASSERT_TRUE(km::core::util::normalize_nfd(action.string));
        } else {
          ASSERT_TRUE(km::core::util::normalize_nfc(action.string));
        }
      }
      std::cout << std::endl << "# check expected" << std::endl;
      std::cout << "expected  : " << string_to_hex(action.string) << " [" << action.string << "]" << std::endl;
      std::cout << "text store: " << string_to_hex(text_store) << " [" << text_store << "]" << std::endl;
      // Compare internal context with expected result
      ASSERT_EQ(text_store, action.string);
    } break;
    case km::tests::LDML_ACTION_CHECK_KEYLIST: {
      std::cout << std::endl << "# checking keylist" << std::endl;
      // get keylist from kbd
      const km_core_keyboard_key* actual_list = test_kb->get_key_list();
      ASSERT_TRUE(verify_key_list(actual_list, action.string, test_source));
      delete [] actual_list;
    } break;
    case km::tests::LDML_ACTION_FAIL: {
      // test requested failure
      FAIL();
    } break;
    case km::tests::LDML_ACTION_SKIP: {
      // test requested skip
      std::wcout << "- SKIP: " << action.string
                 << std::endl;
    } break;
    default:
      std::cerr << " Err: unhandled action type " << action.type << std::endl;
      FAIL();
    }
  } while (!action.done());

  // re-verify at end (if there wasn't already a failure)
  ASSERT_NO_FATAL_FAILURE(verify_context(text_store, test_state, test_context, fully_normalized_mode, normalization_enabled));

  // cleanup
  km_core_state_dispose(test_state);
  km_core_keyboard_dispose(test_kb);

  // Test if the beep action was as expected.
  // TODO-LDML: possible in LDML?
  ASSERT_EQ(g_beep_found, test_source.get_expected_beep());
}


class TestData {
public:
  std::string test_name;
  // const char* keyboard_name;

  km::core::path source;
  km::core::path compiled;

  bool fully_normalized_mode;
  std::shared_ptr<km::tests::LdmlTestSource> test_source;

  std::vector<km_core_action_item> action_items;
  km_core_actions actions;
};

std::string GenerateTestName(const testing::TestParamInfo<TestData>& info) {
  return info.param.test_name;
}

class LdmlKeyboardTests : public testing::TestWithParam<TestData> {
protected:
};

TEST_P(LdmlKeyboardTests, TestRun) {
  auto data = GetParam();
  auto test_source = data.test_source.get();
  ASSERT_NO_FATAL_FAILURE(run_test(data.source, data.compiled, *test_source, data.fully_normalized_mode));
}

std::vector<TestData> values;

/**
 * Google Test allows only alphanumeric characters in test names, so strip other
 * characters and title case whenever a non-alpha is found
 */
std::string formatTestName(const std::string& testName) {
  std::string result = "";
  bool upper = true;
  for(auto& ch : testName) {
    if(isalnum(ch)) {
      result += upper ? toupper(ch) : ch;
      upper = false;
    } else {
      upper = true;
    }
  }
  return result;
}

/**
 * Run all tests for this keyboard
 */
bool load_all_tests(const km::core::path &source, const km::core::path &compiled, bool fully_normalized_mode) {
  std::wcout << "* source file   = " << source << std::endl
             << "* compiled file = " << compiled << std::endl;

  auto embedded_test_source = new km::tests::LdmlEmbeddedTestSource();

  const std::string normalMode[] = {"NFD", "NFC"};

  std::vector<std::string> failures; // track failures for summary

  bool embedded_has_tests = false;
  if(!embedded_test_source->load_source(source, compiled, embedded_has_tests)) {
    std::cerr << "failed to load source" << std::endl;
    return false;
  }

  if(embedded_has_tests) {
    // embedded loaded OK, try it
    TestData data;
    data.test_source = std::shared_ptr<km::tests::LdmlTestSource>(embedded_test_source);
    data.test_name = "EmbeddedTest" + normalMode[fully_normalized_mode];
    data.source = source;
    data.compiled = compiled;
    data.fully_normalized_mode = fully_normalized_mode;
    values.push_back(data);
  }

  km::tests::LdmlJsonTestSourceFactory json_factory;
  // adjust path

  const auto json_path = km::tests::LdmlJsonTestSourceFactory::source_to_test_json(source);
  int json_result = json_factory.load(compiled, json_path);
  if (json_result) {
    const km::tests::JsonTestMap& json_tests = json_factory.get_tests();

    if(json_tests.size() == 0) {
      std::cerr << "Error: no json tests found in file" << std::endl;
      return false;
    }

    int k = 1;
    // Loop over all tests
    for (auto& n : json_tests) {
      TestData jsonData;
      jsonData.test_name = formatTestName(n.first);
      jsonData.fully_normalized_mode = fully_normalized_mode;
      jsonData.source = source;
      jsonData.compiled = compiled;

      for(auto& test : values) {
        if(test.test_name == jsonData.test_name) {
          jsonData.test_name += std::to_string(k);
          k++;
        }
      }
      jsonData.test_name += normalMode[fully_normalized_mode];
      jsonData.test_source = n.second;
      values.push_back(jsonData);
    }
  }

  if(!json_result && !embedded_has_tests) {
    std::cerr << "no tests found: embedded = " << embedded_has_tests << " json = " << json_result << std::endl;
    return false;
  }

  if(values.size() == 0) {
    std::cerr << "expected tests" << std::endl;
    return false;
  }

  return true;
}

INSTANTIATE_TEST_SUITE_P(KeymanCore, LdmlKeyboardTests, testing::ValuesIn(values), GenerateTestName);

constexpr const auto help_str =
    "\
ldml <LDML_FILE> <KMX_FILE> [google test parameters]\n\
help:\n\
\tLDML_FILE:\tThe .xml file for the keyboard under test.\n\
\tKMX_FILE:\tThe corresponding compiled kmx file.\n";

int error_args() {
    std::cerr << "ldml: Not enough arguments." << std::endl;
    std::cout << help_str;
    return 1;
}

GTEST_API_ int
main(int argc, char **argv) {
  if(argc < 3) {
    return error_args();
  }

  // We need these parameters before GoogleTest so we can setup the
  // parameterized tests, which is done in InitGoogleTest()

  km::core::path ldml_file, kmx_file;

#ifdef __EMSCRIPTEN__
  if(!get_wasm_file_path(km::core::path(argv[1]), ldml_file) ||
      !get_wasm_file_path(km::core::path(argv[2]), kmx_file)) {
    // not a fully qualified path
    return 1;
  }
#else
  ldml_file = km::core::path(argv[1]);
  kmx_file = km::core::path(argv[2]);
#endif

  argc -= 2;
  argv += 2;

  if(!load_all_tests(ldml_file, kmx_file, false)) return 1;
  if(!load_all_tests(ldml_file, kmx_file, true)) return 2;

  testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}
