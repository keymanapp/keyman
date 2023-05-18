/*
  Copyright:    © SIL International.
  Description:  Tests for LDML keyboard integration
  Create Date:  5 Aug 2022
  Authors:      Marc Durdin (MD)

  Note: Exit codes will be 100*LINE + ERROR CODE, e.g. 25005 is code 5 on line 250
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

#include "path.hpp"
#include "state.hpp"
#include "utfcodec.hpp"

#include <test_assert.h>
#include <test_color.h>

#include <kmx/kmx_xstring.h>  // for surrogate pair macros

#include "ldml_test_source.hpp"

namespace {

bool g_beep_found = false;

km_kbp_option_item test_env_opts[] =
{
  KM_KBP_OPTIONS_END
};

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
apply_action(
    km_kbp_state const *,
    km_kbp_action_item const &act,
    std::u16string &text_store,
    std::vector<km_kbp_context_item> &context,
    km::tests::LdmlTestSource &test_source) {
  switch (act.type) {
  case KM_KBP_IT_END:
    assert(false);
    break;
  case KM_KBP_IT_ALERT:
    g_beep_found = true;
    // std::cout << "beep" << std::endl;
    break;
  case KM_KBP_IT_CHAR:
    context.push_back(km_kbp_context_item{
        KM_KBP_CT_CHAR,
        {
            0,
        },
        {act.character}});
    {
      km::kbp::kmx::char16_single buf;
      const int len = km::kbp::kmx::Utf32CharToUtf16(act.character, buf);
      for(int i=0; i<len; i++) {
        text_store.push_back(buf.ch[i]);
      }
    }
    // std::cout << "char(" << act.character << ") size=" << cp->size() << std::endl;
    break;
  case KM_KBP_IT_MARKER:
    // std::cout << "deadkey(" << act.marker << ")" << std::endl;
    context.push_back(km_kbp_context_item{
        KM_KBP_CT_MARKER,
        {
            0,
        },
        {(uint32_t)act.marker}});
    break;
  case KM_KBP_IT_BACK:
    // It is valid for a backspace to be received with an empty text store
    // as the user can press backspace with no text in the store and Keyman
    // will pass that back to the client, as the client may do additional
    // processing at start of a text store, e.g. delete from a previous cell
    // in a table. Or, if Keyman has a cached context, then there may be
    // additional text in the text store that Keyman can't see.
    if (act.backspace.expected_type == KM_KBP_BT_MARKER) {
      assert(!context.empty());
      assert(context.back().type == KM_KBP_CT_MARKER);
      context.pop_back();
    } else if (text_store.length() > 0) {
      assert(!context.empty() && !text_store.empty());
      km_kbp_usv ch = text_store.back();
      text_store.pop_back();
      if (text_store.length() > 0 && Uni_IsSurrogate2(ch)) {
        auto ch1 = text_store.back();
        if (Uni_IsSurrogate1(ch1)) {
          // We'll only pop the next character off it is actually a
          // surrogate pair
          ch = Uni_SurrogateToUTF32(ch1, ch);
          text_store.pop_back();
        }
      }
      assert(ch == act.backspace.expected_value);

      assert(context.back().type == KM_KBP_CT_CHAR);
      assert(context.back().character == ch);
      context.pop_back();
    }
    break;
  case KM_KBP_IT_PERSIST_OPT:
    break;
  case KM_KBP_IT_INVALIDATE_CONTEXT:
    std::cout << "action: context invalidated (markers cleared)" << std::endl;
    break;
  case KM_KBP_IT_EMIT_KEYSTROKE:
    std::cout << "action: emit keystroke" << std::endl;
    break;
  case KM_KBP_IT_CAPSLOCK:
    std::cout << "action: capsLock " << act.capsLock << std::endl;
    test_source.set_caps_lock_on(act.capsLock);
    break;
  default:
    assert(false);  // NOT SUPPORTED
    break;
  }
}

/**
 * verify the current context
*/
void
verify_context(std::u16string& text_store, km_kbp_state* &test_state, std::vector<km_kbp_context_item> &test_context) {
      // Compare context and text store at each step - should be identical
    size_t n = 0;
    km_kbp_context_item* citems = nullptr;
    try_status(km_kbp_context_get(km_kbp_state_context(test_state), &citems));
    try_status(km_kbp_context_items_to_utf16(citems, nullptr, &n));
    km_kbp_cp *buf = new km_kbp_cp[n];
    try_status(km_kbp_context_items_to_utf16(citems, buf, &n));
    std::cout << "context   : " << string_to_hex(buf) << " [" << buf << "]" << std::endl;

    // Verify that both our local test_context and the core's test_state.context have
    // not diverged
    auto ci = citems;
    for (auto test_ci = test_context.begin(); ci->type != KM_KBP_CT_END || test_ci != test_context.end(); ci++, test_ci++) {
      assert(ci->type != KM_KBP_CT_END && test_ci != test_context.end());  // Verify that both lists are same length
      assert(test_ci->type == ci->type && test_ci->marker == ci->marker);
    }

    km_kbp_context_items_dispose(citems);
    if (text_store != buf) {
      std::cerr << "text store has diverged from buf" << std::endl;
      std::cerr << "text store: " << string_to_hex(text_store) << " [" << text_store << "]" << std::endl;
      assert(false);
    }
    delete [] buf;

}

int
run_test(const km::kbp::path &source, const km::kbp::path &compiled, km::tests::LdmlTestSource& test_source) {
  km_kbp_keyboard * test_kb = nullptr;
  km_kbp_state * test_state = nullptr;

  const km_kbp_status expect_load_status = test_source.get_expected_load_status();
  assert_equal(km_kbp_keyboard_load(compiled.c_str(), &test_kb), expect_load_status);

  if (expect_load_status != KM_KBP_STATUS_OK) {
    std::cout << "Keyboard was expected to be invalid, so exiting " << std::endl;
    return 0;
  }

  // Setup state, environment
  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));

  // Setup context
  km_kbp_context_item *citems = nullptr;
  try_status(km_kbp_context_items_from_utf16(test_source.get_context().c_str(), &citems));
  try_status(km_kbp_context_set(km_kbp_state_context(test_state), citems));

  // Make a copy of the setup context for the test
  std::vector<km_kbp_context_item> test_context;
  for(km_kbp_context_item *ci = citems; ci->type != KM_KBP_CT_END; ci++) {
    test_context.emplace_back(*ci);
  }
  km_kbp_context_items_dispose(citems);

  // Setup baseline text store
  std::u16string text_store = test_source.get_context();

  km::tests::ldml_action action;

  // verify at beginning
  verify_context(text_store, test_state, test_context);

  // Run through actions, applying output for each event
  for (test_source.next_action(action); action.type != km::tests::LDML_ACTION_DONE; test_source.next_action(action)) {
    if (action.type == km::tests::LDML_ACTION_KEY_EVENT) {
      auto &p = action.k;
      std::cout << "- key action: 0x" << std::hex << p.vk << "/modifier 0x" << p.modifier_state << std::dec << std::endl;
      // Because a normal system tracks caps lock state itself,
      // we mimic that in the tests. We assume caps lock state is
      // updated on key_down before the processor receives the
      // event.
      if (p.vk == KM_KBP_VKEY_CAPS) {
        test_source.toggle_caps_lock_state();
      }

      for (auto key_down = 1; key_down >= 0; key_down--) {
        // expected error only applies to key down
        try_status(km_kbp_process_event(test_state, p.vk, p.modifier_state | test_source.caps_lock_state(), key_down, KM_KBP_EVENT_FLAG_DEFAULT)); // TODO-LDML: for now. Should send touch and hardware events.

        for (auto act = km_kbp_state_action_items(test_state, nullptr); act->type != KM_KBP_IT_END; act++) {
          apply_action(test_state, *act, text_store, test_context, test_source);
        }
      }
      verify_context(text_store, test_state, test_context);
    } else if (action.type == km::tests::LDML_ACTION_EMIT_STRING) {
      std::cout << "- string emit action: " << action.string << std::endl;
      std::cerr << "TODO-LDML: note, LDML_ACTION_EMIT_STRING is NOT going through keyboard, transforms etc." << std::endl;
      text_store.append(action.string); // TODO-LDML: not going through keyboard
      // Now, update context?
      km_kbp_context_item *nitems = nullptr;
      try_status(km_kbp_context_items_from_utf16(action.string.c_str(), &nitems));
      try_status(km_kbp_context_append(km_kbp_state_context(test_state), nitems));
      // update the test_context also.
      for (km_kbp_context_item *ci = nitems; ci->type != KM_KBP_CT_END; ci++) {
        test_context.emplace_back(*ci);
      }
      km_kbp_context_items_dispose(nitems);

      verify_context(text_store, test_state, test_context);
    } else if (action.type == km::tests::LDML_ACTION_CHECK_EXPECTED) {
      std::cout << "- check expected" << std::endl;
      std::cout << "expected  : " << string_to_hex(action.string) << " [" << action.string << "]" << std::endl;
      std::cout << "text store: " << string_to_hex(text_store) << " [" << text_store << "]" << std::endl;
      // Compare internal context with expected result
      if (text_store != action.string) return __LINE__;
    } else if (action.type == km::tests::LDML_ACTION_FAIL) {
      // test requested failure
      std::cout << "- FAIL: " << action.string << std::endl;
      return __LINE__;
    } else {
      std::cerr << " Err: unhandled action type " << action.type << std::endl;
      return __LINE__;
    }
  }
  std::cout << "- DONE" << std::endl;

  // Test if the beep action was as expected
  if (g_beep_found != test_source.get_expected_beep())
    return __LINE__;


  // re-verify at end.
  verify_context(text_store, test_state, test_context);

  // Verify that both our local test_context and the core's test_state.context have
  // not diverged
  try_status(km_kbp_context_get(km_kbp_state_context(test_state), &citems));
  auto ci = citems;
  for(auto test_ci = test_context.begin(); ci->type != KM_KBP_CT_END || test_ci != test_context.end(); ci++, test_ci++) {
    assert(ci->type != KM_KBP_CT_END && test_ci != test_context.end()); // Verify that both lists are same length
    assert(test_ci->type == ci->type && test_ci->marker == ci->marker);
  }

  km_kbp_context_items_dispose(citems);

  // Destroy them
  km_kbp_state_dispose(test_state);
  km_kbp_keyboard_dispose(test_kb);

  return 0;
}

/**
 * Run all tests for this keyboard
 */
int run_all_tests(const km::kbp::path &source, const km::kbp::path &compiled) {
  std::cout << "source file   = " << source << std::endl
            << "compiled file = " << compiled << std::endl;

  km::tests::LdmlEmbeddedTestSource embedded_test_source;

  std::vector<std::string> failures; // track failures for summary

  int embedded_result = embedded_test_source.load_source(source);

  if (embedded_result == 0) {
    // embedded loaded OK, try it
    std::cout << "TEST: " << source.name() << " (embedded)" << std::endl;
    embedded_result = run_test(source, compiled, embedded_test_source);
    if (embedded_result != 0) {
        failures.push_back("in-XML (@@ comment) embedded test failed");
    }
  } else {
    // Not an error in itself, if JSON is present.
    embedded_result = -1; // load failed
  }

  km::tests::LdmlJsonTestSourceFactory json_factory;
  // adjust path

  const auto json_path = km::tests::LdmlJsonTestSourceFactory::kmx_to_test_json(compiled);
  int json_result = json_factory.load(compiled, json_path);
  if (json_result != -1) {
    const km::tests::JsonTestMap& json_tests = json_factory.get_tests();

    assert(json_tests.size() > 0);
    // Loop over all tests
    for (const auto& n : json_tests) {
      std::cout << "TEST: " << json_path.stem() << "/" << n.first << std::endl;
      int sub_test = run_test(source, compiled, *n.second);
      if (sub_test != 0) {
        std::cout << " FAIL: " << json_path.stem() << "/" << n.first << std::endl;
        failures.push_back(json_path.stem() + "/" + n.first);
        json_result = sub_test; // set to last failure
      } else {
        std::cout << " PASS: " << json_path.stem() << "/" << n.first << std::endl;
      }
    }
    std::cout << " " << json_tests.size() << " JSON test(s)  in "  << json_path.stem() << std::endl;
  }


  // OK.
  if (embedded_result == -1) {
    std::cout << "Note: No embedded test." << std::endl;
  }
  if (json_result == -1) {
    std::cout << "Note: No json test." << std::endl;
  }

  // if both are missing, that's an error in itself.
  if (embedded_result == -1 && json_result == -1) {
    // Can't both be missing.
    failures.push_back("Error: Need either embedded test (@@ directives) or json test");
  }

  // recap the failures
  if (failures.size() > 0) {
    for (const auto& f : failures) {
      std::cerr << "failure summary: " << f << std::endl;
    }
    return -1;
  } else {
    std::cout << "run_all_tests passed" << std::endl;
    return 0;
  }
}



constexpr const auto help_str =
    "\
ldml [--color] <LDML_TEST_FILE> <KMX_FILE>\n\
help:\n\
\tKMN_FILE:\tThe ldml test file for the keyboard under test.\n\
\tKMX_FILE:\tThe corresponding compiled kmx file.\n";

}  // namespace

int error_args() {
    std::cerr << "ldml: Not enough arguments." << std::endl;
    std::cout << help_str;
    return 1;
}

int main(int argc, char *argv[]) {
  int first_arg = 1;

  if (argc < 3) {
    return error_args();
  }

  auto arg_color = std::string(argv[1]) == "--color";
  if(arg_color) {
    first_arg++;
    if(argc < 4) {
      return error_args();
    }
  }
  console_color::enabled = console_color::isaterminal() || arg_color;

  int rc = run_all_tests(argv[first_arg], argv[first_arg + 1]);
  if (rc != EXIT_SUCCESS) {
    std::cerr << "FAILED" << std::endl;
    rc = EXIT_FAILURE;
  }
  return rc;
}
