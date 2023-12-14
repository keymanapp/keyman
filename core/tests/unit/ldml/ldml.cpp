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
#include "debuglog.h"

#include "ldml/ldml_transforms.hpp"

namespace {

bool g_beep_found = false;

km_core_option_item test_env_opts[] =
{
  KM_CORE_OPTIONS_END
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
copy_context_items_to_vector(km_core_context_item *citems, std::vector<km_core_context_item> &vector) {
  vector.clear();
  for(km_core_context_item *ci = citems; ci->type != KM_CORE_CT_END; ci++) {
    vector.emplace_back(*ci);
  }
}


void
apply_action(
    km_core_state *test_state,
    km_core_action_item const &act,
    std::u16string &text_store,
    std::vector<km_core_context_item> &context,
    km::tests::LdmlTestSource &test_source,
    std::vector<km_core_context_item> &test_context) {
  switch (act.type) {
  case KM_CORE_IT_END:
    assert(false);
    break;
  case KM_CORE_IT_ALERT:
    g_beep_found = true;
    // std::cout << "beep" << std::endl;
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
    // std::cout << "char(" << act.character << ") size=" << cp->size() << std::endl;
    break;
  case KM_CORE_IT_MARKER:
    // std::cout << "deadkey(" << act.marker << ")" << std::endl;
    context.push_back(km_core_context_item{
        KM_CORE_CT_MARKER,
        {
            0,
        },
        {(uint32_t)act.marker}});
    break;
  case KM_CORE_IT_BACK:
    // It is valid for a backspace to be received with an empty text store
    // as the user can press backspace with no text in the store and Keyman
    // will pass that back to the client, as the client may do additional
    // processing at start of a text store, e.g. delete from a previous cell
    // in a table. Or, if Keyman has a cached context, then there may be
    // additional text in the text store that Keyman can't see.
    if (act.backspace.expected_type == KM_CORE_BT_MARKER) {
      assert(!context.empty());
      assert(context.back().type == KM_CORE_CT_MARKER);
      context.pop_back();
      // no change to text store.
    } else if (text_store.length() > 0) {
      assert(!context.empty() && !text_store.empty());
      km_core_usv ch = text_store.back();
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
      if (act.backspace.expected_type == KM_CORE_BT_CHAR) {
        assert(ch == act.backspace.expected_value);
        assert(context.back().type == KM_CORE_CT_CHAR);
        assert(context.back().character == ch);
        context.pop_back();
      } else {
        // assume it's otherwise KM-coRE_BT_UNKNOWN
        assert(act.backspace.expected_type == KM_CORE_BT_UNKNOWN);
        assert(context.empty()); // if KM_CORE_BT_UNKNOWN, context should be empty.
      }
    }
    break;
  case KM_CORE_IT_PERSIST_OPT:
    break;
  case KM_CORE_IT_INVALIDATE_CONTEXT:
    {
      std::cout << "action: context invalidated (markers cleared)" << std::endl;
      // TODO-LDML: We need the context for tests. So we will simulate recreating
      // the context from the context string.
      km_core_context_item* new_context_items = nullptr;
      // We replace the cached context with the current application context
      km_core_status status = km_core_context_items_from_utf16(text_store.c_str(), &new_context_items);
      assert(status == KM_CORE_STATUS_OK);
      copy_context_items_to_vector(new_context_items, context);
      // also update the test context
      copy_context_items_to_vector(new_context_items, test_context);
      // TODO-LDML: now we need to SET the core context!
      status = km_core_context_set(km_core_state_context(test_state), new_context_items);
      km_core_context_items_dispose(new_context_items);
    }
    break;
  case KM_CORE_IT_EMIT_KEYSTROKE:
    std::cout << "action: emit keystroke" << std::endl;
    // TODO-LDML: For now, this is a no-op. We could handle enter, etc.
    break;
  case KM_CORE_IT_CAPSLOCK:
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
verify_context(std::u16string& text_store, km_core_state* &test_state, std::vector<km_core_context_item> &test_context) {
      // Compare context and text store at each step - should be identical
    size_t n = 0;
    km_core_context_item* citems = nullptr;
    try_status(km_core_context_get(km_core_state_context(test_state), &citems));
    try_status(km_core_context_items_to_utf16(citems, nullptr, &n));
    km_core_cp *buf = new km_core_cp[n];
    try_status(km_core_context_items_to_utf16(citems, buf, &n));
    std::cout << "context   : " << string_to_hex(buf) << " [" << buf << "]" << std::endl;
    std::cout << "testcontext ";
    std::cout.fill('0');
    for (auto i = test_context.begin(); i < test_context.end(); i++) {
      switch(i->type) {
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

    // Verify that both our local test_context and the core's test_state.context have
    // not diverged
    auto ci = citems;
    for (auto test_ci = test_context.begin(); ci->type != KM_CORE_CT_END || test_ci != test_context.end(); ci++, test_ci++) {
      assert(ci->type != KM_CORE_CT_END && test_ci != test_context.end());  // Verify that both lists are same length
      assert(test_ci->type == ci->type && test_ci->marker == ci->marker);
    }

    km_core_context_items_dispose(citems);
    if (text_store != buf) {
      std::cerr << "text store has diverged from buf" << std::endl;
      std::cerr << "text store: " << string_to_hex(text_store) << " [" << text_store << "]" << std::endl;
      assert(false);
    }
    delete [] buf;

}

int
run_test(const km::core::path &source, const km::core::path &compiled, km::tests::LdmlTestSource& test_source) {
  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;

  const km_core_status expect_load_status = test_source.get_expected_load_status();
  assert_equal(km_core_keyboard_load(compiled.c_str(), &test_kb), expect_load_status);

  if (expect_load_status != KM_CORE_STATUS_OK) {
    std::cout << "Keyboard was expected to be invalid, so exiting " << std::endl;
    return 0;
  }

  // Setup state, environment
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));

  std::vector<km_core_context_item> test_context;

  km_core_context_item *citems = nullptr;
  // setup test_context
  try_status(km_core_context_items_from_utf16(test_source.get_context().c_str(), &citems));
  try_status(km_core_context_set(km_core_state_context(test_state), citems));

  // Make a copy of the setup context for the test
  copy_context_items_to_vector(citems, test_context);
  km_core_context_items_dispose(citems);

  // Setup baseline text store
  std::u16string text_store = test_source.get_context();

  km::tests::ldml_action action;

  // verify at beginning
  verify_context(text_store, test_state, test_context);

  // Run through actions, applying output for each event
  for (test_source.next_action(action); action.type != km::tests::LDML_ACTION_DONE; test_source.next_action(action)) {
    // handle backspace here
    if (action.type == km::tests::LDML_ACTION_KEY_EVENT) {
      auto &p = action.k;
      std::cout << "- key action: " << km::core::kmx::Debug_VirtualKey(p.vk) << "/modifier " << km::core::kmx::Debug_ModifierName(p.modifier_state) << " 0x" << p.modifier_state
                << std::dec << std::endl;
      // Because a normal system tracks caps lock state itself,
      // we mimic that in the tests. We assume caps lock state is
      // updated on key_down before the processor receives the
      // event.
      if (p.vk == KM_CORE_VKEY_CAPS) {
        test_source.toggle_caps_lock_state();
      }

      for (auto key_down = 1; key_down >= 0; key_down--) {
        // expected error only applies to key down
        try_status(km_core_process_event(test_state, p.vk, p.modifier_state | test_source.caps_lock_state(), key_down, KM_CORE_EVENT_FLAG_DEFAULT)); // TODO-LDML: for now. Should send touch and hardware events.

        for (auto act = km_core_state_action_items(test_state, nullptr); act->type != KM_CORE_IT_END; act++) {
          apply_action(test_state, *act, text_store, test_context, test_source, test_context);
        }
      }
      verify_context(text_store, test_state, test_context);
    } else if (action.type == km::tests::LDML_ACTION_EMIT_STRING) {
      std::cout << "- string emit action: " << action.string << std::endl;
      std::cerr << "TODO-LDML: note, LDML_ACTION_EMIT_STRING is NOT going through keyboard, transforms etc." << std::endl;
      text_store.append(action.string); // TODO-LDML: not going through keyboard
      // Now, update context?
      km_core_context_item *nitems = nullptr;
      try_status(km_core_context_items_from_utf16(action.string.c_str(), &nitems));
      try_status(km_core_context_append(km_core_state_context(test_state), nitems));
      // update the test_context also.
      for (km_core_context_item *ci = nitems; ci->type != KM_CORE_CT_END; ci++) {
        test_context.emplace_back(*ci);
      }
      km_core_context_items_dispose(nitems);

      verify_context(text_store, test_state, test_context);
    } else if (action.type == km::tests::LDML_ACTION_CHECK_EXPECTED) {
      assert(km::core::ldml::normalize_nfd(action.string)); // TODO-LDML: should be NFC
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

  // Destroy them
  km_core_state_dispose(test_state);
  km_core_keyboard_dispose(test_kb);

  return 0;
}

/**
 * Run all tests for this keyboard
 */
int run_all_tests(const km::core::path &source, const km::core::path &compiled, const std::string &filter) {
  std::wcout << console_color::fg(console_color::BLUE) << "source file   = " << source << std::endl
            << "compiled file = " << compiled << console_color::reset() << std::endl;
  if(!filter.empty()) {
    std::wcout << "Running only tests matching (substring search): " << filter.c_str() << std::endl;
  }

  km::tests::LdmlEmbeddedTestSource embedded_test_source;

  std::vector<std::string> failures; // track failures for summary

  int embedded_result = embedded_test_source.load_source(source);

  if (!filter.empty()) {
    // Always skip the embedded test if there's a filter.
    std::wcout << console_color::fg(console_color::YELLOW) << "SKIP: " << source.name() << " (embedded)" << console_color::reset()
               << std::endl;
    embedded_result = 0;  // no error
  } else if (embedded_result == 0) {
    // embedded loaded OK, try it
    std::wcout << console_color::fg(console_color::BLUE) << console_color::bold() << "TEST: " << source.name() << " (embedded)"
               << console_color::reset() << std::endl;
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

    size_t skip_count = 0;
    assert(json_tests.size() > 0);
    // Loop over all tests
    for (const auto& n : json_tests) {
      const auto test_name = n.first;
      auto qq = test_name.find(filter);
      if (filter == "--list" || (qq == std::string::npos)) {
        skip_count ++;
        std::wcout << console_color::fg(console_color::YELLOW) << "SKIP: " << json_path.stem().c_str() << "/" << console_color::bold() << n.first.c_str() << console_color::reset() << std::endl;
        continue;
      }
      std::wcout << console_color::fg(console_color::BLUE) << "TEST: " << json_path.stem().c_str() << "/" << console_color::bold() << n.first.c_str() << console_color::reset() << std::endl;
      int sub_test = run_test(source, compiled, *n.second);
      if (sub_test != 0) {
        std::wcout << console_color::fg(console_color::BRIGHT_RED) << "FAIL: " << json_path.stem() << "/" << console_color::bold() << n.first.c_str()
                   << console_color::reset() << std::endl;
        failures.push_back(json_path.stem() + "/" + n.first);
        json_result = sub_test; // set to last failure
      } else {
        std::wcout << console_color::fg(console_color::GREEN) << "PASS: " << console_color::reset() << json_path.stem()
                  << "/" << console_color::bold() << n.first.c_str() << std::endl;
      }
    }
    auto all_count  = json_tests.size();
    auto fail_count = failures.size();
    auto pass_count = all_count - fail_count - skip_count;
    if (pass_count > 0) {
      std::wcout << console_color::fg(console_color::GREEN) << " +" << pass_count;
    }
    if (fail_count > 0) {
     std::wcout << console_color::fg(console_color::BRIGHT_RED) <<
                " -" << fail_count;
    }
    if (skip_count > 0) {
     std::wcout << console_color::fg(console_color::YELLOW) <<
                " (skipped " << skip_count << ")";
    }
    std::wcout << console_color::reset() << " of " << all_count << " JSON tests in "
              << json_path.stem() << std::endl;
  }


  // OK.
  std::wcout << console_color::fg(console_color::YELLOW) << "---- Summary of " << source.name() << " ----" << console_color::reset() << std::endl;
  if (embedded_result == -1) {
    std::wcout << console_color::fg(console_color::YELLOW) << "Note: No embedded test." << console_color::reset() << std::endl;
  }
  if (json_result == -1) {
    std::wcout << console_color::fg(console_color::YELLOW) << "Note: No json test." << console_color::reset() << std::endl;
  }

  // if both are missing, that's an error in itself.
  if (embedded_result == -1 && json_result == -1) {
    // Can't both be missing.
    failures.push_back("Error: Need either embedded test (@@ directives) or json test");
  }

  // recap the failures
  if (failures.size() > 0) {
    for (const auto& f : failures) {
      std::wcerr << console_color::fg(console_color::RED) << "failed: " << f.c_str() << console_color::reset() << std::endl;
    }
    return -1;
  } else {
    std::cout << "run_all_tests passed" << std::endl;
    return 0;
  }
}



constexpr const auto help_str =
    "\
ldml [--color] <LDML_FILE> <KMX_FILE> [ <TEST_FILTER> | --list ]\n\
help:\n\
\tLDML_FILE:\tThe .xml file for the keyboard under test.\n\
\tKMX_FILE:\tThe corresponding compiled kmx file.\n\
\tTEST_FILTER:\tIf present, only run json tests containing the filter substring.  --list will list all tests\n";

}  // namespace

int error_args() {
    std::cerr << "ldml: Not enough arguments." << std::endl;
    std::cout << help_str;
    return 1;
}

int main(int argc, char *argv[]) {
  int first_arg = 1;

  if ((argc - first_arg) < 2) { // if < 2 remaining args
    return error_args();
  }

  auto arg_color = std::string(argv[first_arg]) == "--color";
  if(arg_color) {
    first_arg++;
  }
  console_color::enabled = console_color::isaterminal() || arg_color;

  if ((argc - first_arg) < 2) {
    return error_args();
  }
  const km::core::path ldml_file = argv[first_arg++];
  const km::core::path kmx_file  = argv[first_arg++];

  std::string filter; // default to 'all tests'
  if ((argc - first_arg) >= 1) {
    filter = argv[first_arg++];
  }

  int rc = run_all_tests(ldml_file, kmx_file, filter);
  if (rc != EXIT_SUCCESS) {
    std::wcerr << console_color::fg(console_color::BRIGHT_RED) << "FAILED" << console_color::reset() << std::endl;
    rc = EXIT_FAILURE;
  }
  return rc;
}
