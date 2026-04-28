/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Created by Tim Eves on 2018-10-30
 *
 * Keyman Core - Tests for kmx processor
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

#include "keyman_core.h"

#include <kmx/kmx_processevent.h>
#include <kmx/kmx_xstring.h>

#include "path.hpp"
#include "state.hpp"
#include "utfcodec.hpp"

#include "../helpers/core_test_helpers.h"

#include "kmx_test_source.hpp"

// TODO-WEB-CORE: move into a single generated header file of all test keyboards
km::core::path source, compiled;
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
apply_action(
    km_core_state const *,
    km_core_action_item const &act,
    std::u16string &text_store,
    std::vector<km_core_context_item> &context,
    km::tests::kmx_options &options,
    km::tests::KmxTestSource &test_source,
    bool &context_invalidated
    ) {
  switch (act.type) {
  case KM_CORE_IT_END:
    FAIL();
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
      ASSERT_FALSE(context.empty());
      ASSERT_EQ(context.back().type, KM_CORE_CT_MARKER);
      context.pop_back();
    } else if (text_store.length() > 0) {
      ASSERT_FALSE(context.empty());
      ASSERT_FALSE(text_store.empty());
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
      ASSERT_EQ(ch, act.backspace.expected_value);

      ASSERT_EQ(context.back().type, KM_CORE_CT_CHAR);
      ASSERT_EQ(context.back().character, ch);
      context.pop_back();
    }
    break;
  case KM_CORE_IT_PERSIST_OPT: {
    bool found = false;
    for (auto it = options.begin(); it != options.end(); it++) {
      if (it->type == km::tests::KOT_SAVED) {
        if (it->key.compare(act.option->key) == 0) {
          found           = true;
          it->saved_value = act.option->value;
          break;
        }
      }
    }
    std::cout << "action: option " << (act.option->scope == KM_CORE_OPT_ENVIRONMENT ? "environment " : "keyboard ")
              << act.option->key << "=" << act.option->value << " persistence requested" << std::endl;
    if (!found) {
      std::cout << "option " << act.option->key
                << " saved but no expected output found. Suggestion: update test to include saved option value." << std::endl;
    }
  } break;
  case KM_CORE_IT_INVALIDATE_CONTEXT:
    context.clear();
    context_invalidated = true;
    std::cout << "action: context invalidated (markers cleared)" << std::endl;
    break;
  case KM_CORE_IT_EMIT_KEYSTROKE:
    std::cout << "action: emit keystroke" << std::endl;
    break;
  case KM_CORE_IT_CAPSLOCK:
    std::cout << "action: capsLock " << std::to_string(act.capsLock) << std::endl;
    test_source.set_caps_lock_on(act.capsLock);
    break;
  default:
    FAIL();  // NOT SUPPORTED
    break;
  }
}

TEST(KmxTests, TestKmxFile) {
  std::string keys = "";
  std::u16string expected = u"", expected_context = u"NOT SUPPLIED", context = u"";
  km::tests::kmx_options options;
  bool expected_beep = false;
  km::tests::KmxTestSource test_source;

  ASSERT_EQ(test_source.load_source(source, keys, expected, expected_context, context, options, expected_beep), 0);

  // If an expect_context is not provided, default to expected == expect_context
  std::cout << "expected_context  = " << string_to_hex(expected_context) <<  " [" << expected_context << "]" << std::endl;
  if (expected_context == u"NOT SUPPLIED") {
    expected_context = expected;
  }

  std::cout << "source file   = " << source << std::endl
            << "compiled file = " << compiled << std::endl;

  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr;

  auto blob = km::tests::load_kmx_file(compiled.native().c_str());
  ASSERT_STATUS_OK(km_core_keyboard_load_from_blob(compiled.stem().c_str(), blob.data(), blob.size(), &test_kb));

  // Setup state, environment
  ASSERT_STATUS_OK(km_core_state_create(test_kb, test_empty_env_opts, &test_state));

  // Setup keyboard options
  if (options.size() > 0) {
    km_core_option_item *keyboard_opts = test_source.get_keyboard_options(options);

    ASSERT_STATUS_OK(km_core_state_options_update(test_state, keyboard_opts));

    delete [] keyboard_opts;
  }

  // Setup context
  km_core_context_item *citems = nullptr;
  ASSERT_STATUS_OK(context_items_from_utf16(context.c_str(), &citems));
  ASSERT_STATUS_OK(km_core_context_set(km_core_state_context(test_state), citems));

  // Make a copy of the setup context for the test
  std::vector<km_core_context_item> test_context;
  for(km_core_context_item *ci = citems; ci->type != KM_CORE_CT_END; ci++) {
    test_context.emplace_back(*ci);
  }

  km_core_context_items_dispose(citems);

  // Setup baseline text store
  std::u16string text_store = context;
  // used to stop comparing text_store to context if this flag is set
  bool context_invalidated = false;

  // Run through key events, applying output for each event
  for (auto p = test_source.next_key(keys); p.vk != 0; p = test_source.next_key(keys)) {
    // Because a normal system tracks caps lock state itself,
    // we mimic that in the tests. We assume caps lock state is
    // updated on key_down before the processor receives the
    // event.
    if (p.vk == KM_CORE_VKEY_CAPS) {
      test_source.toggle_caps_lock_state();
    }

    for (auto key_down = 1; key_down >= 0; key_down--) {
      ASSERT_STATUS_OK(km_core_process_event(test_state, p.vk, p.modifier_state | test_source.caps_lock_state(), key_down, KM_CORE_EVENT_FLAG_DEFAULT));

      for (auto act = km_core_state_action_items(test_state, nullptr); act->type != KM_CORE_IT_END; act++) {
        ASSERT_NO_FATAL_FAILURE(apply_action(test_state, *act, text_store, test_context, options, test_source, context_invalidated));
      }
    }

    // Compare context and text store at each step
    // should be identical unless an action has caused the context to be invalidated
    size_t n = 0;
    ASSERT_STATUS_OK(km_core_context_get(km_core_state_context(test_state), &citems));
    ASSERT_STATUS_OK(context_items_to_utf16(citems, nullptr, &n));
    km_core_cu *core_context_str = new km_core_cu[n];
    ASSERT_STATUS_OK(context_items_to_utf16(citems, core_context_str, &n));

    // Verify that both our local test_context and the core's test_state.context have
    // not diverged
    auto ci = citems;
    for(auto test_ci = test_context.begin(); ci->type != KM_CORE_CT_END || test_ci != test_context.end(); ci++, test_ci++) {
      // Verify that both lists are same length
      ASSERT_NE(ci->type, KM_CORE_CT_END);
      ASSERT_NE(test_ci, test_context.end());
      // Compare items
      ASSERT_EQ(test_ci->type, ci->type);
      ASSERT_EQ(test_ci->marker, ci->marker);
    }
    km_core_context_items_dispose(citems);
    if ((!context_invalidated) && (text_store != core_context_str)) {
      std::cerr << "text store has unexpectedly diverged from core_context" << std::endl;
      std::cerr << "text store  : " << string_to_hex(text_store) << " [" << text_store << "]" << std::endl;
      std::cerr << "core context: " << string_to_hex(core_context_str) << " [" << core_context_str << "]" << std::endl;
      FAIL();
    }
  }

  // Test if the beep action was as expected
  ASSERT_EQ(g_beep_found, expected_beep);

  // Compare final output - retrieve internal context
  size_t n = 0;
  ASSERT_STATUS_OK(km_core_context_get(km_core_state_context(test_state), &citems));
  ASSERT_STATUS_OK(context_items_to_utf16(citems, nullptr, &n));
  km_core_cu *core_context_str = new km_core_cu[n];
  ASSERT_STATUS_OK(context_items_to_utf16(citems, core_context_str, &n));

  // Verify that both our local test_context and the core's test_state.context have
  // not diverged
  auto ci = citems;
  for(auto test_ci = test_context.begin(); ci->type != KM_CORE_CT_END || test_ci != test_context.end(); ci++, test_ci++) {
    // Verify that both lists are same length
    ASSERT_NE(ci->type, KM_CORE_CT_END);
    ASSERT_NE(test_ci, test_context.end());
    // Compare items
    ASSERT_EQ(test_ci->type, ci->type);
    ASSERT_EQ(test_ci->marker, ci->marker);
  }

  km_core_context_items_dispose(citems);

  std::cout << "expected        : " << string_to_hex(expected) << " [" << expected << "]" << std::endl;
  std::cout << "text store      : " << string_to_hex(text_store) << " [" << text_store << "]" << std::endl;
  std::cout << "expected context: " << string_to_hex(expected_context) << " [" << expected_context << "]" << std::endl;
  std::cout << "context         : " << string_to_hex(core_context_str) << " [" << core_context_str << "]" << std::endl;

  // Compare internal context with expected context result
  ASSERT_EQ(core_context_str, expected_context);

  // Compare text store with expected result
  ASSERT_EQ(text_store, expected);

  // Test resultant options
  for (auto it = options.begin(); it != options.end(); it++) {
    if (it->type == km::tests::KOT_OUTPUT) {
      std::cout << "output option-key: " << it->key << " expected: " << it->value;
      km_core_cu const *value;
      ASSERT_STATUS_OK(km_core_state_option_lookup(test_state, KM_CORE_OPT_KEYBOARD, it->key.c_str(), &value));
      std::cout << " actual: " << value << std::endl;
      ASSERT_EQ(it->value.compare(value), 0);
    } else if (it->type == km::tests::KOT_SAVED) {
      std::cout << "persisted option-key: " << it->key << " expected: " << it->value << " actual: " << it->saved_value << std::endl;
      ASSERT_EQ(it->value.compare(it->saved_value), 0);
    }
  }

  // Destroy them
  km_core_state_dispose(test_state);
  km_core_keyboard_dispose(test_kb);
}


// provide our own `main` so that we can get the path of the exe so that
// we have a well-defined location to find our test keyboards
GTEST_API_ int
main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);

  if(argc < 3) {
    std::cout << "Parameters required: <keyboard.kmn> <keyboard.kmx>\n";
    return 1;
  }

#ifdef __EMSCRIPTEN__
  if(!get_wasm_file_path(km::core::path(argv[1]), source) ||
      !get_wasm_file_path(km::core::path(argv[2]), compiled)) {
    // not a fully qualified path
    return 1;
  }
#else
  source = km::core::path(argv[1]);
  compiled = km::core::path(argv[2]);
#endif

  km::core::kmx::g_debug_ToConsole = TRUE;

  return RUN_ALL_TESTS();
}
