/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for kmx integration
  Create Date:  30 Oct 2018
  Authors:      Marc Durdin (MD), Tim Eves (TSE)
*/
#include <string>
#include <fstream>
#include <sstream>
#include <cctype>
#include <algorithm>
#include <iostream>
#include <keyman/keyboardprocessor.h>
#include "state.hpp"

#define   try_status(expr) \
{auto __s = (expr); if (__s != KM_KBP_STATUS_OK) std::exit(100*__LINE__+__s);}

#ifdef assert
#undef assert
#endif
#define assert(expr) {if (!(expr)) std::exit(100*__LINE__); }

std::string utf16_to_utf8(std::u16string utf16_string);

namespace
{

const std::string base = "tests/unit/kmx/";

int run_test(const std::string & file);
int load_source(const std::string & file, std::string & keys, std::u16string & expected, std::u16string & context);

struct key_event {
  km_kbp_virtual_key vk;
  uint16_t modifier_state;
};

km_kbp_option_item test_env_opts[] =
{
  KM_KBP_OPTIONS_END
};

// String trim functions from https://stackoverflow.com/a/217605/1836776
// trim from start (in place)
static inline void ltrim(std::string &s) {
  s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
    return !std::isspace(ch);
  }));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
  s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
    return !std::isspace(ch);
  }).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
  ltrim(s);
  rtrim(s);
}

// trim from start (copying)
static inline std::string ltrim_copy(std::string s) {
  ltrim(s);
  return s;
}

// trim from end (copying)
static inline std::string rtrim_copy(std::string s) {
  rtrim(s);
  return s;
}

// trim from both ends (copying)
static inline std::string trim_copy(std::string s) {
  trim(s);
  return s;
}

key_event char_to_event(char ch) {
  assert(ch >= 32 && ch < 128);
  return {
    s_char_to_vkey[(int)ch - 32].vk,
    (uint16_t)(s_char_to_vkey[(int)ch - 32].shifted ?  KM_KBP_MODIFIER_SHIFT : 0)
  };
}

uint16_t const get_modifier(std::string const m) {
  for (int i = 0; s_modifier_names[i].name; i++) {
    if (m == s_modifier_names[i].name) {
      return s_modifier_names[i].modifier;
    }
  }
  return 0;
}

km_kbp_virtual_key const get_vk(std::string const & vk) {
  for (int i = 1; i < 256; i++) {
    if (vk == s_key_names[i]) {
      return i;
    }
  }
  return 0;
}

key_event const vkey_to_event(std::string const & vk_event) {
  // vkey format is MODIFIER MODIFIER K_NAME
  //std::cout << "VK=" << vk_event << std::endl;

  std::stringstream f(vk_event);
  std::string s;
  uint16_t modifier_state = 0;
  km_kbp_virtual_key vk = 0;
  while(std::getline(f, s, ' ')) {
    uint16_t modifier = get_modifier(s);
    if (modifier != 0) {
      modifier_state |= modifier;
    }
    else {
      vk = get_vk(s);
      break;
    }
  }

  // The string should be empty at this point
  assert(!std::getline(f, s, ' '));
  assert(vk != 0);

  return {
    vk,
    modifier_state
  };
}

key_event next_key(std::string &keys) {
  // Parse the next element of the string, chop it off, and return it
  if (keys.length() == 0) return { 0 };
  char ch = keys[0];
  if (ch == '[') {
    if (keys.length() > 1 && keys[1] == '[') {
      keys.erase(0, 2);
      return char_to_event(ch);
    }
    auto n = keys.find(']');
    assert(n != std::string::npos);
    auto vkey = keys.substr(1, n - 1);
    keys.erase(0, n+1);
    return vkey_to_event(vkey);
  }
  else {
    keys.erase(0, 1);
    return char_to_event(ch);
  }
}

void apply_action(km_kbp_state const * state, km_kbp_action_item const & act) {
  switch (act.type)
  {
  case KM_KBP_IT_END:
    assert(false);
    break;
  case KM_KBP_IT_ALERT:
    //std::cout << "beep" << std::endl;
    break;
  case KM_KBP_IT_CHAR:
    //std::cout << "char(" << act.character << ") size=" << cp->size() << std::endl;
    break;
  case KM_KBP_IT_MARKER:
    //std::cout << "deadkey(" << act.marker << ")" << std::endl;
    break;
  case KM_KBP_IT_BACK:
    break;
  case KM_KBP_IT_PERSIST_OPT:
  case KM_KBP_IT_RESET_OPT:
    assert(false); // TODO
    break;
  case KM_KBP_IT_VKEYDOWN:
  case KM_KBP_IT_VKEYUP:
  case KM_KBP_IT_VSHIFTDOWN:
  case KM_KBP_IT_VSHIFTUP:
    assert(false); // NOT SUPPORTED
    break;
  default:
    assert(false); // NOT SUPPORTED
    break;
  }
}

int run_test(const std::string & file) {
  std::string keys = "";
  std::u16string expected = u"", context = u"";
  
  int result = load_source(file, keys, expected, context);
  if (result != 0) return result;

  std::cout << "file = " << file << std::endl;
  //std::cout << "keys = " << keys << std::endl;
  //std::cout << "expected = " << utf16_to_utf8(expected) << std::endl;
  //std::cout << "context = " << utf16_to_utf8(context) << std::endl;

  // TODO: compile the keyboard using kmcomp
  km_kbp_keyboard * test_kb = nullptr;
  km_kbp_state * test_state = nullptr;
    
  try_status(km_kbp_keyboard_load(std::filesystem::path(base + file + ".kmx").c_str(), &test_kb));

  // Setup state, environment, options
  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));

  // Setup context
  km_kbp_context_item *citems = nullptr;
  try_status(km_kbp_context_items_from_utf16(context.c_str(), &citems));
  try_status(km_kbp_context_set(km_kbp_state_context(test_state), citems));
  km_kbp_context_items_dispose(citems);

  // Run through key events, applying output for each event
  for (auto p = next_key(keys); p.vk != 0; p = next_key(keys)) {
    try_status(km_kbp_process_event(test_state, p.vk, p.modifier_state));
    for (auto act = km_kbp_state_action_items(test_state, nullptr); act->type != KM_KBP_IT_END; act++) {
      apply_action(test_state, *act);
    }
  }

  // Compare final output { TODO: don't use the inbuilt context, instead use the actions, starting with context }
  size_t n = 0;
  try_status(km_kbp_context_get(km_kbp_state_context(test_state), &citems));
  try_status(km_kbp_context_items_to_utf16(citems, nullptr, &n));
  km_kbp_cp *buf = new km_kbp_cp[n];
  try_status(km_kbp_context_items_to_utf16(citems, buf, &n));
  km_kbp_context_items_dispose(citems);
  
  //std::cout << "result = " << utf16_to_utf8(buf) << std::endl;

  // Destroy them
  km_kbp_state_dispose(test_state);
  km_kbp_keyboard_dispose(test_kb);

  return (buf == expected) ? 0 : __LINE__;
}

std::u16string parse_source_string(std::string const & s) {
  std::u16string t;
  for (auto p = s.begin(); p != s.end(); p++) {
    if (*p == '\\') {
      p++;
      km_kbp_usv v;
      assert(p != s.end());
      if (*p == 'u') {
        // Unicode value
        p++;
        size_t n;
        std::string s1 = s.substr(p - s.begin(), 6);
        v = std::stoul(s1, &n, 16);
        assert(v >= 0x20 && v <= 0x10FFFF);
        p += n-1;
        if (v < 0x10000) {
          t += km_kbp_cp(v);
        }
        else {
          t += km_kbp_cp(Uni_UTF32ToSurrogate1(v)) + km_kbp_cp(Uni_UTF32ToSurrogate2(v));
        }
      }
      else if (*p == 'd') {
        // Deadkey
        // TODO, not yet supported
        assert(false);
      }
    }
    else {
      t += *p;
    }
  }
  return t;
}

int load_source(const std::string & file, std::string & keys, std::u16string & expected, std::u16string & context) {
  const std::string s_keys = "c keys: ",
                    s_expected = "c expected: ",
                    s_context = "c context: ";

  //std::cout << "load_source " << base + file + ".kmn" << std::endl;

  // Parse out the header statements in file.kmn that tell us (a) environment, (b) key sequence, (c) start context, (d) expected result
  std::ifstream kmn(base + file + ".kmn");
  std::string line;
  while (std::getline(kmn, line)) {
    trim(line);

    if (!line.length()) continue;
    if (line.compare(0, s_keys.length(), s_keys) == 0) {
      keys = line.substr(s_keys.length());
      trim(keys);
    }
    else if (line.compare(0, s_expected.length(), s_expected) == 0) {
      line = line.substr(s_expected.length());
      trim(line);
      expected = parse_source_string(line);
    }
    else if (line.compare(0, s_context.length(), s_context) == 0) {
      line = line.substr(s_context.length());
      trim(line);
      context = parse_source_string(line);
    }
  }

  if (keys == "") {
    // We must at least have a key sequence to run the test
    return __LINE__;
  }

  return 0;
}

} // namespace

int main(int, char *[])
{
  int result;
  if ((result = run_test("000 - null keyboard")) != 0) return result;
  if ((result = run_test("001 - basic input UnicodeI")) != 0) return result;
  if ((result = run_test("002 - basic input Unicode")) != 0) return result;
  if ((result = run_test("004 - basic input (shift 2)")) != 0) return result;
  if ((result = run_test("006 - vkey input (shift ctrl)")) != 0) return result;
  if ((result = run_test("007 - vkey input (ctrl alt)")) != 0) return result;
  if ((result = run_test("008 - vkey input (ctrl alt 2)")) != 0) return result;
  if ((result = run_test("012 - ralt")) != 0) return result;
  if ((result = run_test("013 - deadkeys")) != 0) return result;
  if ((result = run_test("014 - groups and virtual keys")) != 0) return result;
  if ((result = run_test("015 - ralt 2")) != 0) return result;
  if ((result = run_test("017 - space mnemonic kbd")) != 0) return result;
  if ((result = run_test("018 - nul testing")) != 0) return result;
  if ((result = run_test("019 - multiple deadkeys")) != 0) return result;
  if ((result = run_test("020 - deadkeys and backspace")) != 0) return result;
  return result;
}


