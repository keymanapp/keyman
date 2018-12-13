/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for kmx integration
  Create Date:  30 Oct 2018
  Authors:      Marc Durdin (MD), Tim Eves (TSE)
*/
#include <algorithm>
#include <cctype>
#include <fstream>
#include <iostream>
#include <list>
#include <sstream>
#include <string>
#include <type_traits>

#include <keyman/keyboardprocessor.h>

#include "state.hpp"

#define   try_status(expr) \
{auto __s = (expr); if (__s != KM_KBP_STATUS_OK) std::exit(100*__LINE__+__s);}

#ifdef assert
#undef assert
#endif
#define assert(expr) {if (!(expr)) std::exit(100*__LINE__); }

std::string utf16_to_utf8(std::u16string utf16_string); // defined in keyboard.cpp

namespace
{

bool g_beep_found = false;

struct key_event {
  km_kbp_virtual_key vk;
  uint16_t modifier_state;
};

typedef enum {
  KOT_INPUT,
  KOT_OUTPUT
} kmx_option_type;

struct kmx_option {
  kmx_option_type type;
  std::u16string key, value;
};

using kmx_options = std::vector<kmx_option>;

int load_source(const std::string &, std::string &, std::u16string &,
                std::u16string &, kmx_options &, bool &);

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

key_event char_to_event(char ch) {
  assert(ch >= 32 && ch <= 127);
  return {
    km::kbp::kmx::s_char_to_vkey[(int)ch - 32].vk,
    (uint16_t)(km::kbp::kmx::s_char_to_vkey[(int)ch - 32].shifted ?  KM_KBP_MODIFIER_SHIFT : 0)
  };
}

uint16_t const get_modifier(std::string const m) {
  for (int i = 0; km::kbp::kmx::s_modifier_names[i].name; i++) {
    if (m == km::kbp::kmx::s_modifier_names[i].name) {
      return km::kbp::kmx::s_modifier_names[i].modifier;
    }
  }
  return 0;
}

km_kbp_virtual_key const get_vk(std::string const & vk) {
  for (int i = 1; i < 256; i++) {
    if (vk == km::kbp::kmx::s_key_names[i]) {
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

void apply_action(km_kbp_state const * state, km_kbp_action_item const & act, std::u16string & text_store) {
  switch (act.type)
  {
  case KM_KBP_IT_END:
    assert(false);
    break;
  case KM_KBP_IT_ALERT:
    g_beep_found = true;
    //std::cout << "beep" << std::endl;
    break;
  case KM_KBP_IT_CHAR:
    if (Uni_IsSMP(act.character)) {
      text_store.push_back(Uni_IsSurrogate1(act.character));
      text_store.push_back(Uni_IsSurrogate2(act.character));
    }
    else {
      text_store.push_back(act.character);
    }
    //std::cout << "char(" << act.character << ") size=" << cp->size() << std::endl;
    break;
  case KM_KBP_IT_MARKER:
    //std::cout << "deadkey(" << act.marker << ")" << std::endl;
    break;
  case KM_KBP_IT_BACK:
    // It is valid for a backspace to be received with an empty text store
    // as the user can press backspace with no text in the store and Keyman
    // will pass that back to the client, as the client may do additional
    // processing at start of a text store, e.g. delete from a previous cell
    // in a table. Or, if Keyman has a cached context, then there may be
    // additional text in the text store that Keyman can't see.
    if (text_store.length() > 0) {
      text_store.pop_back();
    }
    break;
  case KM_KBP_IT_PERSIST_OPT:
    assert(false); // TODO
    break;
  case KM_KBP_IT_INVALIDATE_CONTEXT:
    std::cout << "action: context invalidated (markers cleared)" << std::endl;
    break;
  case KM_KBP_IT_EMIT_KEYSTROKE:
    std::cout << "action: emit keystroke" << std::endl;
    break;
  default:
    assert(false); // NOT SUPPORTED
    break;
  }
}

template<typename P>
std::basic_string<
  typename std::remove_const<
    typename std::remove_pointer<P>::type>::type
>
utf8_to(const std::string &);

template<>
inline
std::basic_string<wchar_t> utf8_to<const wchar_t *>(const std::string & s)
{
  std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> convert;
  return convert.from_bytes(s);
}

template<>
inline
std::basic_string<char> utf8_to<const char *>(const std::string & s)
{
  return s;
}

int run_test(const std::string & source, const std::string & _compiled) {
  std::string keys = "";
  std::u16string expected = u"", context = u"";
  auto compiled = utf8_to<km_kbp_path_name>(_compiled);
  kmx_options options;
  bool expected_beep = false;

  int result = load_source(source, keys, expected, context, options, expected_beep);
  if (result != 0) return result;

  std::cout << "source file   = " << source << std::endl
            << "compiled file = " << _compiled << std::endl;

  km_kbp_keyboard * test_kb = nullptr;
  km_kbp_state * test_state = nullptr;

  try_status(km_kbp_keyboard_load(compiled.c_str(), &test_kb));

  // Setup state, environment

  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));

  // Setup keyboard options

  if (options.size() > 0) {
    km_kbp_option_item *keyboard_opts = new km_kbp_option_item[options.size() + 1];

    int i = 0;
    for (auto it = options.begin(); it != options.end(); it++) {
      if (it->type != KOT_INPUT) continue;

      std::cout << "input option-key: " << utf16_to_utf8(it->key) << std::endl;

      std::u16string key = it->key;
      if (key[0] == u'&') {
        // environment value (aka system store)
        key.erase(0, 1);
        keyboard_opts[i].scope = KM_KBP_OPT_ENVIRONMENT;
      }
      else {
        keyboard_opts[i].scope = KM_KBP_OPT_KEYBOARD;
      }

      km_kbp_cp *cp = new km_kbp_cp[key.length() + 1];
      key.copy(cp, key.length());
      cp[key.length()] = 0;

      keyboard_opts[i].key = cp;

      cp = new km_kbp_cp[it->value.length() + 1];
      it->value.copy(cp, it->value.length());
      cp[it->value.length()] = 0;

      keyboard_opts[i].value = cp;

      i++;
    }

    keyboard_opts[i] = KM_KBP_OPTIONS_END;

    try_status(km_kbp_options_update(test_state, keyboard_opts));

    delete [] keyboard_opts;
  }

  // Setup context
  km_kbp_context_item *citems = nullptr;
  try_status(km_kbp_context_items_from_utf16(context.c_str(), &citems));
  try_status(km_kbp_context_set(km_kbp_state_context(test_state), citems));
  km_kbp_context_items_dispose(citems);

  // Setup baseline text store
  std::u16string text_store = context;

  // Run through key events, applying output for each event
  for (auto p = next_key(keys); p.vk != 0; p = next_key(keys)) {
    try_status(km_kbp_process_event(test_state, p.vk, p.modifier_state));

    for (auto act = km_kbp_state_action_items(test_state, nullptr); act->type != KM_KBP_IT_END; act++) {
      apply_action(test_state, *act, text_store);
    }
  }

  // Test if the beep action was as expected
  if (g_beep_found != expected_beep) return __LINE__;

  // Compare final output - retrieve internal context
  size_t n = 0;
  try_status(km_kbp_context_get(km_kbp_state_context(test_state), &citems));
  try_status(km_kbp_context_items_to_utf16(citems, nullptr, &n));
  km_kbp_cp *buf = new km_kbp_cp[n];
  try_status(km_kbp_context_items_to_utf16(citems, buf, &n));
  km_kbp_context_items_dispose(citems);

  std::cout << "expected: " << utf16_to_utf8(expected) << std::endl;
  std::cout << "text store: " << utf16_to_utf8(text_store) << std::endl;
  std::cout << "result: " << utf16_to_utf8(buf) << std::endl;

  // Compare internal context with expected result
  if (buf != expected) return __LINE__;

  // Compare text store with expected result
  if (text_store != expected) return __LINE__;

  // Test resultant options
  // TODO: test also KM_KBP_IT_PERSIST_OPT and KM_KBP_IT_RESET_OPT actions

  for (auto it = options.begin(); it != options.end(); it++) {
    if (it->type != KOT_OUTPUT) continue;
    std::cout << "output option-key: " << utf16_to_utf8(it->key) << " expected: " << utf16_to_utf8(it->value);
    km_kbp_cp const *value;
    try_status(km_kbp_options_lookup(test_state, KM_KBP_OPT_KEYBOARD, it->key.c_str(), &value));
    std::cout << " actual: " << utf16_to_utf8(value) << std::endl;
    if (it->value.compare(value) != 0) return __LINE__;
    km_kbp_cp_dispose(value);
  }

  // Destroy them
  km_kbp_state_dispose(test_state);
  km_kbp_keyboard_dispose(test_kb);

  return 0;
}

std::u16string parse_source_string(std::string const & s) {
  std::u16string t;
  for (auto p = s.begin(); p != s.end(); p++) {
    if (*p == '\\') {
      p++;
      km_kbp_usv v;
      assert(p != s.end());
      if (*p == 'u' || *p == 'U') {
        // Unicode value
        p++;
        size_t n;
        std::string s1 = s.substr(p - s.begin(), 8);
        v = std::stoul(s1, &n, 16);
        assert(v >= 0x20 && v <= 0x10FFFF);
        p += n-1;
        if (v < 0x10000) {
          t += km_kbp_cp(v);
        }
        else {
          t += km_kbp_cp(Uni_UTF32ToSurrogate1(v));
          t += km_kbp_cp(Uni_UTF32ToSurrogate2(v));
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

bool parse_option_string(std::string line, kmx_options &options, kmx_option_type type) {
  auto x = line.find('=');
  if (x == std::string::npos) return false;

  kmx_option o;

  o.type = type;
  o.key = parse_source_string(line.substr(0, x));
  o.value = parse_source_string(line.substr(x + 1));

  options.emplace_back(o);
  return true;
}

bool is_token(const std::string token, std::string &line) {
  if (line.compare(0, token.length(), token) == 0) {
    line = line.substr(token.length());
    trim(line);
    return true;
  }
  return false;
}

int load_source(const std::string & path, std::string & keys, std::u16string & expected, std::u16string & context, kmx_options &options, bool &expected_beep) {
  const std::string s_keys = "c keys: ",
    s_expected = "c expected: ",
    s_context = "c context: ",
    s_option = "c option: ",
    s_option_expected = "c expected option: ";

  // Parse out the header statements in file.kmn that tell us (a) environment, (b) key sequence, (c) start context, (d) expected result
  std::ifstream kmn(path);
  if (!kmn.good()) {
    return __LINE__;
  }
  std::string line;
  while (std::getline(kmn, line)) {
    trim(line);

    if (!line.length()) continue;
    if (line.compare(0, s_keys.length(), s_keys) == 0) {
      keys = line.substr(s_keys.length());
      trim(keys);
    }
    else if(is_token(s_expected, line)) {
      if (line == "\\b") {
        expected_beep = true;
      }
      else {
        expected = parse_source_string(line);
      }
    }
    else if (is_token(s_context, line)) {
      context = parse_source_string(line);
    }
    else if (is_token(s_option, line)) {
      if (!parse_option_string(line, options, KOT_INPUT)) return __LINE__;
    }
    else if (is_token(s_option_expected, line)) {
      if (!parse_option_string(line, options, KOT_OUTPUT)) return __LINE__;
    }
  }

  if (keys == "") {
    // We must at least have a key sequence to run the test
    return __LINE__;
  }

  return 0;
}

constexpr const auto help_str = "\
kmx <KMN_FILE> <KMX_FILE>\n\
help:\n\
\tKMN_FILE:\tThe source file for the keyboard under test.\n\
\tKMX_FILE:\tThe corresponding compiled kmx file produced from KMN_FILE.\n";
} // namespace

int main(int argc, char *argv[])
{
  if (argc < 3)
  {
    std::cerr << "kmx: Not enough arguments." << std::endl;
    std::cout << help_str;
    return 1;
  }

  return run_test(argv[1], argv[2]);
}
