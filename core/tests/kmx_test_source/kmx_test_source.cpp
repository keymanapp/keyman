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

#include <kmx/kmx_processevent.h>
#include <kmx/kmx_xstring.h>

#include "path.hpp"
#include "state.hpp"
#include "utfcodec.hpp"

#include "kmx_test_source.hpp"

#include <test_assert.h>
#include <test_color.h>

namespace km {
namespace tests {

KmxTestSource::KmxTestSource() {
}

// String trim functions from https://stackoverflow.com/a/217605/1836776
// trim from start (in place)
static inline void
ltrim(std::string &s) {
  s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) { return !std::isspace(ch); }));
}

// trim from end (in place)
static inline void
rtrim(std::string &s) {
  s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) { return !std::isspace(ch); }).base(), s.end());
}

// trim from both ends (in place)
static inline void
trim(std::string &s) {
  ltrim(s);
  rtrim(s);
}

std::u16string
KmxTestSource::parse_source_string(std::string const &s) {
  std::u16string t;
  for (auto p = s.begin(); p != s.end(); p++) {
    if (*p == '\\') {
      p++;
      km_core_usv v;
      test_assert(p != s.end());
      if (*p == 'u' || *p == 'U') {
        // Unicode value
        p++;
        size_t n;
        std::string s1 = s.substr(p - s.begin(), 8);
        v              = std::stoul(s1, &n, 16);
        // Allow deadkey_number (U+0001) characters and onward
        test_assert(v >= 0x0001 && v <= 0x10FFFF);
        p += n - 1;
        if (v < 0x10000) {
          t += km_core_cu(v);
        } else {
          t += km_core_cu(Uni_UTF32ToSurrogate1(v));
          t += km_core_cu(Uni_UTF32ToSurrogate2(v));
        }
      } else if (*p == 'd') {
        // Deadkey
        // TODO, not yet supported
        test_assert(false);
      }
    } else {
      t += *p;
    }
  }
  return t;
}

bool
KmxTestSource::parse_option_string(std::string line, kmx_options &options, kmx_option_type type) {
  auto x = line.find('=');
  if (x == std::string::npos)
    return false;

  kmx_option o;

  o.type  = type;
  o.key   = parse_source_string(line.substr(0, x));
  o.value = parse_source_string(line.substr(x + 1));

  options.emplace_back(o);
  return true;
}

bool
KmxTestSource::is_token(const std::string token, std::string &line) {
  if (line.compare(0, token.length(), token) == 0) {
    line = line.substr(token.length());
    trim(line);
    return true;
  }
  return false;
}

int
KmxTestSource::load_source(
    const km::core::path &path,
    std::string &keys,
    std::u16string &expected,
    std::u16string &expected_context,
    std::u16string &context,
    kmx_options &options,
    bool &expected_beep) {
  const std::string s_keys = "c keys: ";
  const std::string s_expected = "c expected: ";
  const std::string s_expected_context = "c expected context: ";
  const std::string s_context = "c context: ";
  const std::string s_option = "c option: ";
  const std::string s_option_expected = "c expected option: ";
  const std::string s_option_saved = "c saved option: ";
  const std::string s_capsLock = "c capsLock: ";

  // Parse out the header statements in file.kmn that tell us (a) environment, (b) key sequence, (c) start context, (d) expected
  // result
  std::ifstream kmn(path.native());
  if (!kmn.good()) {
    std::cerr << "could not open file: " << path << std::endl;
    return __LINE__;
  }
  std::string line;
  while (std::getline(kmn, line)) {
    trim(line);

    if (!line.length())
      continue;
    if (line.compare(0, s_keys.length(), s_keys) == 0) {
      keys = line.substr(s_keys.length());
      trim(keys);
    } else if (is_token(s_expected, line)) {
      if (line == "\\b") {
        expected_beep = true;
      } else {
        expected = parse_source_string(line);
      }
    } else if (is_token(s_expected_context, line)) {
      expected_context = parse_source_string(line);
    } else if (is_token(s_context, line)) {
      context = parse_source_string(line);
    } else if (is_token(s_option, line)) {
      if (!parse_option_string(line, options, KOT_INPUT))
        return __LINE__;
    } else if (is_token(s_option_expected, line)) {
      if (!parse_option_string(line, options, KOT_OUTPUT))
        return __LINE__;
    } else if (is_token(s_option_saved, line)) {
      if (!parse_option_string(line, options, KOT_SAVED))
        return __LINE__;
    } else if (is_token(s_capsLock, line)) {
      _caps_lock_on = parse_source_string(line).compare(u"1") == 0;
    }
  }

  if (keys == "") {
    // We must at least have a key sequence to run the test
    return __LINE__;
  }

  return 0;
}

km_core_option_item *
KmxTestSource::get_keyboard_options(kmx_options options) {
  km_core_option_item *keyboard_opts = new km_core_option_item[options.size() + 1];

  int i = 0;
  for (auto it = options.begin(); it != options.end(); it++) {
    if (it->type != KOT_INPUT)
      continue;

    std::cout << "input option-key: " << it->key << std::endl;

    std::u16string key = it->key;
    if (key[0] == u'&') {
      // environment value (aka system store)
      key.erase(0, 1);
      keyboard_opts[i].scope = KM_CORE_OPT_ENVIRONMENT;
    } else {
      keyboard_opts[i].scope = KM_CORE_OPT_KEYBOARD;
    }

    km_core_cu *cp = new km_core_cu[key.length() + 1];
    key.copy(cp, key.length());
    cp[key.length()] = 0;

    keyboard_opts[i].key = cp;

    cp = new km_core_cu[it->value.length() + 1];
    it->value.copy(cp, it->value.length());
    cp[it->value.length()] = 0;

    keyboard_opts[i].value = cp;

    i++;
  }

  keyboard_opts[i] = KM_CORE_OPTIONS_END;
  return keyboard_opts;
}

key_event
KmxTestSource::char_to_event(char ch) {
  test_assert(ch >= 32);
  return {
      km::core::kmx::s_char_to_vkey[(int)ch - 32].vk,
      (uint16_t)(km::core::kmx::s_char_to_vkey[(int)ch - 32].shifted ? KM_CORE_MODIFIER_SHIFT : 0)};
}

uint16_t
KmxTestSource::get_modifier(std::string const m) {
  for (int i = 0; km::core::kmx::s_modifier_names[i].name; i++) {
    if (m == km::core::kmx::s_modifier_names[i].name) {
      return km::core::kmx::s_modifier_names[i].modifier;
    }
  }
  return 0;
}

km_core_virtual_key
KmxTestSource::get_vk(std::string const &vk) {
  for (int i = 1; i < 256; i++) {
    if (vk == km::core::kmx::s_key_names[i]) {
      return i;
    }
  }
  return 0;
}

key_event
KmxTestSource::vkey_to_event(std::string const &vk_event) {
  // vkey format is MODIFIER MODIFIER K_NAME
  // std::cout << "VK=" << vk_event << std::endl;

  std::stringstream f(vk_event);
  std::string s;
  uint16_t modifier_state = 0;
  km_core_virtual_key vk   = 0;
  while (std::getline(f, s, ' ')) {
    uint16_t modifier = get_modifier(s);
    if (modifier != 0) {
      modifier_state |= modifier;
    } else {
      vk = get_vk(s);
      break;
    }
  }

  // The string should be empty at this point
  test_assert(!std::getline(f, s, ' '));
  test_assert(vk != 0);

  return {vk, modifier_state};
}

key_event
KmxTestSource::next_key(std::string &keys) {
  // Parse the next element of the string, chop it off, and return it
  if (keys.length() == 0)
    return {0, 0};
  char ch = keys[0];
  if (ch == '[') {
    if (keys.length() > 1 && keys[1] == '[') {
      keys.erase(0, 2);
      return char_to_event(ch);
    }
    auto n = keys.find(']');
    test_assert(n != std::string::npos);
    auto vkey = keys.substr(1, n - 1);
    keys.erase(0, n + 1);
    return vkey_to_event(vkey);
  } else {
    keys.erase(0, 1);
    return char_to_event(ch);
  }
}

}  // namespace tests
}  // namespace km
