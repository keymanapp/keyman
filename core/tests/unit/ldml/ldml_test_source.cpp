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

#if 0
// TODO-LDML If we need to avoid exceptions in JSON
#define JSON_TRY_USER if(true)
#define JSON_CATCH_USER(exception) if(false)
#define JSON_THROW_USER(exception) { return __LINE__; } // get out
#endif

#include <json.hpp>

#include <kmx/kmx_processevent.h> // for char to vk mapping tables
#include <kmx/kmx_xstring.h> // for surrogate pair macros
#include <kmx/kmx_plus.h>
#include "ldml/keyman_core_ldml.h"
#include "ldml/ldml_processor.hpp"
#include "ldml/ldml_markers.hpp"

#include "path.hpp"
#include "state.hpp"
#include "utfcodec.hpp"

#include "ldml_test_source.hpp"
#include "ldml_test_utils.hpp"

#include "core_icu.h"
#include "unicode/uniset.h"
#include "unicode/usetiter.h"

#include <test_color.h>

#define assert_or_return(expr) if(!(expr)) { \
  std::wcerr << __FILE__ << ":" << __LINE__ << ": " << \
  console_color::fg(console_color::BRIGHT_RED) \
             << "warning: " << (#expr) \
             << console_color::reset() \
             << std::endl; \
  return __LINE__; \
}

#define TEST_JSON_SUFFIX "-test.json"
namespace km {
namespace tests {




/** string munging */
static void append_to_str(std::u16string &str, const char *buf) {
  const PKMX_WCHAR p = km::core::kmx::strtowstr((char *)buf); /** cast away const, unused*/
  const std::u16string p2(p);
  str.append(p2);
  delete [] p;
}

/** string munging */
static void append_to_str(std::u16string &str, long n) {
  char buf[64];

  snprintf(buf, 64, "%ld", n);
  append_to_str(str, buf);
}

void
ldml_action::formatType(const char *f, int l, ldml_action_type setType, const std::u16string &msg) {
  type = setType;
  string.clear();
  append_to_str(string, f);
  string.append(u":");
  append_to_str(string, l);
  string.append(u" ");
  string.append(msg);
}

void
ldml_action::formatType(const char *f, int l, ldml_action_type setType, const std::u16string &msg, const std::u16string &msg2) {
  std::u16string tmp = msg;
  tmp.append(msg2);
  formatType(f, l, setType, tmp);
}

void
ldml_action::formatType(const char *f, int l, ldml_action_type setType, const std::u16string &msg, long msg2) {
  std::u16string tmp;
  append_to_str(tmp, msg2);
  formatType(f, l, setType, msg, tmp);
}

void
ldml_action::formatType(const char *f, int l, ldml_action_type setType, const std::u16string &msg, const std::string &msg2) {
  std::u16string tmp;
  append_to_str(tmp, msg2.c_str());
  formatType(f, l, setType, msg, tmp);
}

bool ldml_action::done() const {
  return (type == LDML_ACTION_DONE || type == LDML_ACTION_SKIP || type == LDML_ACTION_FAIL);
}

LdmlTestSource::LdmlTestSource() {
}


LdmlTestSource::~LdmlTestSource() {

}

km_core_status LdmlTestSource::get_expected_load_status() {
  return KM_CORE_STATUS_OK;
}

bool LdmlTestSource::get_expected_beep() const {
  return false;
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

LdmlEmbeddedTestSource::LdmlEmbeddedTestSource() {

}

LdmlEmbeddedTestSource::~LdmlEmbeddedTestSource() {

}

std::u16string
LdmlTestSource::parse_source_string(std::string const &s) {
  std::u16string t;
  for (auto p = s.begin(); p != s.end(); p++) {
    if (*p == '\\') {
      p++;
      km_core_usv v;
      bool had_open_curly = false;
      assert(p != s.end());
      if (*p == 'u' || *p == 'U') {
        // Unicode value
        p++;
        if (*p == '{') {
          p++;
          assert(p != s.end());
          had_open_curly = true;
        }
        size_t n;
        std::string s1 = s.substr(p - s.begin(), 8);
        v              = std::stoul(s1, &n, 16);
        // Allow deadkey_number (U+0001) characters and onward
        assert(v >= 0x0001 && v <= 0x10FFFF);
        p += n - 1;
        if (v < 0x10000) {
          t += km_core_cu(v);
        } else {
          t += km_core_cu(Uni_UTF32ToSurrogate1(v));
          t += km_core_cu(Uni_UTF32ToSurrogate2(v));
        }
        if (had_open_curly) {
          p++;
          // close what you opened
          assert(*p == '}'); // close curly
          assert(p != s.end());
        }
      } else if (*p == 'd') {
        // Deadkey
        // TODO, not yet supported
        assert(false);
      }
    } else {
      t += *p;
    }
  }
  return t;
}

std::u16string
LdmlTestSource::parse_u8_source_string(std::string const &u8s) {
  // convert from utf-8 to utf-16 first
  std::u16string s = convert<char, char16_t>(u8s);
  std::u16string t;
  for (auto p = s.begin(); p != s.end(); p++) {
    if (*p == '\\') {
      p++;
      km_core_usv v;
      bool had_open_curly = false;
      assert(p != s.end());
      if (*p == 'u' || *p == 'U') {
        // Unicode value
        p++;
        if (*p == '{') {
          p++;
          assert(p != s.end());
          had_open_curly = true;
        }
        size_t n;
        std::u16string s1 = s.substr(p - s.begin(), 8);
        // TODO-LDML: convert back first?
        std::string s1b = convert<char16_t, char>(s1);
        v              = std::stoul(s1b, &n, 16);
        // Allow deadkey_number (U+0001) characters and onward
        assert(v >= 0x0001 && v <= 0x10FFFF);
        p += n - 1;
        if (v < 0x10000) {
          t += km_core_cu(v);
        } else {
          t += km_core_cu(Uni_UTF32ToSurrogate1(v));
          t += km_core_cu(Uni_UTF32ToSurrogate2(v));
        }
        if (had_open_curly) {
          p++;
          // close what you opened
          assert(*p == '}'); // close curly
          assert(p != s.end());
        }
      } else if (*p == 'd') {
        // Deadkey
        // TODO, not yet supported
        assert(false);
      }
    } else {
      t += *p;
    }
  }
  return t;
}

bool
LdmlEmbeddedTestSource::is_token(const std::string token, std::string &line) {
  if (line.compare(0, token.length(), token) == 0) {
    line = line.substr(token.length());
    trim(line);
    return true;
  }
  return false;
}

int
LdmlEmbeddedTestSource::load_source( const km::core::path &path ) {
  const std::string s_keys = "@@keys: ";
  const std::string s_expected = "@@expected: ";
  const std::string s_context = "@@context: ";
  const std::string s_capsLock = "@@capsLock: ";
  const std::string s_expecterror = "@@expect-error: ";

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
      auto k = line.substr(s_keys.length());
      trim(k);
      keys.emplace_back(k);
    } else if (is_token(s_expected, line)) {
      if (line == "\\b") {
        expected_beep = true;
      } else {
        // allow multiple expected lines
        expected.emplace_back(parse_source_string(line));
      }
    } else if (is_token(s_expecterror, line)) {
      expected_error = true;
    } else if (is_token(s_context, line)) {
      context = parse_source_string(line);
    } else if (is_token(s_capsLock, line)) {
      set_caps_lock_on(parse_source_string(line).compare(u"1") == 0);
    }
  }

  if (keys.empty() && expected.empty() && !expected_error) {
    // don't note this, the parent will complain if there's neither json nor embedded
    return __LINE__;
  } else if (keys.empty()) {
    // We must at least have a key sequence to run the test
    std::cerr << "Need at least one key sequence." << std::endl;
    return __LINE__;
  } else if(!expected_error && (keys.size() != expected.size())) {
    std::cerr << "Need the same number of " << s_keys << " and " << s_expected << " lines." << std::endl;
    return __LINE__;
  }

  return 0;
}

km_core_status
LdmlEmbeddedTestSource::get_expected_load_status() {
  return expected_error ? KM_CORE_STATUS_INVALID_KEYBOARD : KM_CORE_STATUS_OK;
}

const std::u16string&
LdmlEmbeddedTestSource::get_context() {
  return context;
}

bool LdmlEmbeddedTestSource::get_expected_beep() const {
  return expected_beep;
}

int
LdmlTestSource::caps_lock_state() {
  return _caps_lock_on ? KM_CORE_MODIFIER_CAPS : 0;
}

void
LdmlTestSource::toggle_caps_lock_state() {
  _caps_lock_on = !_caps_lock_on;
}

void
LdmlTestSource::set_caps_lock_on(bool caps_lock_on) {
  _caps_lock_on = caps_lock_on;
}

key_event
LdmlTestSource::char_to_event(char ch) {
  assert(ch >= 32);
  return {
      km::core::kmx::s_char_to_vkey[(int)ch - 32].vk,
      (uint16_t)(km::core::kmx::s_char_to_vkey[(int)ch - 32].shifted ? KM_CORE_MODIFIER_SHIFT : 0)};
}

uint16_t
LdmlTestSource::get_modifier(std::string const m) {
  for (int i = 0; km::core::kmx::s_modifier_names[i].name; i++) {
    if (m == km::core::kmx::s_modifier_names[i].name) {
      return km::core::kmx::s_modifier_names[i].modifier;
    }
  }
  return 0;
}

key_event
LdmlEmbeddedTestSource::vkey_to_event(std::string const &vk_event) {
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
      if (vk == 0) {
        std::cerr << "Error parsing [" << vk_event << "] - could not find vkey or modifier: " << s << std::endl;
      }
      assert(vk != 0);
      break; // only one vkey allowed
    }
  }

  // The string should be empty at this point
  if (std::getline(f, s, ' ')) {
    std::cerr << "Error parsing vkey ["<<vk_event<<"] - excess string after key: " << s << std::endl;
    assert(false);
  }
  assert(vk != 0);

  return {vk, modifier_state};
}

void
LdmlEmbeddedTestSource::next_action(ldml_action &fillin) {
  if (is_done || keys.empty()) {
    // We were already done. return done.
    fillin.type = LDML_ACTION_DONE;
    return;
  } else if(keys[0].empty()) {
    // Got to the end of a key set. time to check
    fillin.type = LDML_ACTION_CHECK_EXPECTED;
    fillin.string = expected[0]; // copy expected
    expected.pop_front();
    keys.pop_front();
    if (keys.empty()) {
      is_done = true; // so we get DONE next time
    }
  } else {
    fillin.type = LDML_ACTION_KEY_EVENT;
    fillin.k = next_key();
  }
}


key_event
LdmlEmbeddedTestSource::next_key() {
  // mutate this->keys
  return next_key(keys[0]);
}

key_event
LdmlEmbeddedTestSource::next_key(std::string &keys) {
  // Parse the next element of the string, chop it off, and return it
  // mutates keys
  if (keys.length() == 0)
    return {0, 0};
  char ch = keys[0];
  if (ch == '[') {
    if (keys.length() > 1 && keys[1] == '[') {
      keys.erase(0, 2);
      return char_to_event(ch);
    }
    auto n = keys.find(']');
    assert(n != std::string::npos);
    auto vkey = keys.substr(1, n - 1);
    keys.erase(0, n + 1);
    return vkey_to_event(vkey);
  } else {
    keys.erase(0, 1);
    return char_to_event(ch);
  }
}


class LdmlJsonTestSource : public LdmlTestSource {
public:
  LdmlJsonTestSource(const std::string &path, km::core::kmx::kmx_plus *kmxplus);
  virtual ~LdmlJsonTestSource();
  virtual const std::u16string &get_context();
  int load(const nlohmann::json &test);
  virtual void next_action(ldml_action &fillin);
private:
  std::string path;
  nlohmann::json data;  // maybe
  std::u16string context;
  std::u16string expected;
  /**
   * Which action are we on?
  */
  std::size_t action_index = -1;
  const km::core::kmx::kmx_plus *kmxplus;
  /** @return false if not found */
  bool set_key_from_id(key_event& k, const std::u16string& id);
  bool loaded_context = false;
};

LdmlJsonTestSource::LdmlJsonTestSource(const std::string &path, km::core::kmx::kmx_plus *k)
:path(path), kmxplus(k) {

}

LdmlJsonTestSource::~LdmlJsonTestSource() {
}

bool LdmlJsonTestSource::set_key_from_id(key_event& k, const std::u16string& id) {
  k = {0, 0};  // set to a null value at first.

  assert(kmxplus != nullptr);
  // lookup the id
  assert(kmxplus->key2 != nullptr);

  assert(kmxplus->key2Helper.valid());
  // First, find the string
  KMX_DWORD strId = kmxplus->strs->find(id);
  if (strId == 0) {
    return false;
  }

  // OK. Now we can search the keybag
  KMX_DWORD keyIndex = 0; // initialize loop
  auto *key2 = kmxplus->key2Helper.findKeyByStringId(strId, keyIndex);
  if (key2 == nullptr) {
    return false;
  }

  // Now, look for the _first_ candidate vkey match in the kmap.
  for (KMX_DWORD kmapIndex = 0; kmapIndex < kmxplus->key2->kmapCount; kmapIndex++) {
    auto *kmap = kmxplus->key2Helper.getKmap(kmapIndex);
    assert(kmap != nullptr);
    if (kmap->key == keyIndex) {
      k = {(km_core_virtual_key)kmap->vkey, (uint16_t)kmap->mod};
      return true;
    }
  }
  // Else, unfound
  return false;
}


void
LdmlJsonTestSource::next_action(ldml_action &fillin) {
  if ((action_index+1) >= data["/actions"_json_pointer].size()) {
    // at end, done
    fillin.type = LDML_ACTION_DONE;
    return;
  }

  action_index++;
  auto action   = data["/actions"_json_pointer].at(action_index);
  // load up several common attributes
  auto type     = action["/type"_json_pointer];
  auto result   = action["/result"_json_pointer];
  auto key      = action["/key"_json_pointer];
  auto to       = action["/to"_json_pointer];

  // is it a check event?
  if (type == "check") {
    fillin.type   = LDML_ACTION_CHECK_EXPECTED;
    fillin.string = LdmlTestSource::parse_u8_source_string(result.get<std::string>());
    if (!get_normalization_disabled()) {
      assert(km::core::ldml::normalize_nfd(fillin.string)); // TODO-LDML: will be NFC when core is normalizing to NFC
    }
    return;
  } else if (type == "keystroke") {
    fillin.type   = LDML_ACTION_KEY_EVENT;
    auto keyId = LdmlTestSource::parse_u8_source_string(key.get<std::string>());
    // now, look up the key
    if (!set_key_from_id(fillin.k, keyId)) {
      fillin.formatType(__FILE__, __LINE__, LDML_ACTION_FAIL, u"Could not find key: ", keyId);
    }
    return;
  } else if (type == "emit") {
    fillin.type   = LDML_ACTION_EMIT_STRING;
    fillin.string = LdmlTestSource::parse_u8_source_string(to.get<std::string>());
    if (!get_normalization_disabled()) {
      assert(km::core::ldml::normalize_nfd(fillin.string)); // TODO-LDML: will be NFC when core is normalizing to NFC
    }
    return;
  } else if (type == "backspace") {
    // backspace is handled as a key event
    fillin.type             = LDML_ACTION_KEY_EVENT;
    fillin.k.modifier_state = 0;
    fillin.k.vk             = KM_CORE_VKEY_BKSP;
    return;
  }

  // unhandled, so fail
  fillin.formatType(__FILE__, __LINE__, LDML_ACTION_FAIL, u"Error, unknown/unhandled action: ", (long)type);
  return;
}

const std::u16string &
LdmlJsonTestSource::get_context() {
  // load this on demand so that we can normalize (or not) properly.

  // TODO-LDML: Need an update to json.hpp to use contains()
  // if (data.contains("/startContext"_json_pointer)) {
  if (!loaded_context && data.find("startContext") != data.end()) {
    // only set startContext if present - it's optional.
    auto startContext = data["/startContext/to"_json_pointer];
    context = LdmlTestSource::parse_u8_source_string(startContext);
    if (!get_normalization_disabled()) {
      assert(km::core::ldml::normalize_nfd(context)); // TODO-LDML: should be NFC
    }
  }
  loaded_context = true;
  return context;
}

int LdmlJsonTestSource::load(const nlohmann::json &data) {
  this->data        = data;
  // TODO-LDML: validate here?
  return 0;
}

#if defined(HAVE_ICU4C)
class LdmlJsonRepertoireTestSource : public LdmlTestSource {
public:
  LdmlJsonRepertoireTestSource(const std::string &path, km::core::kmx::kmx_plus *kmxplus);
  virtual ~LdmlJsonRepertoireTestSource();
  virtual const std::u16string &get_context();
  int load(const nlohmann::json &test);
  virtual void next_action(ldml_action &fillin);
private:
  std::string path;
  nlohmann::json data;  // maybe
  std::string type;
  std::u16string context;
  std::u16string expected;
  std::string chars;
  std::unique_ptr<icu::UnicodeSet> uset;
  std::unique_ptr<icu::UnicodeSetIterator> iterator;
  bool need_check = false; // set this after each char
  const km::core::kmx::kmx_plus *kmxplus;
};

LdmlJsonRepertoireTestSource::LdmlJsonRepertoireTestSource(const std::string &path, km::core::kmx::kmx_plus *k)
:path(path), kmxplus(k){

}

LdmlJsonRepertoireTestSource::~LdmlJsonRepertoireTestSource() {
}

void
LdmlJsonRepertoireTestSource::next_action(ldml_action &fillin) {
  if (type != "simple") {
    fillin.formatType(__FILE__, __LINE__, LDML_ACTION_SKIP, u"TODO-LDML: Only 'simple' is supported, not ", type);
    return;
  }

  if (!iterator->next()) {
    fillin.type = LDML_ACTION_DONE;
    return;
  }

  if (need_check) {
    need_check = false;
    fillin.type = LDML_ACTION_CHECK_EXPECTED;
    fillin.string = expected;
    return;
  }

  // we have already excluded strings, so just get the codepoint
  const km_core_usv ch = iterator->getCodepoint();

  // as string for debugging.
  // const icu::UnicodeString& str = iterator->getString();

  km::core::kmx::char16_single ch16;
  std::size_t len = km::core::kmx::Utf32CharToUtf16(ch, ch16);
  std::u16string chstr = std::u16string(ch16.ch, len);
  if (!get_normalization_disabled()) {
    assert(km::core::ldml::normalize_nfd(chstr)); // TODO-LDML: will be NFC when core is normalizing to NFC
  }
  // append to expected
  expected.append(chstr);
  need_check = true;

  // ---------------------------------------------------
  // find a key that can emit this string.
  // TODO-LDML: no transforms yet.
  // TODO-LDML: looking for an exact single key for now

  assert(kmxplus != nullptr);
  // lookup the id
  assert(kmxplus->strs != nullptr);
  assert(kmxplus->key2 != nullptr);
  assert(kmxplus->layr != nullptr);

  assert(kmxplus->key2Helper.valid());
  assert(kmxplus->layrHelper.valid());

  // First, find the string as an id
  // TODO-LDML: will not work for multi string cases
  KMX_DWORD strId = kmxplus->strs->find(chstr); // not an error if chstr is 0, may be single ch

  // OK. Now we can search the keybag
  KMX_DWORD keyIndex = 0;
  auto *key2 = kmxplus->key2Helper.findKeyByStringTo(chstr, strId, keyIndex);
  if (key2 == nullptr) {
    fillin.formatType(__FILE__, __LINE__, LDML_ACTION_FAIL, u"No key for repertoire test: ", chstr);
    return;
  }

  // Now, look for the _first_ candidate vkey match in the kmap.
  for (KMX_DWORD kmapIndex = 0; kmapIndex < kmxplus->key2->kmapCount; kmapIndex++) {
    auto *kmap = kmxplus->key2Helper.getKmap(kmapIndex);
    assert(kmap != nullptr);
    if (kmap->key == keyIndex) {
      fillin.k = {(km_core_virtual_key)kmap->vkey, (uint16_t)kmap->mod};
      std::cout << "found vkey " << fillin.k.vk << ":" << fillin.k.modifier_state << std::endl;
      fillin.type = LDML_ACTION_KEY_EVENT;
      return;
    }
  }

  fillin.formatType(__FILE__, __LINE__, LDML_ACTION_FAIL, u"Could not find candidate vkey: ", chstr);
}

const std::u16string &
LdmlJsonRepertoireTestSource::get_context() {
  return context; // no context needed
}

int LdmlJsonRepertoireTestSource::load(const nlohmann::json &data) {
  this->data = data;  // TODO-LDML
  // Need an update to json.hpp to use contains()
  // if (data.contains("/type"_json_pointer)) {
  if (data.find("type") != data.end()) {
    type = data["/type"_json_pointer].get<std::string>();
  } else {
    type = "default";
  }
  chars      = data["/chars"_json_pointer].get<std::string>();
  std::cout << "Loaded " << path << " = " << this->type << " || " << this->chars << std::endl;
  UErrorCode status = U_ZERO_ERROR;
  icu::StringPiece piece(chars);
  icu::UnicodeString pattern = icu::UnicodeString::fromUTF8(piece);
  // icu::UnicodeString pattern(chars.data(), (int32_t)chars.length());
  uset = std::unique_ptr<icu::UnicodeSet>(new icu::UnicodeSet(pattern, status));
  if (U_FAILURE(status)) {
    // could be a malformed syntax isue
    std::cerr << "UnicodeSet c'tor problem " << u_errorName(status) << std::endl;
    return 1;
  }
  std::cout << "Got UnicodeSet of " << uset->size() << " char(s)." << std::endl;
  // #if (U_ICU_VERSION_MAJOR_NUM >= 70)
  // // TODO-LDML: function was private previously
  // if (uset->hasStrings()) {
  //   // illegal unicodeset of this form:  [a b c {this_is_a_string}]
  //   std::cerr << "Spec err: may not have strings. " << chars << std::endl;
  //   return 1;
  // }
  // #endif
  iterator = std::unique_ptr<icu::UnicodeSetIterator>(new icu::UnicodeSetIterator(*uset));
  return 0;
}
#endif // HAVE_ICU4C

LdmlJsonTestSourceFactory::LdmlJsonTestSourceFactory() : test_map() {
}

km::core::path
LdmlJsonTestSourceFactory::kmx_to_test_json(const km::core::path &kmx) {
  km::core::path p = kmx;
  p.replace_extension(TEST_JSON_SUFFIX);
  return p;
}

int LdmlJsonTestSourceFactory::load(const km::core::path &compiled, const km::core::path &path) {
  std::ifstream json_file(path.native());
  if (!json_file) {
    return -1; // no file
  }
  nlohmann::json data = nlohmann::json::parse(json_file);
  if (data.empty()) {
    return __LINE__; // empty
  }

  // check and load the KMX (yes, once again)
  if(!km::core::ldml_processor::is_kmxplus_file(compiled, rawdata)) {
    std::cerr << "Reading KMX for test purposes failed: " << compiled << std::endl;
    return __LINE__;
  }

  auto comp_keyboard = (const km::core::kmx::COMP_KEYBOARD*)rawdata.data();
  // initialize the kmxplus object with our copy
  kmxplus.reset(new km::core::kmx::kmx_plus(comp_keyboard, rawdata.size()));

  if (!kmxplus->is_valid()) {
    std::cerr << "kmx_plus invalid" << std::endl;
    return __LINE__;
  }

  if (!kmxplus->key2Helper.valid()) {
    std::cerr << "kmx_plus invalid" << std::endl;
    return __LINE__;
  }

  auto conformsTo = data["/keyboardTest3/conformsTo"_json_pointer].get<std::string>();
  assert_or_return(std::string(LDML_CLDR_TEST_VERSION_LATEST) == conformsTo);
  auto info_keyboard = data["/keyboardTest3/info/keyboard"_json_pointer].get<std::string>();
  auto info_author = data["/keyboardTest3/info/author"_json_pointer].get<std::string>();
  auto info_name = data["/keyboardTest3/info/name"_json_pointer].get<std::string>();
  // TODO-LDML: store these elsewhere?
  std::wcout << console_color::fg(console_color::BLUE) << "test file     = " << path.name().c_str() << console_color::reset() << std::endl;
  std::wcout << console_color::fg(console_color::YELLOW) << info_name.c_str() << "/ " << console_color::reset()
             << " test: " << info_keyboard.c_str() << " author: " << info_author.c_str() << std::endl;

  auto all_tests = data["/keyboardTest3/tests"_json_pointer];
  assert_or_return((!all_tests.empty()) && (all_tests.size() > 0));  // TODO-LDML: can be empty if repertoire only?

  for(auto tests : all_tests) {
    auto tests_name = tests["/name"_json_pointer].get<std::string>();
    for (auto test : tests["/test"_json_pointer]) {
      auto test_name = test["/name"_json_pointer].get<std::string>();
      std::string test_path;
      test_path.append(info_name).append("/tests/").append(tests_name).append("/").append(test_name);
      // std::cout << "JSON: reading " << info_name << "/" << test_path << std::endl;

      std::unique_ptr<LdmlJsonTestSource> subtest(new LdmlJsonTestSource(test_path, kmxplus.get()));
      assert_or_return(subtest->load(test) == 0);
      test_map[test_path] = std::unique_ptr<LdmlTestSource>(subtest.release());
    }
  }

#if defined(HAVE_ICU4C)
  auto rep_tests = data["/keyboardTest3/repertoire"_json_pointer];
  assert_or_return((!rep_tests.empty()) && (rep_tests.size() > 0));  // TODO-LDML: can be empty if tests only?

  for(auto rep : rep_tests) {
    auto rep_name  = rep["/name"_json_pointer].get<std::string>();

    std::string test_path;
    test_path.append(info_name).append("/repertoire/").append(rep_name);

    std::unique_ptr<LdmlJsonRepertoireTestSource> reptest(new LdmlJsonRepertoireTestSource(test_path, kmxplus.get()));
    assert_or_return(reptest->load(rep) == 0);
    test_map[test_path] = std::unique_ptr<LdmlTestSource>(reptest.release());
  }


#else
  std::cerr << "Warning: HAVE_ICU4C not defined, so not enabling repertoire tests" << std::endl;
#endif

  return 0;
}

const JsonTestMap&
LdmlJsonTestSourceFactory::get_tests() const {
  return test_map;
}


}  // namespace tests
}  // namespace km
