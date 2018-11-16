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

struct char_to_vkey {
  km_kbp_virtual_key vk;
  bool shifted;
};

struct modifier_names {
  const char *name;
  uint16_t modifier;
};

extern const struct char_to_vkey s_char_to_vkey[];
extern const char *s_key_names[];
extern const struct modifier_names s_modifier_names[];

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
  km_kbp_virtual_key vk;
  while(std::getline(f, s, ' ')) {
    uint16_t modifier = get_modifier(s);
    if (modifier != 0) {
      modifier_state |= modifier;
    }
    else {
      vk = get_vk(s);
      assert(vk != 0);
      break;
    }
  }

  // The string should be empty at this point
  assert(!std::getline(f, s, ' '));

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


const struct char_to_vkey s_char_to_vkey[] = {
  {KM_KBP_VKEY_SPACE},     // 
  {'1', 1},       // !
  {KM_KBP_VKEY_QUOTE, 1},  // "
  {'3', 1},       // #
  {'4', 1},       // $
  {'5', 1},       // %
  {'7', 1},       // &
  {KM_KBP_VKEY_QUOTE},     // '
  {'9', 1},       // (
  {'0', 1},       // )
  {'8', 1},       // *
  {KM_KBP_VKEY_EQUAL, 1},  // +
  {KM_KBP_VKEY_COMMA},     // ,
  {KM_KBP_VKEY_HYPHEN},    // -
  {KM_KBP_VKEY_PERIOD},    // .
  {KM_KBP_VKEY_SLASH},     // /
  {'0'},
  {'1'},
  {'2'},
  {'3'},
  {'4'},
  {'5'},
  {'6'},
  {'7'},
  {'8'},
  {'9'},
  {KM_KBP_VKEY_COLON, 1},  // :
  {KM_KBP_VKEY_COLON},     // ;
  {KM_KBP_VKEY_COMMA, 1},  // <
  {KM_KBP_VKEY_EQUAL},     // =
  {KM_KBP_VKEY_PERIOD, 1}, // >
  {KM_KBP_VKEY_SLASH, 1},  // ?
  {'2', 1},       // @
  {'A', 1},
  {'B', 1},
  {'C', 1},
  {'D', 1},
  {'E', 1},
  {'F', 1},
  {'G', 1},
  {'H', 1},
  {'I', 1},
  {'J', 1},
  {'K', 1},
  {'L', 1},
  {'M', 1},
  {'N', 1},
  {'O', 1},
  {'P', 1},
  {'Q', 1},
  {'R', 1},
  {'S', 1},
  {'T', 1},
  {'U', 1},
  {'V', 1},
  {'W', 1},
  {'X', 1},
  {'Y', 1},
  {'Z', 1},
  {KM_KBP_VKEY_LBRKT},
  {KM_KBP_VKEY_BKSLASH},
  {KM_KBP_VKEY_RBRKT},
  {'6', 1},
  {KM_KBP_VKEY_HYPHEN, 1},
  {KM_KBP_VKEY_BKQUOTE},
  {'A'},
  {'B'},
  {'C'},
  {'D'},
  {'E'},
  {'F'},
  {'G'},
  {'H'},
  {'I'},
  {'J'},
  {'K'},
  {'L'},
  {'M'},
  {'N'},
  {'O'},
  {'P'},
  {'Q'},
  {'R'},
  {'S'},
  {'T'},
  {'U'},
  {'V'},
  {'W'},
  {'X'},
  {'Y'},
  {'Z'},
  {KM_KBP_VKEY_LBRKT, 1},
  {KM_KBP_VKEY_BKSLASH, 1},
  {KM_KBP_VKEY_RBRKT, 1},
  {KM_KBP_VKEY_BKQUOTE, 1},
  {0, 0}
};

const char *s_key_names[256] = {
  // Key Codes
    "K_?00",        // &H0
    "K_LBUTTON",      // &H1
    "K_RBUTTON",      // &H2
    "K_CANCEL",       // &H3
    "K_MBUTTON",      // &H4
    "K_?05",        // &H5
    "K_?06",        // &H6
    "K_?07",        // &H7
    "K_BKSP",         // &H8
    "K_TAB",          // &H9
    "K_?0A",        // &HA
    "K_?0B",        // &HB
    "K_KP5",          // &HC
    "K_ENTER",        // &HD
    "K_?0E",        // &HE
    "K_?0F",        // &HF
    "K_SHIFT",        // &H10
    "K_CONTROL",      // &H11
    "K_ALT",        // &H12
    "K_PAUSE",        // &H13
    "K_CAPS",       // &H14
    "K_KANJI?15",     // &H15
    "K_KANJI?16",     // &H16
    "K_KANJI?17",     // &H17
    "K_KANJI?18",     // &H18
    "K_KANJI?19",     // &H19
    "K_?1A",        // &H1A
    "K_ESC",        // &H1B
    "K_KANJI?1C",     // &H1C
    "K_KANJI?1D",     // &H1D
    "K_KANJI?1E",     // &H1E
    "K_KANJI?1F",     // &H1F
    "K_SPACE",        // &H20
    "K_PGUP",       // &H21
    "K_PGDN",       // &H22
    "K_END",        // &H23
    "K_HOME",       // &H24
    "K_LEFT",       // &H25
    "K_UP",       // &H26
    "K_RIGHT",        // &H27
    "K_DOWN",       // &H28
    "K_SEL",        // &H29
    "K_PRINT",        // &H2A
    "K_EXEC",       // &H2B
    "K_PRTSCN",     // &H2C
    "K_INS",        // &H2D
    "K_DEL",        // &H2E
    "K_HELP",       // &H2F
    "K_0",          // &H30
    "K_1",          // &H31
    "K_2",          // &H32
    "K_3",          // &H33
    "K_4",          // &H34
    "K_5",          // &H35
    "K_6",          // &H36
    "K_7",          // &H37
    "K_8",          // &H38
    "K_9",          // &H39
    "K_?3A",        // &H3A
    "K_?3B",        // &H3B
    "K_?3C",        // &H3C
    "K_?3D",        // &H3D
    "K_?3E",        // &H3E
    "K_?3F",        // &H3F
    "K_?40",        // &H40

    "K_A",          // &H41
    "K_B",          // &H42
    "K_C",          // &H43
    "K_D",          // &H44
    "K_E",          // &H45
    "K_F",          // &H46
    "K_G",          // &H47
    "K_H",          // &H48
    "K_I",          // &H49
    "K_J",          // &H4A
    "K_K",          // &H4B
    "K_L",          // &H4C
    "K_M",          // &H4D
    "K_N",          // &H4E
    "K_O",          // &H4F
    "K_P",          // &H50
    "K_Q",          // &H51
    "K_R",          // &H52
    "K_S",          // &H53
    "K_T",          // &H54
    "K_U",          // &H55
    "K_V",          // &H56
    "K_W",          // &H57
    "K_X",          // &H58
    "K_Y",          // &H59
    "K_Z",          // &H5A
    "K_?5B",        // &H5B
    "K_?5C",        // &H5C
    "K_?5D",        // &H5D
    "K_?5E",        // &H5E
    "K_?5F",        // &H5F
    "K_NP0",        // &H60
    "K_NP1",        // &H61
    "K_NP2",        // &H62
    "K_NP3",        // &H63
    "K_NP4",        // &H64
    "K_NP5",        // &H65
    "K_NP6",        // &H66
    "K_NP7",        // &H67
    "K_NP8",        // &H68
    "K_NP9",        // &H69
    "K_NPSTAR",     // &H6A
    "K_NPPLUS",     // &H6B
    "K_SEPARATOR",      // &H6C
    "K_NPMINUS",      // &H6D
    "K_NPDOT",        // &H6E
    "K_NPSLASH",      // &H6F
    "K_F1",       // &H70
    "K_F2",       // &H71
    "K_F3",       // &H72
    "K_F4",       // &H73
    "K_F5",       // &H74
    "K_F6",       // &H75
    "K_F7",       // &H76
    "K_F8",       // &H77
    "K_F9",       // &H78
    "K_F10",        // &H79
    "K_F11",        // &H7A
    "K_F12",        // &H7B
    "K_F13",        // &H7C
    "K_F14",        // &H7D
    "K_F15",        // &H7E
    "K_F16",        // &H7F
    "K_F17",        // &H80
    "K_F18",        // &H81
    "K_F19",        // &H82
    "K_F20",        // &H83
    "K_F21",        // &H84
    "K_F22",        // &H85
    "K_F23",        // &H86
    "K_F24",        // &H87

    "K_?88",        // &H88
    "K_?89",        // &H89
    "K_?8A",        // &H8A
    "K_?8B",        // &H8B
    "K_?8C",        // &H8C
    "K_?8D",        // &H8D
    "K_?8E",        // &H8E
    "K_?8F",        // &H8F

    "K_NUMLOCK",      // &H90
    "K_SCROLL",     // &H91

    "K_?92",        // &H92
    "K_?93",        // &H93
    "K_?94",        // &H94
    "K_?95",        // &H95
    "K_?96",        // &H96
    "K_?97",        // &H97
    "K_?98",        // &H98
    "K_?99",        // &H99
    "K_?9A",        // &H9A
    "K_?9B",        // &H9B
    "K_?9C",        // &H9C
    "K_?9D",        // &H9D
    "K_?9E",        // &H9E
    "K_?9F",        // &H9F
    "K_?A0",        // &HA0
    "K_?A1",        // &HA1
    "K_?A2",        // &HA2
    "K_?A3",        // &HA3
    "K_?A4",        // &HA4
    "K_?A5",        // &HA5
    "K_?A6",        // &HA6
    "K_?A7",        // &HA7
    "K_?A8",        // &HA8
    "K_?A9",        // &HA9
    "K_?AA",        // &HAA
    "K_?AB",        // &HAB
    "K_?AC",        // &HAC
    "K_?AD",        // &HAD
    "K_?AE",        // &HAE
    "K_?AF",        // &HAF
    "K_?B0",        // &HB0
    "K_?B1",        // &HB1
    "K_?B2",        // &HB2
    "K_?B3",        // &HB3
    "K_?B4",        // &HB4
    "K_?B5",        // &HB5
    "K_?B6",        // &HB6
    "K_?B7",        // &HB7
    "K_?B8",        // &HB8
    "K_?B9",        // &HB9

    "K_COLON",        // &HBA
    "K_EQUAL",        // &HBB
    "K_COMMA",        // &HBC
    "K_HYPHEN",     // &HBD
    "K_PERIOD",     // &HBE
    "K_SLASH",        // &HBF
    "K_BKQUOTE",      // &HC0

    "K_?C1",        // &HC1
    "K_?C2",        // &HC2
    "K_?C3",        // &HC3
    "K_?C4",        // &HC4
    "K_?C5",        // &HC5
    "K_?C6",        // &HC6
    "K_?C7",        // &HC7
    "K_?C8",        // &HC8
    "K_?C9",        // &HC9
    "K_?CA",        // &HCA
    "K_?CB",        // &HCB
    "K_?CC",        // &HCC
    "K_?CD",        // &HCD
    "K_?CE",        // &HCE
    "K_?CF",        // &HCF
    "K_?D0",        // &HD0
    "K_?D1",        // &HD1
    "K_?D2",        // &HD2
    "K_?D3",        // &HD3
    "K_?D4",        // &HD4
    "K_?D5",        // &HD5
    "K_?D6",        // &HD6
    "K_?D7",        // &HD7
    "K_?D8",        // &HD8
    "K_?D9",        // &HD9
    "K_?DA",        // &HDA

    "K_LBRKT",        // &HDB
    "K_BKSLASH",      // &HDC
    "K_RBRKT",        // &HDD
    "K_QUOTE",        // &HDE
    "K_oDF",        // &HDF
    "K_oE0",        // &HE0
    "K_oE1",        // &HE1
    "K_oE2",        // &HE2
    "K_oE3",        // &HE3
    "K_oE4",        // &HE4

    "K_?E5",        // &HE5

    "K_oE6",        // &HE6

    "K_?E7",        // &HE7
    "K_?E8",        // &HE8

    "K_oE9",        // &HE9
    "K_oEA",        // &HEA
    "K_oEB",        // &HEB
    "K_oEC",        // &HEC
    "K_oED",        // &HED
    "K_oEE",        // &HEE
    "K_oEF",        // &HEF
    "K_oF0",        // &HF0
    "K_oF1",        // &HF1
    "K_oF2",        // &HF2
    "K_oF3",        // &HF3
    "K_oF4",        // &HF4
    "K_oF5",        // &HF5

    "K_?F6",        // &HF6
    "K_?F7",        // &HF7
    "K_?F8",        // &HF8
    "K_?F9",        // &HF9
    "K_?FA",        // &HFA
    "K_?FB",        // &HFB
    "K_?FC",        // &HFC
    "K_?FD",        // &HFD
    "K_?FE",        // &HFE
    "K_?FF"       // &HFF
};

const struct modifier_names s_modifier_names[14] = {
  {"LCTRL", 0x0001},    // Left Control flag
  {"RCTRL", 0x0002},    // Right Control flag
  {"LALT", 0x0004},   // Left Alt flag
  {"RALT", 0x0008},   // Right Alt flag
  {"SHIFT", 0x0010},    // Either shift flag
  {"CTRL-do-not-use", 0x0020},    // Either ctrl flag -- don't use this for inputs
  {"ALT-do-not-use", 0x0040},   // Either alt flag -- don't use this for inputs
  {"CAPS", 0x0100},   // Caps lock on
  {"NCAPS", 0x0200},    // Caps lock NOT on
  {"NUMLOCK", 0x0400},    // Num lock on
  {"NNUMLOCK", 0x0800},   // Num lock NOT on
  {"SCROLL", 0x1000},   // Scroll lock on
  {"NSCROLL", 0x2000},    // Scroll lock NOT on
  {NULL, 0}
};

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


