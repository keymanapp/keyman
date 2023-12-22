#ifndef __KMX_TEST_SOURCE_HPP__
#define __KMX_TEST_SOURCE_HPP__

#include "path.hpp"

namespace km {
namespace tests {

struct key_event {
  km_core_virtual_key vk;
  uint16_t modifier_state;
};

typedef enum { KOT_INPUT, KOT_OUTPUT, KOT_SAVED } kmx_option_type;

struct kmx_option {
  kmx_option_type type;
  std::u16string key, value, saved_value;
};

using kmx_options = std::vector<kmx_option>;

class KmxTestSource {
public:
  KmxTestSource();

  int load_source(
      const km::core::path &path,
      std::string &keys,
      std::u16string &expected,
      std::u16string &expected_context,
      std::u16string &context,
      kmx_options &options,
      bool &expected_beep);

  km_core_option_item *get_keyboard_options(kmx_options options);

  key_event next_key(std::string &keys);

  int
  caps_lock_state() {
    return _caps_lock_on ? KM_CORE_MODIFIER_CAPS : 0;
  }

  void
  toggle_caps_lock_state() {
    _caps_lock_on = !_caps_lock_on;
  }

  void set_caps_lock_on(bool caps_lock_on) {
    _caps_lock_on = caps_lock_on;
  }

private:
  bool _caps_lock_on = false;

  std::u16string parse_source_string(std::string const &s);
  bool parse_option_string(std::string line, kmx_options &options, kmx_option_type type);
  bool is_token(const std::string token, std::string &line);
  key_event char_to_event(char ch);
  uint16_t get_modifier(std::string const m);
  km_core_virtual_key get_vk(std::string const &vk);
  key_event vkey_to_event(std::string const &vk_event);
};

}  // namespace tests
}  // namespace km

#endif // __KMX_TEST_SOURCE_HPP__
