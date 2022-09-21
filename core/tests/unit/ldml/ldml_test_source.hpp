#ifndef __KMX_TEST_SOURCE_HPP__
#define __KMX_TEST_SOURCE_HPP__

#include "path.hpp"

namespace km {
namespace tests {

struct key_event {
  km_kbp_virtual_key vk;
  uint16_t modifier_state;
};

class LdmlTestSource {
public:
  LdmlTestSource();

  int load_source(
      const km::kbp::path &path,
      std::string &keys,
      std::u16string &expected,
      std::u16string &context,
      bool &expected_beep,
      bool &expected_error);

  key_event next_key(std::string &keys);

  int
  caps_lock_state() {
    return _caps_lock_on ? KM_KBP_MODIFIER_CAPS : 0;
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
  bool is_token(const std::string token, std::string &line);
  key_event char_to_event(char ch);
  uint16_t get_modifier(std::string const m);
  km_kbp_virtual_key get_vk(std::string const &vk);
  key_event vkey_to_event(std::string const &vk_event);
};

}  // namespace tests
}  // namespace km

#endif // __LDML_TEST_SOURCE_HPP__
