#ifndef __KMX_TEST_SOURCE_HPP__
#define __KMX_TEST_SOURCE_HPP__

#include "path.hpp"

namespace km {
namespace tests {

struct key_event {
  km_kbp_virtual_key vk;
  uint16_t modifier_state;
};

/**
 * pure virtual representing a test source, or a specific subtest
 */
class LdmlTestSource {
public:
  LdmlTestSource();
  virtual ~LdmlTestSource();
  virtual key_event next_key()                     = 0;
  virtual int caps_lock_state()                    = 0;
  virtual void toggle_caps_lock_state()            = 0;
  virtual void set_caps_lock_on(bool caps_lock_on) = 0;
  virtual km_kbp_status get_expected_load_status();
  virtual const std::u16string &get_context() const  = 0;
  virtual const std::u16string &get_expected() const = 0;
  virtual bool get_expected_beep() const;
};

class LdmlEmbeddedTestSource : public LdmlTestSource {
public:
  LdmlEmbeddedTestSource();
  virtual ~LdmlEmbeddedTestSource();

  /**
   * Load the test_source from comments in the .xml source
   */
  int load_source(const km::kbp::path &path);

  virtual key_event next_key();

  virtual int
  caps_lock_state();

  virtual void
  toggle_caps_lock_state();

  virtual void set_caps_lock_on(bool caps_lock_on);

  virtual km_kbp_status get_expected_load_status();
  virtual const std::u16string &get_context() const;
  virtual bool get_expected_beep() const;
  virtual const std::u16string &get_expected() const;

private:
  bool _caps_lock_on = false;

  std::u16string parse_source_string(std::string const &s);
  bool is_token(const std::string token, std::string &line);
  key_event char_to_event(char ch);
  uint16_t get_modifier(std::string const m);
  km_kbp_virtual_key get_vk(std::string const &vk);
  key_event vkey_to_event(std::string const &vk_event);
  key_event next_key(std::string &keys);

  std::string keys = "";
  std::u16string expected = u"", context = u"";
  bool expected_beep = false;
  bool expected_error = false;
};

}  // namespace tests
}  // namespace km

#endif // __LDML_TEST_SOURCE_HPP__
