#ifndef __KMX_TEST_SOURCE_HPP__
#define __KMX_TEST_SOURCE_HPP__

#include "path.hpp"

#include <map>
#include <memory>

#include "kmx/kmx_plus.h"

namespace km {
namespace tests {

struct key_event {
  km_core_virtual_key vk;
  uint16_t modifier_state;
};

enum ldml_action_type {
  /**
   * Done. no more actions
  */
  LDML_ACTION_DONE,
  /**
   * Skip this test
  */
  LDML_ACTION_SKIP,
  /**
   * key_event - a vkey
  */
  LDML_ACTION_KEY_EVENT,
  /**
   * string - emit this
  */
  LDML_ACTION_EMIT_STRING,
  /**
   * expected text
  */
  LDML_ACTION_CHECK_EXPECTED,
  // TODO-LDML: gestures, etc? Depends on touch.

  /**
   * fail test.  'string' has message.
  */
  LDML_ACTION_FAIL,
};

struct ldml_action {
  ldml_action_type type;
  key_event k;
  std::u16string string;

  /** mark failure as specified type */
  void formatType(const char *file, int line, ldml_action_type type, const std::u16string &msg);
  /** mark failure as specified type. msg2 is concatenated */
  void formatType(const char *file, int line, ldml_action_type type, const std::u16string &msg, const std::u16string &msg2);
  /** mark failure as specified type. msg2 is concatenated */
  void formatType(const char *file, int line, ldml_action_type type, const std::u16string &msg, long msg2);
  /** mark failure as specified type.  msg2 is concatenated */
  void formatType(const char *file, int line, ldml_action_type type, const std::u16string &msg, const std::string &msg2);

  /** @returns true if caller should stop processing events */
  bool done() const;
};

/**
 * pure virtual representing a test source, or a specific subtest
 */
class LdmlTestSource {
public:
  LdmlTestSource();
  virtual ~LdmlTestSource();
  virtual void next_action(ldml_action &fillin) = 0;
  virtual int caps_lock_state();
  virtual void toggle_caps_lock_state();
  virtual void set_caps_lock_on(bool caps_lock_on);
  virtual km_core_status get_expected_load_status();
  virtual const std::u16string &get_context() = 0;
  virtual bool get_expected_beep() const;

  // helper functions
  static key_event char_to_event(char ch);
  static uint16_t get_modifier(std::string const m);
  static std::u16string parse_source_string(std::string const &s);
  static std::u16string parse_u8_source_string(std::string const &s);

  // sets the normalization switch.
  // why is this here? to prevent an additional pass of parsing the KMX+ file.
  void set_normalization_disabled(bool is_disabled) {
    normalization_disabled = is_disabled;
    setup = true;
  }

  bool get_normalization_disabled() const {
    assert(setup); // make sure set_ was called first
    return normalization_disabled;
  }

private:
  bool normalization_disabled = false;
  bool setup = false;

private:
  bool _caps_lock_on = false;
};

typedef std::map<std::string, std::unique_ptr<km::tests::LdmlTestSource>> JsonTestMap;

class LdmlJsonTestSourceFactory {
  public:
    LdmlJsonTestSourceFactory();
    /**
     * @param compiled the KMX - for lookup
     * @param path the json
    */
    int load(const km::core::path &compiled, const km::core::path &path);

    static km::core::path kmx_to_test_json(const km::core::path& kmx);

    const JsonTestMap& get_tests() const;
  private:
    JsonTestMap test_map;
    // copy of the kbd data, for lookups
    std::vector<uint8_t> rawdata;
    std::unique_ptr<km::core::kmx::kmx_plus> kmxplus;
};


class LdmlEmbeddedTestSource : public LdmlTestSource {
public:
  LdmlEmbeddedTestSource();
  virtual ~LdmlEmbeddedTestSource();

  /**
   * Load the test_source from comments in the .xml source
   */
  int load_source(const km::core::path &path);

  virtual km_core_status get_expected_load_status();
  virtual const std::u16string &get_context();
  virtual bool get_expected_beep() const;

  virtual void next_action(ldml_action &fillin);

private:

  bool is_token(const std::string token, std::string &line);
  key_event vkey_to_event(std::string const &vk_event);
  key_event next_key(std::string &keys);
  key_event next_key();

  std::deque<std::string> keys;
  std::deque<std::u16string> expected;
  std::u16string context = u"";
  bool expected_beep = false;
  bool expected_error = false;
  bool is_done = false;
};

}  // namespace tests
}  // namespace km

#endif // __LDML_TEST_SOURCE_HPP__
