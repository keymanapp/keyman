/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the context API family of functions.
  Create Date:  30 Oct 2018
  Authors:      Tim Eves (TSE)
*/
#include <cstdlib>
#include <string>

#include <keyboardprocessor.h>

#include "option.hpp"

#define   try_status(expr) \
{auto __s = (expr); if (__s != KM_KBP_STATUS_OK) std::exit(100*__LINE__+__s);}

namespace
{
  km::kbp::options_set mock_options = KM_KBP_OPT_ENVIRONMENT;
  km_kbp_options_set api_mock_options = mock_options;

  std::string get_json_doc(km_kbp_options_set const& opts)
  {
    size_t sz = 0;
    try_status(km_kbp_options_set_to_json(&opts, nullptr, &sz));
    std::string buf(sz-1, 0);
    try_status(km_kbp_options_set_to_json(&opts, &buf[0], &sz));

    return buf;
  }

  #define assert_lookup_equals(k,v,s) {if (!_assert_lookup_equals(k,v,s)) return __LINE__; }
  bool _assert_lookup_equals(std::string const key, std::string value, km_kbp_option_scope scope)
  {
    auto opt = km_kbp_options_set_lookup(&api_mock_options, key.c_str());
    return opt && opt->key == key
               && opt->value == value
               && opt->scope == scope;
  }

  km_kbp_option test_opts[] =
  {
    {"isdummy",   "yes", 0},
    {"envstuff",  "things", KM_KBP_OPT_ENVIRONMENT},
    {"hello",     "world", 0},
    {"showhowscopeis",  "ignored on set", KM_KBP_OPT_KEYBOARD},
    KM_KBP_OPTIONS_END
  };

constexpr char const *empty_json = "\
{\n\
    \"scope\" : \"enviroment\",\n\
    \"options\" : {}\n\
}\n";

constexpr char const *mock_json = "\
{\n\
    \"scope\" : \"enviroment\",\n\
    \"options\" : {\n\
        \"hello\" : \"globe\"\n\
    }\n\
}\n";

}

int main(int, char * [])
{
  // Simple sanity tests on an empty options_set.
  if (km_kbp_options_set_size(&api_mock_options) != 0)
    return __LINE__;
  if (km_kbp_options_set_lookup(&api_mock_options, "isdummy") != nullptr)
    return __LINE__;
  if (get_json_doc(api_mock_options) != empty_json) return __LINE__;

  // Lets add data.
  try_status(km_kbp_options_set_update(&api_mock_options, test_opts));
  if (km_kbp_options_set_size(&api_mock_options)
      != sizeof test_opts/sizeof(*test_opts)-1)
    return __LINE__;

  assert_lookup_equals("isdummy", "yes", KM_KBP_OPT_ENVIRONMENT);
  assert_lookup_equals("envstuff", "things", KM_KBP_OPT_ENVIRONMENT);
  assert_lookup_equals("showhowscopeis", "ignored on set", KM_KBP_OPT_ENVIRONMENT);
  assert_lookup_equals("hello", "world", KM_KBP_OPT_ENVIRONMENT);

  // Overwrite some data.
  km_kbp_option new_opt[] = {{"hello", "globe", 0}, KM_KBP_OPTIONS_END};
  try_status(km_kbp_options_set_update(&api_mock_options, new_opt));
  assert_lookup_equals("hello", "globe", KM_KBP_OPT_ENVIRONMENT);
  if (km_kbp_options_set_size(&api_mock_options)
      != sizeof test_opts/sizeof(*test_opts)-1)
    return __LINE__;

  // Create a single entry options set for testing fuller json output.
  // Because the underlying data structure is unordered there is no way define
  // a static json test document that will be consistently sorted across
  // runtimes or platforms.
  mock_options.clear();
  mock_options.insert({"hello", "globe"});
  if (get_json_doc(api_mock_options) != mock_json) return __LINE__;

  return 0;
}
