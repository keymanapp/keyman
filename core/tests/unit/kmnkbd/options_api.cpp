/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the context API family of functions.
  Create Date:  30 Oct 2018
  Authors:      Tim Eves (TSE)
*/
#include <cstdlib>
#include <string>

#include "keyman_core.h"

#include "option.hpp"
#include "state.hpp"

#define   try_status(expr) \
{auto __s = (expr); if (__s != KM_CORE_STATUS_OK) std::exit(100*__LINE__+__s);}

namespace
{
#if 0
  km_core_option_item const test_env[] =
  {
    {u"isdummy",   u"yes", 0},
    {u"testing",   u"maybe", 0},
    KM_CORE_OPTIONS_END
  };

  km_core_option_item const test_kb[] =
  {
    {u"hello",     u"world", 0},
    KM_CORE_OPTIONS_END
  };

  km_core_option_item test_opts[] =
  {
    {u"isdummy",   u"no", KM_CORE_OPT_ENVIRONMENT},
    {u"hello",     u"globe", KM_CORE_OPT_KEYBOARD},
    KM_CORE_OPTIONS_END
  };

  km_core_option_item bad_key_opts[] =
  {
    {u"isdumber",   u"yes!", KM_CORE_OPT_ENVIRONMENT},
    KM_CORE_OPTIONS_END
  };

  km_core_option_item bad_scope_opts[] =
  {
    {u"hello",   u"!", KM_CORE_OPT_UNKNOWN},
    KM_CORE_OPTIONS_END
  };
#endif
  km_core_option_item const empty_options_list[] = {KM_CORE_OPTIONS_END};

  km_core_option_item const api_mock_options[] =
  {
    {u"hello",   u"world", KM_CORE_OPT_KEYBOARD},
    {u"isdummy",     u"yes", KM_CORE_OPT_ENVIRONMENT},
    {u"testing",     u"maybe", KM_CORE_OPT_ENVIRONMENT},
    KM_CORE_OPTIONS_END
  };

#if 0
  km::core::state mock_state(test_kb, test_env);
  km::core::state empty_state(empty_options_list, empty_options_list);


  std::string get_json_doc(km_core_state * const state)
  {
    size_t sz = 0;
    try_status(km_core_state_options_to_json(state, nullptr, &sz));
    std::string buf(sz-1, 0);
    try_status(km_core_state_options_to_json(state, &buf[0], &sz));

    return buf;
  }

  #define assert_lookup_equals(k,v,s) {if (!_assert_lookup_equals(k,v,s)) return __LINE__; }
  bool _assert_lookup_equals(std::u16string const key, std::u16string value, km_core_option_scope scope)
  {

    km_core_cu const * ret = nullptr;
    auto s = km_core_state_option_lookup(api_mock_options, scope,
                                         key.c_str(),
                                         &ret);
    bool v = s == KM_CORE_STATUS_OK && ret == value;
    return v;
  }

constexpr char const *empty_json = "\
{\n\
    \"keyboard\" : {},\n\
    \"environment\" : {},\n\
    \"saved\" : {\n\
        \"keyboard\" : {},\n\
        \"environment\" : {}\n\
    }\n\
}\n";

constexpr char const *mock_json = "\
{\n\
    \"keyboard\" : {\n\
        \"hello\" : \"world\"\n\
    },\n\
    \"environment\" : {\n\
        \"isdummy\" : \"yes\",\n\
        \"testing\" : \"maybe\"\n\
    },\n\
    \"saved\" : {\n\
        \"keyboard\" : {\n\
            \"hello\" : \"globe\"\n\
        },\n\
        \"environment\" : {\n\
            \"isdummy\" : \"no\"\n\
        }\n\
    }\n\
}\n";
#endif
}

int main(int, char * [])
{

  // Simple sanity tests on an empty options and mock options with 3 items.
  if (km_core_options_list_size(empty_options_list) != 0)
    return __LINE__;

  if (km_core_options_list_size(api_mock_options) != 3)
    return __LINE__;

#if 0
  km_core_cu const *value;
  auto s = km_core_options_lookup(api_empty_options,
                                     KM_CORE_OPT_ENVIRONMENT,
                                     u"isdummy", &value);
  if (s != KM_CORE_STATUS_KEY_ERROR) return __LINE__;
  if (get_json_doc(api_empty_options) != empty_json) return __LINE__;

  // Lets update data.
  assert_lookup_equals(u"isdummy", u"yes", KM_CORE_OPT_ENVIRONMENT);
  assert_lookup_equals(u"hello", u"world", KM_CORE_OPT_KEYBOARD);
  try_status(km_core_options_update(api_mock_options, test_opts));
  assert_lookup_equals(u"isdummy", u"no", KM_CORE_OPT_ENVIRONMENT);
  assert_lookup_equals(u"hello", u"globe", KM_CORE_OPT_KEYBOARD);

  // Test writing of non-exitent keys
  if (km_core_options_update(api_mock_options, bad_key_opts)
      != KM_CORE_STATUS_KEY_ERROR)
    return __LINE__;

  if (km_core_options_update(api_mock_options, bad_scope_opts)
      != KM_CORE_STATUS_INVALID_ARGUMENT)
    return __LINE__;

  // Create a single entry options set for testing fuller json output.
  // Because the underlying data structure is unordered there is no way define
  // a static json test document that will be consistently sorted across
  // runtimes or platforms.
  if (get_json_doc(api_mock_options) != mock_json) return __LINE__;
#endif

  return 0;
}
