/*
  Copyright:    © 2018 SIL International.
  Description:  Tests for the context API family of functions.
  Create Date:  30 Oct 2018
  Authors:      Tim Eves (TSE)
*/
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include "keyman_core.h"

#include "path.hpp"
#include "state.hpp"
#include "action_items.hpp"
#include "mock/mock_processor.hpp"

#include <test_assert.h>

#if defined(__GNUC__) || defined(__clang__)
#define PRAGMA(X)                   _Pragma(#X)
#define DISABLE_WARNING_PUSH        PRAGMA(GCC diagnostic push)
#define DISABLE_WARNING_POP         PRAGMA(GCC diagnostic pop)
#define DISABLE_WARNING(W)          PRAGMA(GCC diagnostic ignored #W)
#define DISABLE_WARNING_TYPE_LIMITS DISABLE_WARNING(-Wtype-limits)
#else
#define DISABLE_WARNING_PUSH
#define DISABLE_WARNING_POP
#define DISABLE_WARNING_TYPE_LIMITS
#endif

namespace
{
  std::string get_json_doc(km_core_state const& state)
  {
    size_t sz = 0;
    try_status(km_core_state_to_json(&state, nullptr, &sz));
    std::string buf(sz-1, 0);
    try_status(km_core_state_to_json(&state, &buf[0], &sz));

    return buf;
  }

  km_core_option_item test_env_opts[] =
  {
    {u"hello",     u"world", 0},
    KM_CORE_OPTIONS_END
  };

constexpr char const *doc1_expected = u8"\
{\n\
    \"$schema\" : \"keyman/core/doc/introspection.schema\",\n\
    \"keyboard\" : {\n\
        \"id\" : \"dummy\",\n\
        \"version\" : \"3.145\",\n\
        \"rules\" : []\n\
    },\n\
    \"context\" : [\n\
        \"H\",\n\
        \"e\",\n\
        \"l\",\n\
        \"l\",\n\
        \"o\",\n\
        \" \",\n\
        \"😁\",\n\
        \"S\",\n\
        \"I\",\n\
        \"L\"\n\
    ],\n\
    \"actions\" : [\n\
        { \"persist\" : { \"keyboard\" : { \"__test_point\" : \"F2 pressed test save.\" } } }\n\
    ]\n\
}\n";

constexpr char const *doc2_expected = u8"\
{\n\
    \"$schema\" : \"keyman/core/doc/introspection.schema\",\n\
    \"keyboard\" : {\n\
        \"id\" : \"dummy\",\n\
        \"version\" : \"3.145\",\n\
        \"rules\" : []\n\
    },\n\
    \"context\" : [],\n\
    \"actions\" : []\n\
}\n";


constexpr km_core_option_item const expected_persist_opt = {
  u"__test_point",
  u"F2 pressed test save.",
  KM_CORE_OPT_KEYBOARD
};

extern "C"
{
  uint8_t test_imx_callback(km_core_state *state, uint32_t imx_id, void *callback_object){

  // does nothing;
  return 1;
  }
};

} // namespace

int main(int argc, char * argv[])
{
  auto arg_color = std::string(argc > 1 ? argv[1] : "") == "--color";
  console_color::enabled = console_color::isaterminal() || arg_color;

  km_core_keyboard * test_kb = nullptr;
  km_core_state * test_state = nullptr,
               * test_clone = nullptr;
  test_kb = (km_core_keyboard *)new km::core::mock_processor(km::core::path("dummy.mock"));

  // Simple sanity tests.
  try_status(km_core_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_core_state_clone(test_state, &test_clone));
  // Check sub objects have been copied and not shared.
  if (km_core_state_context(test_state) == km_core_state_context(test_clone))
    return __LINE__;
  size_t n_actions = 0;
  if (km_core_state_action_items(test_state, &n_actions) == nullptr
      && n_actions != 0)
    return __LINE__;
  // Check registering platform engine callback
  km_core_state_imx_register_callback(test_state, test_imx_callback, nullptr);
  km_core_state_imx_deregister_callback(test_state);

  // Lets add data and do some basic checks of options and km_core_context
  km_core_context_item *citems = nullptr;
  try_status(context_items_from_utf16(u"Hello 😁", &citems));
  try_status(km_core_context_set(km_core_state_context(test_state), citems));
  km_core_context_items_dispose(citems);
  if(km_core_context_length(km_core_state_context(test_state)) != 7)
    return __LINE__;
  if(km_core_context_length(km_core_state_context(test_clone)) != 0)
    return __LINE__;

  // Overwrite some data.
  km_core_option_item new_opt[] = {
    {u"hello", u"globe", KM_CORE_OPT_ENVIRONMENT},
    KM_CORE_OPTIONS_END};
  try_status(km_core_state_options_update(test_clone, new_opt));

  // Test the engine
  auto attrs = km_core_get_engine_attrs(test_state);

  DISABLE_WARNING_PUSH
  DISABLE_WARNING_TYPE_LIMITS
  // Check the lib supplies our required interface.
  if (attrs->current - attrs->age > KM_CORE_LIB_CURRENT
      || attrs->current < KM_CORE_LIB_CURRENT) return __LINE__;
  if (attrs->max_context < 16) return __LINE__;
  DISABLE_WARNING_POP

  try_status(km_core_process_event(test_state, KM_CORE_VKEY_S,
                                  KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('S')}}, {KM_CORE_IT_END}}));
  try_status(km_core_process_event(test_state, KM_CORE_VKEY_I,
                                  KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('I')}}, {KM_CORE_IT_END}}));
  try_status(km_core_process_event(test_state, KM_CORE_VKEY_L, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('l')}}, {KM_CORE_IT_END}}));

  try_status(km_core_process_event(test_state, KM_CORE_VKEY_BKSP, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  assert(action_items(test_state, {{KM_CORE_IT_BACK, {0,}, {0}}, {KM_CORE_IT_END}}));
  try_status(km_core_process_event(test_state, KM_CORE_VKEY_L,
                                  KM_CORE_MODIFIER_SHIFT, 1, KM_CORE_EVENT_FLAG_DEFAULT));
  assert(action_items(test_state, {{KM_CORE_IT_CHAR, {0,}, {km_core_usv('L')}}, {KM_CORE_IT_END}}));
  try_status(km_core_process_event(test_state, KM_CORE_VKEY_F2, 0, 1, KM_CORE_EVENT_FLAG_DEFAULT));

  km_core_action_item action = {KM_CORE_IT_PERSIST_OPT, {0,}, };
  action.option = &expected_persist_opt;
  assert(action_items(test_state, {action, {KM_CORE_IT_END}}));

  // Test debug dump
  auto doc1 = get_json_doc(*test_state),
       doc2 = get_json_doc(*test_clone);

  std::cout << doc1 << std::endl;
  std::cout << doc2 << std::endl;

  // These should not be equal.
  if (doc1 == doc2)           return __LINE__;
  // These should be.
  if (doc1 != doc1_expected)  return __LINE__;
  if (doc2 != doc2_expected)  return __LINE__;

  // Destroy them
  km_core_state_dispose(test_state);
  km_core_state_dispose(test_clone);
  km_core_keyboard_dispose(test_kb);


  return 0;
}
