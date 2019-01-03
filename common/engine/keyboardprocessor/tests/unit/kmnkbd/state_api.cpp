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

#include <keyman/keyboardprocessor.h>
#include "path.hpp"

#include "state.hpp"

#define   try_status(expr) \
{auto __s = (expr); if (__s != KM_KBP_STATUS_OK) std::exit(100*__LINE__+__s);}

namespace
{
  std::string get_json_doc(km_kbp_state const& state)
  {
    size_t sz = 0;
    try_status(km_kbp_state_to_json(&state, nullptr, &sz));
    std::string buf(sz-1, 0);
    try_status(km_kbp_state_to_json(&state, &buf[0], &sz));

    return buf;
  }

  km_kbp_option_item test_env_opts[] =
  {
    {u"hello",     u"world", 0},
    KM_KBP_OPTIONS_END
  };

constexpr char const *doc1_expected = u8"\
{\n\
    \"$schema\" : \"keyman/keyboardprocessor/doc/introspection.schema\",\n\
    \"keyboard\" : {\n\
        \"id\" : \"dummy\",\n\
        \"folder\" : \"\",\n\
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
    \"$schema\" : \"keyman/keyboardprocessor/doc/introspection.schema\",\n\
    \"keyboard\" : {\n\
        \"id\" : \"dummy\",\n\
        \"folder\" : \"\",\n\
        \"version\" : \"3.145\",\n\
        \"rules\" : []\n\
    },\n\
    \"context\" : [],\n\
    \"actions\" : []\n\
}\n";


constexpr km_kbp_option_item const expected_persist_opt = {
  u"__test_point",
  u"F2 pressed test save.",
  KM_KBP_OPT_KEYBOARD
};

inline
bool operator==(km_kbp_option_item const & lhs, km_kbp_option_item const & rhs)
{
  return lhs.scope == rhs.scope
      && std::u16string(lhs.key) == rhs.key
      && std::u16string(lhs.value) == rhs.value;
}


bool operator==(km_kbp_action_item const & lhs,
                km_kbp_action_item const & rhs)
{
  if (lhs.type != rhs.type) return false;
  switch(lhs.type)
  {
    case KM_KBP_IT_CHAR:        return lhs.character == rhs.character;
    case KM_KBP_IT_MARKER:      return lhs.marker == rhs.marker;
    case KM_KBP_IT_PERSIST_OPT: return *lhs.option == *rhs.option;
    default: break;
  }

  return true;
}

#ifdef assert
#undef assert
#endif
#define assert(expr) {if (!(expr)) return __LINE__; }
bool action_items(km_kbp_state const * state,
                  std::initializer_list<km_kbp_action_item> const & expected)
{
  size_t n = 0;
  auto act = km_kbp_state_action_items(state, &n);

  for (auto &rhs: expected)
    if (!(*act++ == rhs)) return false;

  return true;
}

} // namespace

int main(int, char * [])
{
  km_kbp_keyboard * test_kb = nullptr;
  km_kbp_state * test_state = nullptr,
               * test_clone = nullptr;
  try_status(km_kbp_keyboard_load(km::kbp::path("dummy.mock").c_str(), &test_kb));

  // Simple sanity tests.
  try_status(km_kbp_state_create(test_kb, test_env_opts, &test_state));
  try_status(km_kbp_state_clone(test_state, &test_clone));
  // Check sub objects have been copied and not shared.
  if (km_kbp_state_context(test_state) == km_kbp_state_context(test_clone))
    return __LINE__;
  size_t n_actions = 0;
  if (km_kbp_state_action_items(test_state, &n_actions) == nullptr
      && n_actions != 0)
    return __LINE__;

  // Lets add data and do some basic checks of options and km_kbp_context
  km_kbp_context_item *citems = nullptr;
  try_status(km_kbp_context_items_from_utf16(u"Hello 😁", &citems));
  try_status(km_kbp_context_set(km_kbp_state_context(test_state), citems));
  km_kbp_context_items_dispose(citems);
  if(km_kbp_context_length(km_kbp_state_context(test_state)) != 7)
    return __LINE__;
  if(km_kbp_context_length(km_kbp_state_context(test_clone)) != 0)
    return __LINE__;

  // Overwrite some data.
  km_kbp_option_item new_opt[] = {
    {u"hello", u"globe", KM_KBP_OPT_ENVIRONMENT},
    KM_KBP_OPTIONS_END};
  try_status(km_kbp_state_options_update(test_clone, new_opt));

  // Test the engine
  auto attrs = km_kbp_get_engine_attrs(test_state);
  // Check the lib supplies our required interface.
  if (attrs->current - attrs->age > KM_KBP_LIB_CURRENT
      || attrs->current < KM_KBP_LIB_CURRENT) return __LINE__;
  if (attrs->max_context < 16) return __LINE__;

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_S,
                                  KM_KBP_MODIFIER_SHIFT));
  assert(action_items(test_state, {{KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('S')}}}));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_I,
                                  KM_KBP_MODIFIER_SHIFT));
  assert(action_items(test_state, {{KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('I')}}}));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_L, 0));
  assert(action_items(test_state, {{KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('l')}}}));

  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_BKSP, 0));
  assert(action_items(test_state, {{KM_KBP_IT_BACK, {0,}, {0}}}));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_L,
                                  KM_KBP_MODIFIER_SHIFT));
  assert(action_items(test_state, {{KM_KBP_IT_CHAR, {0,}, {km_kbp_usv('L')}}}));
  try_status(km_kbp_process_event(test_state, KM_KBP_VKEY_F2,0));
  assert(action_items(test_state, {{KM_KBP_IT_PERSIST_OPT, {0,},
                      {uintptr_t(&expected_persist_opt)}}}));

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
  km_kbp_state_dispose(test_state);
  km_kbp_state_dispose(test_clone);
  km_kbp_keyboard_dispose(test_kb);


  return 0;
}
