/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Keyman Core - Helper functions for comparing debug items for tests
 */

#include <gtest/gtest.h>
#include "debug_items.hpp"

namespace km::tests {

bool are_store_offsets_equal(const uint16_t (&lhs)[DEBUG_STORE_OFFSETS_SIZE], const uint16_t (&rhs)[DEBUG_STORE_OFFSETS_SIZE]) {
  return std::equal(std::begin(lhs), std::end(lhs), std::begin(rhs));
  //  !memcmp(lhs, rhs, sizeof(lhs));
}

void debug_items_equal(
  km_core_state_debug_item const & lhs,
  km_core_state_debug_item const & rhs,
  bool &result
) {
  result = (lhs.type == rhs.type && lhs.flags == rhs.flags && lhs.kmx_info.first_action == rhs.kmx_info.first_action);
  if(result) {
    LPGROUP lgp = static_cast<LPGROUP>(lhs.kmx_info.group), rgp = static_cast<LPGROUP>(rhs.kmx_info.group);
    LPKEY lrule = static_cast<LPKEY>(lhs.kmx_info.rule), rrule = static_cast<LPKEY>(rhs.kmx_info.rule);
    LPSTORE loption_store = static_cast<LPSTORE>(lhs.kmx_info.option.store), roption_store = static_cast<LPSTORE>(rhs.kmx_info.option.store);
    switch(lhs.type) {
      case KM_CORE_DEBUG_BEGIN:
        result = lhs.key_info.character == rhs.key_info.character &&
                 lhs.key_info.modifier_state == rhs.key_info.modifier_state &&
                 lhs.key_info.vk == rhs.key_info.vk;
        break;
      case KM_CORE_DEBUG_END:
        break;
      case KM_CORE_DEBUG_GROUP_ENTER:
      case KM_CORE_DEBUG_GROUP_EXIT:
      case KM_CORE_DEBUG_MATCH_ENTER:
      case KM_CORE_DEBUG_MATCH_EXIT:
      case KM_CORE_DEBUG_NOMATCH_ENTER:
      case KM_CORE_DEBUG_NOMATCH_EXIT:
        ASSERT_NE(lgp, nullptr);
        ASSERT_NE(rgp, nullptr);
        ASSERT_NE(lgp->dpName, nullptr);
        ASSERT_NE(rgp->dpName, nullptr);
        result = u16cmp(lgp->dpName, rgp->dpName) == 0;
        break;
      case KM_CORE_DEBUG_RULE_ENTER:
      case KM_CORE_DEBUG_RULE_EXIT:
        ASSERT_NE(lgp, nullptr);
        ASSERT_NE(rgp, nullptr);
        ASSERT_NE(lgp->dpName, nullptr);
        ASSERT_NE(rgp->dpName, nullptr);
        ASSERT_NE(lrule, nullptr);
        ASSERT_NE(rrule, nullptr);
        result = u16cmp(lgp->dpName, rgp->dpName) == 0 &&
          lrule->Line == rrule->Line &&
          lrule->Key == rrule->Key &&
          lrule->ShiftFlags == rrule->ShiftFlags &&
          u16cmp(lhs.kmx_info.context, rhs.kmx_info.context) == 0 &&
          are_store_offsets_equal(lhs.kmx_info.store_offsets, rhs.kmx_info.store_offsets);
        break;
      case KM_CORE_DEBUG_SET_OPTION:
        ASSERT_NE(loption_store, nullptr);
        ASSERT_NE(roption_store, nullptr);
        result =
          u16cmp(loption_store->dpName, roption_store->dpName) == 0 &&
          u16cmp(lhs.kmx_info.option.value, rhs.kmx_info.option.value) == 0;
        break;
      default:
        FAIL();
    }
  }

  if(!result) {
    print_debug_item("actual", lhs);
    print_debug_item("expected", rhs);
  }
}

// TODO-WEB-CORE: avoid cout; instead use google test reporting once all tests are google test
void compare_debug_items(
  km_core_state const * state,
  std::initializer_list<km_core_state_debug_item> const & expected
) {
  size_t n = 0;
  auto act = km_core_state_debug_items(state, &n);

  for (auto &rhs: expected) {
    if ((int)--n < 0) {
      std::cout << "expected longer than actual" << std::endl;
      print_debug_item("next expected item:", rhs);
      FAIL();
    }

    bool result;
    ASSERT_NO_FATAL_FAILURE(debug_items_equal(*act++, rhs, result));
    ASSERT_TRUE(result);
  }

  if(n != 0) {
    std::cout << "actual longer than expected" << std::endl;
    print_debug_item("next actual item:", *act);
    FAIL();
  }
}

// debug items

const char *debug_item_types[] = {
  "KM_CORE_DEBUG_BEGIN", // = 0,
  "//KM_CORE_DEBUG_BEGIN_ANSI", // = 1, // not supported; instead rewrite ansi keyboards to Unicode with mcompile
  "KM_CORE_DEBUG_GROUP_ENTER", // = 2,
  "KM_CORE_DEBUG_GROUP_EXIT", // = 3,
  "KM_CORE_DEBUG_RULE_ENTER", // = 4,
  "KM_CORE_DEBUG_RULE_EXIT", // = 5,
  "KM_CORE_DEBUG_MATCH_ENTER", // = 6,
  "KM_CORE_DEBUG_MATCH_EXIT", // = 7,
  "KM_CORE_DEBUG_NOMATCH_ENTER", // = 8,
  "KM_CORE_DEBUG_NOMATCH_EXIT", // = 9,
  "KM_CORE_DEBUG_END", // = 10,
  "KM_CORE_DEBUG_SET_OPTION", // = 11
};

void print_debug_item(const char *title, km_core_state_debug_item const & item) {
  LPGROUP gp = static_cast<LPGROUP>(item.kmx_info.group);
  LPKEY rule = static_cast<LPKEY>(item.kmx_info.rule);
  LPSTORE store = static_cast<LPSTORE>(item.kmx_info.option.store);

  std::cout
    << "debug_item " << title << std::endl
    << "  type:    " << debug_item_types[item.type] << std::endl
    << "  flags:   " << item.flags << std::endl
    << "  key_info: (vk: " << item.key_info.vk
    <<    " mod:   " << item.key_info.modifier_state
    <<    " char:  " << item.key_info.character << ")" << std::endl;

  std::cout
    << "  kmx_info: "<< std::endl
    << "    context: '" << item.kmx_info.context << "'" << std::endl
    << "    action:  " << item.kmx_info.first_action << std::endl;

  if(gp) std::cout
    << "    group:   " << gp->dpName << std::endl;
  if(rule) std::cout
    << "    rule: " << std::endl
    << "      key:    " << rule->Key << std::endl
    << "      shift:  " << rule->ShiftFlags << std::endl
    << "      line:   " << rule->Line << std::endl;
  std::cout
    << "    offsets:  ";
  std::copy(
    std::begin(item.kmx_info.store_offsets),
    std::end(item.kmx_info.store_offsets),
    std::ostream_iterator<uint16_t>(std::cout, " ")
  );
  std::cout << std::endl;
  if(store) std::cout
    << "    option.store: " << std::endl
    << "      name:  " << store->dpName << std::endl
    << "      value: " << store->dpString << std::endl;
  if(store) std::cout
    << "    option.value: " << item.kmx_info.option.value << std::endl;
}

};