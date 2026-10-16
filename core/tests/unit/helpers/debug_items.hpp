#include <iostream>
#include "keyman_core.h"
#include "kmx/kmx_base.h"
#include "kmx/kmx_xstring.h"

using namespace km::core::kmx;

namespace km::tests {

// TODO: consider using same const approach in kmx_base.h
//       but this ripples through a whole lot of code
typedef struct tagDEBUG_GROUP
{
  KMX_WCHAR const * dpName;
  KEY const * dpKeyArray;   // [LPKEY] address of first item in key array
  KMX_WCHAR const * dpMatch;
  KMX_WCHAR const * dpNoMatch;
  KMX_DWORD cxKeyArray;   // in array entries
  KMX_BOOL  fUsingKeys;   // group(xx) [using keys] <-- specified or not
} DEBUG_GROUP, *LPDEBUG_GROUP;

typedef struct tagDEBUG_KEY
{
  KMX_WCHAR Key;
  KMX_DWORD Line;
  KMX_DWORD ShiftFlags;
  KMX_WCHAR const * dpOutput;
  KMX_WCHAR const * dpContext;
} DEBUG_KEY, *LPDEBUG_KEY;

typedef struct tagDEBUG_STORE
{
  KMX_DWORD dwSystemID;
  KMX_WCHAR const * dpName;
  KMX_WCHAR const * dpValue;
} DEBUG_STORE, *LPDEBUG_STORE;

bool are_store_offsets_equal(const uint16_t (&lhs)[DEBUG_STORE_OFFSETS_SIZE], const uint16_t (&rhs)[DEBUG_STORE_OFFSETS_SIZE]);

void debug_items_equal(
  km_core_state_debug_item const & lhs,
  km_core_state_debug_item const & rhs,
  bool &result
);

void compare_debug_items(
  km_core_state const * state,
  std::initializer_list<km_core_state_debug_item> const & expected
);

void print_debug_item(const char *title, km_core_state_debug_item const & item);


};
