/*
  Copyright:    © 2018-2024 SIL International.
  Description:  Implementation of the state API functions using internal
                data structures and functions.
  Create Date:  15 Jan 2024
  Authors:      Marc Durdin
  History:      15 Jan 2024 - MCD - Refactor our km_core_state_context_set_if_needed
                                    and implement normalization
*/
#include <cassert>

#include "keyman_core.h"

#include "processor.hpp"
#include "state.hpp"
#include "debuglog.h"

#if !defined(HAVE_ICU4C)
#error icu4c is required for this code
#endif

#define U_FALLTHROUGH
#include "unicode/utypes.h"
#include "unicode/unistr.h"
#include "unicode/normalizer2.h"

using namespace km::core;

enum context_changed_type {
  // new and internal contexts are identical
  CONTEXT_UNCHANGED = 0,
  // new and internal contexts differ
  CONTEXT_DIFFERENT = 1,
  // internal context is shorter, but content is same as far as it goes
  CONTEXT_SHORTER = 2,
  // internal context is longer, but content is same as far as it goes
  CONTEXT_LONGER = 3,
};

typedef struct {
  uint8_t type;
  // the number of characters/context items (from beginning of context)
  // that are missing from the new/internal context.
  uint8_t end_difference;
} context_change_type;

// Forward declarations

bool replace_context(context_change_type context_changed, km_core_context *context, km_core_cp const *new_context);
bool should_normalize(km_core_state *state);
context_change_type is_context_unchanged(km_core_cp const *new_context, km_core_context *context);
bool do_normalize_nfd(km_core_cp const * src, std::u16string &dst);
km_core_context_status do_fail(km_core_context *app_context, km_core_context *cached_context, const char* error);

// ---------------------------------------------------------------------------

km_core_context_status
km_core_state_context_set_if_needed(
  km_core_state *state,
  km_core_cp const *new_app_context
) {
  assert(state != nullptr);
  assert(new_app_context != nullptr);
  if (state == nullptr || new_app_context == nullptr) {
    return KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT;
  }

  auto app_context    = km_core_state_app_context(state);
  auto cached_context = km_core_state_context(state);

  // Compare the internal app context with the passed-in application context

  auto context_changed = is_context_unchanged(new_app_context, app_context);
  if (context_changed.type != CONTEXT_UNCHANGED) {
    // We replace the internal app context with the passed-in application context
    if (!replace_context(context_changed, app_context, new_app_context)) {
      switch (context_changed.type) {
        case CONTEXT_DIFFERENT:
          return do_fail(app_context, cached_context, "could not set new app context");
        case CONTEXT_SHORTER:
          return do_fail(app_context, cached_context, "could not prepend new app context");
        case CONTEXT_LONGER:
          return do_fail(app_context, cached_context, "could not shrink app context");
      }
    }
  }

  // Finally, we normalize and replace the cached context

  std::u16string normalized_buffer;
  km_core_cp const *new_cached_context = nullptr;

  if (should_normalize(state)) {
    if (!do_normalize_nfd(new_app_context, normalized_buffer)) {
      return do_fail(app_context, cached_context, "could not normalize string");
    }
    new_cached_context = normalized_buffer.c_str();
  } else {
    new_cached_context = new_app_context;
  }

  context_changed = is_context_unchanged(new_cached_context, cached_context);
  if (context_changed.type == CONTEXT_UNCHANGED) {
    // We keep the context as is
    return KM_CORE_CONTEXT_STATUS_UNCHANGED;
  }

  if (!replace_context(context_changed, cached_context, new_cached_context)) {
    switch (context_changed.type) {
    case CONTEXT_DIFFERENT:
      return do_fail(app_context, cached_context, "could not set new cached context");
    case CONTEXT_SHORTER:
      return do_fail(app_context, cached_context, "could not prepend new cached context");
    case CONTEXT_LONGER:
      return do_fail(app_context, cached_context, "could not shrink cached context");
    }
  }

  return KM_CORE_CONTEXT_STATUS_UPDATED;
}

bool
replace_context(
  context_change_type context_changed,
  km_core_context *context,
  km_core_cp const *new_context
) {
  if (context_changed.type == CONTEXT_DIFFERENT) {
    if (set_context_from_string(context, new_context) != KM_CORE_STATUS_OK) {
      return false;
    }
  } else if (context_changed.type == CONTEXT_SHORTER) {
    km_core_context_item *new_context_items;
    if (context_items_from_utf16(new_context, &new_context_items) != KM_CORE_STATUS_OK ||
        context_prepend(context, new_context_items, context_changed.end_difference) != KM_CORE_STATUS_OK) {
      km_core_context_items_dispose(new_context_items);
      return false;
    }
  } else {
    assert(context_changed.type == CONTEXT_LONGER);
    if (context_shrink(context, context_changed.end_difference, false) != KM_CORE_STATUS_OK) {
      return false;
    }
  }
  return true;
}

/**
 * Returns true if the current keyboard processor wants a normalized cached context
 */
bool should_normalize(km_core_state *state) {
  return state->processor().supports_normalization();
}

/**
 * Returns the previous context item that is a character skipping any
 * markers, or NULL if there are no more characters before to
 * `context_items`.
 */
km_core_context_item const *
context_previous_char(
  km_core_context_item const *context_items,
  km_core_context_item const *context_start
) {
  if (context_items == context_start) {
    return NULL;
  }
  while (context_items > context_start) {
    context_items--;
    if (context_items->type == KM_CORE_CT_CHAR) {
      return context_items;
    }
    // skip over marker
  }
  return NULL;
}

/**
 * Returns true if the internal app context does not need to be updated to the new
 * app context
 */
context_change_type
is_context_unchanged(
  km_core_cp const *new_context_string,
  km_core_context *context
) {
  context_change_type change_type({CONTEXT_DIFFERENT, 0});
  if (new_context_string == nullptr || context == nullptr) {
    return change_type;
  }

  std::unique_ptr<km_core_cp[]> context_string(get_context_as_string(context));
  if (context_string[0] == '\0') {
    // If the app_context is "empty" then it needs updating
    return change_type;
  }

  km_core_context_item *context_items;
  if (context_get(context, &context_items) != KM_CORE_STATUS_OK) {
    return change_type;
  }

  // move to the end of the new and internal app context
  km_core_cp const *new_context_p = new_context_string;
  while (*new_context_p) {
    new_context_p++;
  }

  km_core_context_item const *context_p = context_items;
  while (context_p->type != KM_CORE_CT_END) {
    context_p++;
  }

  // we need to compare from the end of the cached context
  for (; new_context_p >= new_context_string && context_p != NULL;
        new_context_p--, context_p = context_previous_char(context_p, context_items)) {
    if (*new_context_p != context_p->character && context_p->type != KM_CORE_CT_END) {
      // The cached context doesn't match the application context
      km_core_context_items_dispose(context_items);
      return change_type;
    }
  }

  if (context_p == NULL && new_context_p < new_context_string) {
    // new and internal app contexts are identical
    change_type.type = CONTEXT_UNCHANGED;
    km_core_context_items_dispose(context_items);
    return change_type;
  }

  if (context_p >= context_items) {
    uint8_t n = 1;
    for (; context_p > context_items; context_p--) {
      n++;
    }
    change_type.type = CONTEXT_LONGER;
    change_type.end_difference = n;
    km_core_context_items_dispose(context_items);
    return change_type;
  }
  uint8_t n = 1;
  for (; new_context_p > new_context_string; new_context_p--) {
    n++;
  }
  change_type.type = CONTEXT_SHORTER;
  change_type.end_difference = n;
  km_core_context_items_dispose(context_items);
  return change_type;
}

/**
 * Normalize the input string using ICU
 */
bool do_normalize_nfd(km_core_cp const * src, std::u16string &dst) {
  UErrorCode icu_status = U_ZERO_ERROR;
  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(icu_status);
  assert(U_SUCCESS(icu_status));
  if(!U_SUCCESS(icu_status)) {
    // TODO: log the failure code
    return false;
  }
  icu::UnicodeString udst;
  icu::UnicodeString usrc = icu::UnicodeString(src);
  nfd->normalize(usrc, udst, icu_status);
  assert(U_SUCCESS(icu_status));
  if(!U_SUCCESS(icu_status)) {
    // TODO: log the failure code
    return false;
  }

  dst.assign(udst.getBuffer(), udst.length());
  return true;
}

/**
 * Clear the context when we have a failure so we don't end up with inconsistent
 * context buffers, and log the error to our diagnostic log.
 */
km_core_context_status do_fail(km_core_context *app_context, km_core_context *cached_context, const char* error) {
  DebugLog("%s", error);
  km_core_context_clear(app_context);
  km_core_context_clear(cached_context);
  return KM_CORE_CONTEXT_STATUS_CLEARED;
}
