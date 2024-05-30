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
#include "util_normalize.hpp"
#include "kmx/kmx_xstring.h" // for Unicode routines

using namespace km::core;

enum context_change_type {
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
  enum context_change_type type;
  // the number of context items (from beginning of context)
  // that are missing from the new/internal context (i.e. USV not UTF-16)
  uint32_t end_difference;
} context_change_result;

// Forward declarations

bool replace_context(context_change_result context_change, km_core_context *context, km_core_cu const *new_context);
bool should_normalize(km_core_state *state);
context_change_result get_context_change(km_core_cu const *new_context, km_core_context *context);
context_change_result get_context_items_change(km_core_context_item *new_context_items, km_core_context_item *context_items);

bool do_normalize_nfd(km_core_cu const * src, std::u16string &dst);
km_core_context_status do_fail(km_core_context *app_context, km_core_context *cached_context, const char* error);

// ---------------------------------------------------------------------------

km_core_context_status
km_core_state_context_set_if_needed(
  km_core_state *state,
  km_core_cu const *new_app_context
) {
  assert(state != nullptr);
  assert(new_app_context != nullptr);
  if (state == nullptr || new_app_context == nullptr) {
    return KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT;
  }

  // if the app context begins with a trailing surrogate,
  // skip over it.
  if (Uni_IsSurrogate2(*new_app_context)) {
    new_app_context++;
  }

  auto app_context    = km_core_state_app_context(state);
  auto cached_context = km_core_state_context(state);

  // Compare the internal app context with the passed-in application context

  auto context_change = get_context_change(new_app_context, app_context);
  if (context_change.type != CONTEXT_UNCHANGED) {
    // We replace the internal app context with the passed-in application context
    if (!replace_context(context_change, app_context, new_app_context)) {
      switch (context_change.type) {
        case CONTEXT_DIFFERENT:
          return do_fail(app_context, cached_context, "could not set new app context");
        case CONTEXT_SHORTER:
          return do_fail(app_context, cached_context, "could not prepend new app context");
        case CONTEXT_LONGER:
          return do_fail(app_context, cached_context, "could not shrink app context");
        case CONTEXT_UNCHANGED:
          break; // can't happen, but makes compiler happy
        }
    }
  }

  // Finally, we normalize and replace the cached context

  std::u16string normalized_buffer;
  km_core_cu const *new_cached_context = nullptr;

  if (should_normalize(state)) {
    if (!km::core::util::normalize_nfd(new_app_context, normalized_buffer)) {
      return do_fail(app_context, cached_context, "could not normalize string");
    }
    new_cached_context = normalized_buffer.c_str();
  } else {
    new_cached_context = new_app_context;
  }

  context_change = get_context_change(new_cached_context, cached_context);
  if (context_change.type == CONTEXT_UNCHANGED) {
    // We keep the context as is
    return KM_CORE_CONTEXT_STATUS_UNCHANGED;
  }

  if (!replace_context(context_change, cached_context, new_cached_context)) {
    switch (context_change.type) {
      case CONTEXT_DIFFERENT:
        return do_fail(app_context, cached_context, "could not set new cached context");
      case CONTEXT_SHORTER:
        return do_fail(app_context, cached_context, "could not prepend new cached context");
      case CONTEXT_LONGER:
        return do_fail(app_context, cached_context, "could not shrink cached context");
      case CONTEXT_UNCHANGED:
        break;  // can't happen, but makes compiler happy
    }
  }

  return KM_CORE_CONTEXT_STATUS_UPDATED;
}

bool
replace_context(
  context_change_result context_change,
  km_core_context *context,
  km_core_cu const *new_context
) {
  if (context_change.type == CONTEXT_DIFFERENT) {
    if (set_context_from_string(context, new_context) != KM_CORE_STATUS_OK) {
      return false;
    }
  } else if (context_change.type == CONTEXT_SHORTER) {
    km_core_context_item *new_context_items = nullptr;
    if (context_items_from_utf16(new_context, &new_context_items) != KM_CORE_STATUS_OK ||
        context_prepend(context, new_context_items, context_change.end_difference) != KM_CORE_STATUS_OK) {
      km_core_context_items_dispose(new_context_items);
      return false;
    }
    km_core_context_items_dispose(new_context_items);
  } else {
    assert(context_change.type == CONTEXT_LONGER);
    if (context_shrink(context, context_change.end_difference, false) != KM_CORE_STATUS_OK) {
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
 * Analyzes the difference between existing context and a new context string
 * retrieved from the application, and returns the type of change and the
 * number of context items affected by the change (including markers if they
 * are in the original context).
 */
context_change_result
get_context_change(
  km_core_cu const *new_context_string,
  km_core_context *context
) {
  context_change_result change_type({CONTEXT_DIFFERENT, 0});
  assert(new_context_string != nullptr);
  assert(context != nullptr);
  if (new_context_string == nullptr || context == nullptr) {
    return change_type;
  }

  km_core_context_item *new_context_items = nullptr;
  km_core_context_item *context_items = nullptr;

  if(context_items_from_utf16(new_context_string, &new_context_items) == KM_CORE_STATUS_OK) {
    if(km_core_context_get(context, &context_items) == KM_CORE_STATUS_OK) {
      change_type = get_context_items_change(new_context_items, context_items);
    }
  }

  km_core_context_items_dispose(new_context_items);
  km_core_context_items_dispose(context_items);

  return change_type;
}

/**
 * Analyzes the difference between existing context items array and a new
 * context items array retrieved from the application, and returns the type of
 * change and the number of context items affected by the change (including
 * markers if they are in the original context).
 */
context_change_result
get_context_items_change(
  km_core_context_item *new_context_items,
  km_core_context_item *context_items
) {
  int n = 0;
  km_core_context_item const *context_p = nullptr, *new_context_p = nullptr;

  context_change_result change_type({CONTEXT_DIFFERENT, 0});

  if(context_items->type == KM_CORE_CT_END) {
    // If the app_context is "empty" then it needs updating
    return change_type;
  }

  // move to the end of the new and internal app context
  new_context_p = new_context_items;
  while (new_context_p->type != KM_CORE_CT_END) {
    new_context_p++;
  }

  context_p = context_items;
  while (context_p->type != KM_CORE_CT_END) {
    context_p++;
  }

  // Move to last character for each pointer
  new_context_p = context_previous_char(new_context_p, new_context_items);
  context_p = context_previous_char(context_p, context_items);

  // we need to compare from the end of the cached context
  for (; new_context_p != NULL && context_p != NULL;
        new_context_p = context_previous_char(new_context_p, new_context_items),
        context_p = context_previous_char(context_p, context_items)) {
    if (new_context_p->character != context_p->character) {
      // The cached context doesn't match the application context
      return change_type;
    }
  }

  if (context_p == NULL && new_context_p == NULL) {
    // new and internal app contexts are identical
    change_type.type = CONTEXT_UNCHANGED;
    return change_type;
  }

  if (context_p >= context_items) {
    for (n = 1; context_p > context_items; context_p--) {
      n++;
    }
    change_type.type = CONTEXT_LONGER;
    change_type.end_difference = n;
    return change_type;
  }

  for (n = 1; new_context_p > new_context_items; new_context_p--) {
    n++;
  }
  change_type.type = CONTEXT_SHORTER;
  change_type.end_difference = n;
  return change_type;
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
