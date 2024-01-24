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
#include "core_icu.h"

using namespace km::core;

// Forward declarations

bool should_normalize(km_core_state *state);
bool is_context_valid(km_core_cp const * context, km_core_cp const * cached_context);
bool do_normalize_nfd(km_core_cp const * src, std::u16string &dst);
km_core_context_status do_fail(km_core_context *app_context, km_core_context *cached_context, const char* error);

// ---------------------------------------------------------------------------

km_core_context_status km_core_state_context_set_if_needed(
  km_core_state *state,
  km_core_cp const *new_app_context
) {
  assert(state != nullptr);
  assert(new_app_context != nullptr);
  if(state == nullptr || new_app_context == nullptr) {
    return KM_CORE_CONTEXT_STATUS_INVALID_ARGUMENT;
  }

  auto app_context = km_core_state_app_context(state);
  auto cached_context = km_core_state_context(state);

  // Retrieve the existing internally cached app context for comparison

  std::unique_ptr<km_core_cp[]> app_context_string(get_context_as_string(app_context));

  // Compare the internal app context with the passed-in application context

  if(is_context_valid(new_app_context, app_context_string.get())) {
    // We keep the context as is
    return KM_CORE_CONTEXT_STATUS_UNCHANGED;
  }

  // We replace the internal app context with the passed-in application context

  if(set_context_from_string(app_context, new_app_context) != KM_CORE_STATUS_OK) {
    return do_fail(app_context, cached_context, "could not set new app context");
  }

  // Finally, we normalize and replace the cached context

  std::u16string normalized_buffer;
  km_core_cp const *new_cached_context = nullptr;

  if(should_normalize(state)) {
    if(!do_normalize_nfd(new_app_context, normalized_buffer)) {
      return do_fail(app_context, cached_context, "could not normalize string");
    }
    new_cached_context = normalized_buffer.c_str();
  } else {
    new_cached_context = new_app_context;
  }

  // TODO: #10100 will alter how we replace the cached context here -- maintaining
  //       markers as far as possible

  if(set_context_from_string(cached_context, new_cached_context) != KM_CORE_STATUS_OK) {
    return do_fail(app_context, cached_context, "could not set new cached context");
  }

  return KM_CORE_CONTEXT_STATUS_UPDATED;
}

/**
 * Returns true if the current keyboard processor wants a normalized cached context
 */
bool should_normalize(km_core_state *state) {
  return state->processor().supports_normalization();
}

/**
 * Returns true if the internal app context does not need to be updated to the new
 * app context
 *
 * TODO: #10100 will alter some of the assumptions here
 */
bool is_context_valid(km_core_cp const * new_app_context, km_core_cp const * app_context) {
  if (new_app_context == nullptr || app_context == nullptr || *app_context == '\0') {
    // If the app_context is "empty" then it needs updating
    return false;
  }
  km_core_cp const* new_app_context_p = new_app_context;
  while(*new_app_context_p) {
    new_app_context_p++;
  }

  km_core_cp const* app_context_p = app_context;
  while(*app_context_p) {
    app_context_p++;
  }

  // we need to compare from the end of the cached context
  for(; new_app_context_p >= new_app_context && app_context_p >= app_context; new_app_context_p--, app_context_p--) {
    if(*new_app_context_p != *app_context_p) {
      // The cached context doesn't match the application context, so it is
      // invalid
      return false;
    }
  }

  if(app_context_p > app_context) {
    // if the cached context is longer than the application context, then we also
    // assume that it is invalid
    return false;
  }

  // It's acceptable for the application context to be longer than the cached
  // context, so if we match the whole cached context, we can safely return true
  return true;
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