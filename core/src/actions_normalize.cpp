/*
  Copyright:    © 2024 SIL International.
  Description:  Implementation of the action output normalization.
  Create Date:  16 Jan 2024
  Authors:      Marc Durdin (MCD)
  History:      16 Jan 2024 - MCD - Initial implementation from #9999
*/
#include <algorithm>
#include <sstream>
#include <memory>

#include <cassert>
#include "context.hpp"
#include "action.hpp"
#include "state.hpp"
#include "option.hpp"
#include "debuglog.h"
#include "util_normalize.hpp"
#include "utfcodec.hpp"
#include "kmx/kmx_xstring.h"

// forward declaration
bool context_items_to_unicode_string(km::core::context const *context, std::u32string &str);

/**
 * Normalize the output from an action to NFC, across the context | output
 * boundary, fixing up the app_context and the output actions to take into
 * account the NFU input app_context
 *
 * @param cached_context  the cached context, in NFD, after transform has been
 *                        applied to it by the keyboard processor
 * @param app_context     the app context, in NFU; transform has not been
 *                        applied, and will be applied by this function
 * @param actions         transform to apply, in NFD, which will be converted
 *                        to NFC by this function
 * @return true on success, false on failure
 */
bool km::core::actions_normalize(
  /* in */      km::core::context const *cached_context,
  /* in, out */ km::core::context *app_context,
  /* in, out */ km_core_actions &actions
) {
  assert(cached_context != nullptr);
  assert(app_context != nullptr);
  if(cached_context == nullptr || app_context == nullptr) {
    return false;
  }

  /*
    The code_points_to_delete value at this point is in NFD. The cached_context
    is in NFD and has already been updated by the keyboard processor to the
    expected result of the action, so we need to remove the output from a string
    copy of the cached_context to start, in order to get it to the same position
    as the app_context.

    The app_context is in NFU. We need to figure out how many characters to
    remove from the end of app_context in order to correctly normalize across
    the boundary, without normalizing more of the string than necessary.

    We do not need to mutate the cached_context itself, because it is already
    correct. This is good, because it means we will not lose track of markers
    within it. The app_context will be mutated, as it will need the new output
    appended, in order to match the expected result. Remember that the
    app_context does not contain markers; these are maintained only in the
    cached_context.
  */

  std::u32string output(actions.output);
  std::u32string cached_context_string, app_context_string;
  if (!context_items_to_unicode_string(cached_context, cached_context_string)) {
    return false;
  }
  if (!context_items_to_unicode_string(app_context, app_context_string)) {
    return false;
  }
  int nfu_to_delete = 0;

  /*
    Further debug assertion of inputs
  */
  assert(km::core::util::is_nfd(output));
  assert(km::core::util::is_nfd(cached_context_string));

  /*
    The keyboard processor will have updated the cached_context already,
    applying the transform to it, so we need to rewind this.

    Assert that 'cached_context_string' ends with 'output'

    Remove the output
    from cached_context_string to start.
  */

  assert(cached_context_string.length() >= output.length());
  size_t n = cached_context_string.length() - output.length();
  // auto end_cached = cached_context_string.substr(n, output.length());
  // assert(end_cached == output);
  assert(cached_context_string.compare(n, output.length(), output) == 0);
  cached_context_string.resize(n);

  /*
    While cached_context is guaranteed to be normalized, actions->output may not
    start at a normalization boundary. In order to achieve the correct NFC
    normalization in our output, we now need to look for a normalization
    boundary prior to the intersection of the cached_context and the output.
  */
  if(!output.empty()) {
    while(n > 0 && !km::core::util::has_nfd_boundary_before(output[0])) {
      // The output may interact with the context further in normalization. We
      // need to copy characters back further until we reach a normalization
      // boundary.

      // Remove last code point from the context ...

      auto len = cached_context_string.length();
      assert(len>0);
      auto chr = cached_context_string.at(len-1);
      cached_context_string.resize(len-1);

      // And prepend it to the output ...

      output.insert(0, 1, chr);
    }
  }

  /*
    At this point, our output and cached_context are coherent and normalization
    will be complete at the edit boundary.

    Now, we need to adjust the delete_back to match the number of codepoints
    that must actually be deleted from the applications's NFU context

    To adjust, we remove one codepoint at a time from the app_context until
    its normalized form matches the cached_context normalized form.
  */

  while(!app_context_string.empty()) {
    auto app_context_nfd = app_context_string;
    if(!km::core::util::normalize_nfd(app_context_nfd)) {
      DebugLog("nfd->normalize failed");
      return false;
    }

    if(app_context_nfd == cached_context_string) {
      break;
    }

    size_t len = app_context_string.length();
    // remove the cp at end
    app_context_string.resize(len-1);
    nfu_to_delete++;
  }

  /*
    Normalize our output string
  */

  auto output_nfc = output;
  if(!km::core::util::normalize_nfc(output_nfc)) {
    DebugLog("nfc->normalize failed");
    return false;
  }

  auto new_output = km::core::util::string_to_usv(output_nfc);
  assert(new_output != nullptr);
  if(new_output == nullptr) {
    return false;
  }

  /*
    Final steps -- set our outputs
  */

  // Append the new NFC output to our reduced app_context

  app_context_string.append(output_nfc);
  km_core_context_item *app_context_items = nullptr;
  km_core_status status = KM_CORE_STATUS_OK;
  if((status = context_items_from_utf32(app_context_string.c_str(), &app_context_items)) != KM_CORE_STATUS_OK) {
    DebugLog("context_items_from_string failed with %x", status);
    delete [] new_output;
    return false;
  }

  if((status = km_core_context_set(static_cast<km_core_context*>(app_context), app_context_items)) != KM_CORE_STATUS_OK) {
    DebugLog("km_core_context_set failed with %x", status);
    km_core_context_items_dispose(app_context_items);
    delete [] new_output;
    return false;
  }

  km_core_context_items_dispose(app_context_items);

  // Update actions with new NFC output + count of NFU code points to delete

  delete [] actions.output;
  actions.output = new_output;
  actions.code_points_to_delete = nfu_to_delete;

  return true;
}

/**
 * Helper to convert km_core_context list into a icu::UnicodeString
 */
bool context_items_to_unicode_string(km::core::context const *context, std::u32string &str) {

  km_core_context_item *items = nullptr;
  km_core_status status;
  if((status = km_core_context_get(static_cast<km_core_context const *>(context), &items)) != KM_CORE_STATUS_OK) {
    DebugLog("Failed to retrieve context with %s", status);
    return false;
  }
  size_t buf_size = 0;
  if((status = context_items_to_utf32(items, nullptr, &buf_size)) != KM_CORE_STATUS_OK) {
    DebugLog("Failed to retrieve context size with %s", status);
    km_core_context_items_dispose(items);
    return false;
  }

  km_core_usv *buf = new km_core_usv[buf_size];
  if((status = context_items_to_utf32(items, buf, &buf_size)) != KM_CORE_STATUS_OK) {
    DebugLog("Failed to retrieve context with %s", status);
    km_core_context_items_dispose(items);
    delete [] buf;
    return false;
  }

  str = std::u32string(buf, buf_size - 1); // don't include terminating null
  km_core_context_items_dispose(items);
  delete [] buf;
  return true;
}


/**
 * Refresh app_context to match the cached_context. Does not do normalization,
 * unlike `actions_normalize`. Used in conjunction with keyboard processors that
 * do not support normalization. Resulting app context is same as the cached
 * context, but without markers.
 *
 * @param cached_context  the cached context, in NFU, after transform has been
 *                        applied to it by the keyboard processor
 * @param app_context     the app context, in NFU; transform has not been
 *                        applied, and will effectively be applied by this
 *                        function
 * @return true on success, false on failure
 */
bool km::core::actions_update_app_context_nfu(
  /* in */      km::core::context const *cached_context,
  /* in, out */ km::core::context *app_context
) {
  km_core_status status = KM_CORE_STATUS_OK;
  km_core_context_item *items = nullptr;

  if((status = km_core_context_get(static_cast<km_core_context const *>(cached_context), &items)) != KM_CORE_STATUS_OK) {
    DebugLog("km_core_context_get failed with %d", status);
    return false;
  }

  // Strip markers from returned context

  int i, j;
  for(i = 0, j = 0; items[i].type != KM_CORE_CT_END; i++) {
    if(items[i].type != KM_CORE_CT_MARKER) {
      items[j] = items[i];
      j++;
    }
  }
  items[j] = KM_CORE_CONTEXT_ITEM_END;

  if((status = km_core_context_set(static_cast<km_core_context*>(app_context), items)) != KM_CORE_STATUS_OK) {
    DebugLog("km_core_context_set failed with %d", status);
  }

  delete [] items;

  return status == KM_CORE_STATUS_OK;
}
