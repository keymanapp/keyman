// TEMP
#define KMN_NO_ICU 0
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
#include "core_icu.h"

// forward declarations

icu::UnicodeString context_items_to_unicode_string(km::core::context const *context);


// TEMP
namespace km {
namespace core {
namespace util {

/**
 * Helper to convert icu::UnicodeString to a UTF-32 km_core_usv buffer,
 * nul-terminated
 */
inline km_core_usv *unicode_string_to_usv(icu::UnicodeString& src) {
  UErrorCode icu_status = U_ZERO_ERROR;

  km_core_usv *dst = new km_core_usv[src.length() + 1];

  src.toUTF32(reinterpret_cast<UChar32*>(dst), src.length(), icu_status);

  assert(U_SUCCESS(icu_status));
  if(!U_SUCCESS(icu_status)) {
    DebugLog("toUTF32 failed with %x", icu_status);
    delete[] dst;
    return nullptr;
  }

  dst[src.length()] = 0;
  return dst;
}
}}}
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

  /*
    Initialization
  */

  UErrorCode icu_status = U_ZERO_ERROR;
  const icu::Normalizer2 *nfc = icu::Normalizer2::getNFCInstance(icu_status);
  assert(U_SUCCESS(icu_status));
  if(!U_SUCCESS(icu_status)) {
    DebugLog("getNFCInstance failed with %x", icu_status);
    return false;
  }

  const icu::Normalizer2 *nfd = icu::Normalizer2::getNFDInstance(icu_status);
  assert(U_SUCCESS(icu_status));
  if(!U_SUCCESS(icu_status)) {
    DebugLog("getNFDInstance failed with %x", icu_status);
    return false;
  }

  icu::UnicodeString output = icu::UnicodeString::fromUTF32(reinterpret_cast<const UChar32*>(actions.output), -1);
  icu::UnicodeString cached_context_string = context_items_to_unicode_string(cached_context);
  icu::UnicodeString app_context_string = context_items_to_unicode_string(app_context);
  assert(!output.isBogus());
  assert(!cached_context_string.isBogus());
  assert(!app_context_string.isBogus());
  if(output.isBogus() || cached_context_string.isBogus() || app_context_string.isBogus()) {
    return false;
  }
  int nfu_to_delete = 0;

  /*
    Further debug assertion of inputs
  */

  assert(nfd->isNormalized(output, icu_status) && U_SUCCESS(icu_status));
  assert(nfd->isNormalized(cached_context_string, icu_status) && U_SUCCESS(icu_status));

  /*
    The keyboard processor will have updated the cached_context already,
    applying the transform to it, so we need to rewind this. Remove the output
    from cached_context_string to start
  */

  assert(cached_context_string.length() >= output.length());
  int n = cached_context_string.length() - output.length();
  assert(cached_context_string.compare(n, output.length(), output) == 0);
  cached_context_string.remove(n);

  /*
    While cached_context is guaranteed to be normalized, actions->output may not
    start at a normalization boundary. In order to achieve the correct NFC
    normalization in our output, we now need to look for a normalization
    boundary prior to the intersection of the cached_context and the output.
  */
  if(!output.isEmpty()) {
    while(n > 0 && !nfd->hasBoundaryBefore(output[0])) {
      // The output may interact with the context further in normalization. We
      // need to copy characters back further until we reach a normalization
      // boundary.

      // Remove last code point from the context ...

      n = cached_context_string.moveIndex32(n, -1);
      UChar32 chr = cached_context_string.char32At(n);
      cached_context_string.remove(n);

      // And prepend it to the output ...

      output.insert(0, chr);
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

  while(app_context_string.countChar32()) {
    icu::UnicodeString app_context_nfd;
    nfd->normalize(app_context_string, app_context_nfd, icu_status);
    assert(U_SUCCESS(icu_status));
    if(!U_SUCCESS(icu_status)) {
      DebugLog("nfd->normalize failed with %x", icu_status);
      return false;
    }

    if(app_context_nfd.compare(cached_context_string) == 0) {
      break;
    }

    // remove the last UChar32
    int32_t lastUChar32 = app_context_string.length()-1;
    // adjust pointer to get the entire char (i.e. so we don't slice a non-BMP char)
    lastUChar32 = app_context_string.getChar32Start(lastUChar32);
    // remove the UChar32 (1 or 2 code units)
    app_context_string.remove(lastUChar32);
    nfu_to_delete++;
  }

  /*
    Normalize our output string
  */

  icu::UnicodeString output_nfc;
  nfc->normalize(output, output_nfc, icu_status);
  assert(U_SUCCESS(icu_status));
  if(!U_SUCCESS(icu_status)) {
    DebugLog("nfc->normalize failed with %x", icu_status);
    return false;
  }

  auto new_output = km::core::util::unicode_string_to_usv(output_nfc);
  if(!new_output) {
    // error logging handled in unicode_string_to_usv
    return false;
  }

  /*
    Final steps -- set our outputs
  */

  // Append the new NFC output to our reduced app_context

  app_context_string.append(output_nfc);
  km_core_context_item *app_context_items = nullptr;
  km_core_status status = KM_CORE_STATUS_OK;
  if((status = context_items_from_utf16(app_context_string.getTerminatedBuffer(), &app_context_items)) != KM_CORE_STATUS_OK) {
    DebugLog("context_items_from_utf16 failed with %x", status);
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
icu::UnicodeString context_items_to_unicode_string(km::core::context const *context) {
  icu::UnicodeString nullString;
  nullString.setToBogus();

  km_core_context_item *items = nullptr;
  km_core_status status;
  if((status = km_core_context_get(static_cast<km_core_context const *>(context), &items)) != KM_CORE_STATUS_OK) {
    DebugLog("Failed to retrieve context with %s", status);
    return nullString;
  }
  size_t buf_size = 0;
  if((status = context_items_to_utf32(items, nullptr, &buf_size)) != KM_CORE_STATUS_OK) {
    DebugLog("Failed to retrieve context size with %s", status);
    km_core_context_items_dispose(items);
    return nullString;
  }

  km_core_usv *buf = new km_core_usv[buf_size];
  if((status = context_items_to_utf32(items, buf, &buf_size)) != KM_CORE_STATUS_OK) {
    DebugLog("Failed to retrieve context with %s", status);
    km_core_context_items_dispose(items);
    delete [] buf;
    return nullString;
  }

  auto result = icu::UnicodeString::fromUTF32(reinterpret_cast<const UChar32*>(buf), -1);
  km_core_context_items_dispose(items);
  delete [] buf;
  return result;
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
