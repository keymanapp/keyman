#include <gio/gio.h>
#include <unicode/locid.h>
#include <unicode/uloc.h>

/// Minimize the BCP-47 `tag` so that unnecessary parts get ommitted.
/// The result gets stored in `minimzedTag`.
///
/// @param tag           The tag to process
/// @param minimizedTag  Caller-provided character array for the
///                      resulting minimized tag.
/// @param tagCapacity   Array size of `minimizedTag`
/// @return The length of the minimized tag, or -1 in error case
int bcp47_minimize(const char* tag, char* minimizedTag, int tagCapacity) {
  UErrorCode status = U_ZERO_ERROR;
  if (!tag || strlen(tag) == 0) {
    strncpy(minimizedTag, "", tagCapacity);
    return -1;
  }

  // special treatment for `und-Latn` which is used by sil_ipa keyboard
  if (strcmp(tag, "und-Latn") == 0) {
    strncpy(minimizedTag, "und-Latn", tagCapacity);
    return strlen(minimizedTag);
  }

  int capacity = 255;
  char workingTag[capacity];

  // special treatment for tags that start with `und`: replace `und` with `en`.
  // ICU 70 doesn't properly treat `und`.
  int isUnd = strncmp(tag, "und", 3) == 0;
  if (isUnd) {
    strcpy(workingTag, "en");
    strncat(&workingTag[2], &tag[3], capacity - 3);
  } else {
    strncpy(workingTag, tag, capacity - 1);
  }
  workingTag[capacity - 1] = 0;

  char localeId[capacity];
  uloc_forLanguageTag(workingTag, localeId, capacity, NULL, &status);
  if (U_FAILURE(status)) {
    g_error("%s: uloc_forLanguageTag returned %0x", __FUNCTION__, status);
    return -1;
  }

  char minimizedLocaleId[capacity];
  uloc_minimizeSubtags(localeId, minimizedLocaleId, capacity, &status);
  if (U_FAILURE(status)) {
    g_error("%s: uloc_minimizeSubtags returned %0x", __FUNCTION__, status);
    return -1;
  }

  int taglen = uloc_toLanguageTag(minimizedLocaleId, minimizedTag, tagCapacity, FALSE, &status);
  if (U_FAILURE(status)) {
    g_error("%s: uloc_toLanguageTag returned %0x", __FUNCTION__, status);
    return -1;
  }

  if (isUnd) {
    // Replace 'en' with 'und' again
    strncpy(workingTag, &minimizedTag[2], capacity - 1);
    workingTag[capacity - 1] = 0;
    strcpy(minimizedTag, "und");
    strncat(minimizedTag, workingTag, tagCapacity - 4);
    minimizedTag[tagCapacity - 1] = 0;
    taglen = strlen(minimizedTag);
  }
  return taglen;
}

/// Extract the language code from the BCP-47 `tag`
///
/// @param tag        The BCP-47 tag
/// @param lang_code  Caller-provided character array that will receive
///                   the language code extracted from `tag`
/// @param capacity   Array size of `lang_code`
/// @return TRUE if successful, otherwise FALSE
int bcp47_get_language_code(const char* tag, char* lang_code, int capacity) {
  UErrorCode status = U_ZERO_ERROR;
  if (!tag || strlen(tag) == 0) {
    strncpy(lang_code, "", capacity);
    return FALSE;
  }

  // ICU 70 doesn't properly treat `und`
  if (strncmp(tag, "und", 3) == 0) {
    strncpy(lang_code, "und", capacity);
    return TRUE;
  }

  uloc_getLanguage(tag, lang_code, capacity, &status);
  if (U_FAILURE(status)) {
    g_error("%s: uloc_getLanguage returned %0x", __FUNCTION__, status);
    return FALSE;
  }

  return TRUE;
}
