// Internal data structures used in the implementation of keymanutil methods
// and exposed for unit testing.

#ifndef __KEYMANUTIL_INTERNAL_H__
#define __KEYMANUTIL_INTERNAL_H__

#include <gmodule.h>
#include "kmpdetails.h"

typedef struct {
  GList *engines_list;
  kmp_info *info;
  gchar *kmp_dir;
} add_keyboard_data;

IBusEngineDesc *ibus_keyman_engine_desc_new(
    gchar *file_name,
    gchar *name,
    gchar *description,
    gchar *copyright,
    gchar *lang,
    gchar *license,
    gchar *author,
    gchar *icon,
    gchar *layout,
    gchar *version);

IBusEngineDesc *get_engine_for_language(
    kmp_keyboard *keyboard,
    kmp_info *info,
    keyboard_details *kbd_details,
    gchar *kmp_dir,
    gchar *lang_id,
    gchar *lang_name);

void keyman_add_keyboard(gpointer data, gpointer user_data);
void keyman_add_keyboards_from_dir(gpointer data, gpointer user_data);

#endif  // __KEYMANUTIL_INTERNAL_H__
