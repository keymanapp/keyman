/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2009-2023 SIL International
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

#include <ibus.h>
#include <string.h>
#include <stdio.h>

#include <gdk/gdk.h>
#include <gio/gio.h>

#include <keyman/keyman_core_api.h>
#include <keyman/keyman_core_api_consts.h>

#include "config.h"
#include "keymanutil.h"
#include "keyman-service.h"
#include "KeymanSystemServiceClient.h"
#include "engine.h"
#include "keycodes.h"

// Fallback for older ibus versions that don't define IBUS_PREFILTER_MASK
#ifndef IBUS_HAS_PREFILTER
#warning Compiling against ibus version that does not include prefilter mask patch (https://github.com/ibus/ibus/pull/2440). Output ordering guarantees will be disabled.
#define IBUS_PREFILTER_MASK (1 << 23)
#endif

#define MAXCONTEXT_ITEMS 128
#define KEYMAN_BACKSPACE 14
#define KEYMAN_BACKSPACE_KEYSYM  IBUS_KEY_BackSpace
#define KEYMAN_LCTRL 29 // 0x1D
#define KEYMAN_LALT  56 // 0x38
#define KEYMAN_RCTRL 97 // 0x61
#define KEYMAN_RALT 100 // 0x64
#define KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL  202
#define KEYMAN_NOCHAR_KEYSYM (0xfdd0 | 0x1000000) // Unicode NOCHAR

typedef struct _IBusKeymanEngine IBusKeymanEngine;
typedef struct _IBusKeymanEngineClass IBusKeymanEngineClass;

#define MAX_QUEUE_SIZE 100

typedef struct _commit_queue_item {
  gchar *char_buffer;
  gboolean emitting_keystroke;
  guint keyval;
  guint keycode;
  guint state;
} commit_queue_item;

struct _IBusKeymanEngine {
  IBusEngine parent;

  /* members */
  km_core_keyboard *keyboard;
  km_core_state    *state;
  gchar           *ldmlfile;
  gchar           *kb_name;
  gboolean         lctrl_pressed;
  gboolean         rctrl_pressed;
  gboolean         lalt_pressed;
  gboolean         ralt_pressed;
  IBusLookupTable *table;
  IBusProperty    *status_prop;
  IBusPropList    *prop_list;

  commit_queue_item commit_queue[MAX_QUEUE_SIZE];
  commit_queue_item *commit_item;
  guint             consecutive_backspaces;
};

struct _IBusKeymanEngineClass {
  IBusEngineClass parent;
};

/* functions prototype */
static void ibus_keyman_engine_class_init (IBusKeymanEngineClass  *klass);
static void ibus_keyman_engine_init       (IBusKeymanEngine       *keyman);
static GObject* ibus_keyman_engine_constructor
                                          (GType                   type,
                                           guint                   n_construct_params,
                                           GObjectConstructParam  *construct_params);
static void ibus_keyman_engine_destroy    (IBusKeymanEngine       *keyman);
static gboolean ibus_keyman_engine_process_key_event
                                          (IBusEngine             *engine,
                                           guint                   keyval,
                                           guint                   keycode,
                                           guint                   state);
static void ibus_keyman_engine_focus_in   (IBusEngine             *engine);
static void ibus_keyman_engine_focus_out  (IBusEngine             *engine);
static void ibus_keyman_engine_reset      (IBusEngine             *engine);
static void ibus_keyman_engine_enable     (IBusEngine             *engine);
static void ibus_keyman_engine_disable    (IBusEngine             *engine);
static void ibus_keyman_engine_set_surrounding_text
                                          (IBusEngine             *engine,
                                           IBusText               *text,
                                           guint                   cursor_pos,
                                           guint                   anchor_pos);
// static void ibus_keyman_engine_set_cursor_location
//                                           (IBusEngine             *engine,
//                                            guint                   x,
//                                            guint                   y,
//                                            guint                   w,
//                                            guint                   h);
// static void ibus_keyman_engine_set_capabilities
//                                           (IBusEngine             *engine,
//                                            guint                   caps);
// static void ibus_keyman_engine_page_up    (IBusEngine             *engine);
// static void ibus_keyman_engine_page_down  (IBusEngine             *engine);
// static void ibus_keyman_engine_cursor_up  (IBusEngine             *engine);
// static void ibus_keyman_engine_cursor_down(IBusEngine             *engine);
// static void ibus_keyman_engine_property_activate
//                                           (IBusEngine             *engine,
//                                            const gchar            *prop_name,
//                                            guint                   prop_state);
// static void ibus_keyman_engine_property_show
//                                           (IBusEngine             *engine,
//                                            const gchar            *prop_name);
// static void ibus_keyman_engine_property_hide
//                                           (IBusEngine             *engine,
//                                            const gchar            *prop_name);

static void commit_string(IBusKeymanEngine *keyman, const gchar *string);

static IBusEngineClass *parent_class = NULL;

GType
ibus_keyman_engine_get_type (void)
{
  static GType type = 0;

  static const GTypeInfo type_info = {
    sizeof (IBusKeymanEngineClass),
    (GBaseInitFunc)  NULL,
    (GBaseFinalizeFunc) NULL,
    (GClassInitFunc) ibus_keyman_engine_class_init,
    NULL,
    NULL,
    sizeof (IBusKeymanEngine),
    0,
    (GInstanceInitFunc) ibus_keyman_engine_init,
  };

  if (type == 0) {
    type = g_type_register_static (IBUS_TYPE_ENGINE,
                     "IBusKeymanEngine",
                     &type_info,
                     (GTypeFlags) 0);
  }

  return type;
}

static void
ibus_keyman_engine_class_init (IBusKeymanEngineClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    IBusObjectClass *ibus_object_class = IBUS_OBJECT_CLASS (klass);
    IBusEngineClass *engine_class = IBUS_ENGINE_CLASS (klass);

    parent_class = (IBusEngineClass *) g_type_class_peek_parent (klass);

    object_class->constructor = ibus_keyman_engine_constructor;
    ibus_object_class->destroy = (IBusObjectDestroyFunc) ibus_keyman_engine_destroy;

    engine_class->process_key_event = ibus_keyman_engine_process_key_event;

    engine_class->reset = ibus_keyman_engine_reset;
    engine_class->enable = ibus_keyman_engine_enable;
    engine_class->disable = ibus_keyman_engine_disable;

    engine_class->set_surrounding_text = ibus_keyman_engine_set_surrounding_text;
    // engine_class->set_cursor_location = ibus_keyman_engine_set_cursor_location;


    engine_class->focus_in = ibus_keyman_engine_focus_in;
    engine_class->focus_out = ibus_keyman_engine_focus_out;

    // engine_class->page_up = ibus_keyman_engine_page_up;
    // engine_class->page_down = ibus_keyman_engine_page_down;

    // engine_class->cursor_up = ibus_keyman_engine_cursor_up;
    // engine_class->cursor_down = ibus_keyman_engine_cursor_down;

    // engine_class->property_activate = ibus_keyman_engine_property_activate;
}

static gchar *
get_context_debug(IBusEngine *engine, km_core_context *context) {
  IBusKeymanEngine *keyman       = (IBusKeymanEngine *)engine;
  km_core_context *state_context = context ? context : km_core_state_context(keyman->state);

  size_t buf_size8 = 512, buf_size16 = 512;
  km_core_context_item *context_items;
  gchar *current_context_utf8 = g_new0(gchar, buf_size8);
  km_core_cp *current_context_utf16 = g_new0(km_core_cp, buf_size16);
  if (km_core_context_get(state_context, &context_items) == KM_CORE_STATUS_OK) {
    km_core_context_items_to_utf8(context_items, current_context_utf8, &buf_size8);
    km_core_context_items_to_utf16(context_items, current_context_utf16, &buf_size16);
  }
  GString *output = g_string_new("");
  g_string_append_printf(output, "|%s| (len:%zu) [", current_context_utf8, km_core_context_length(state_context));
  for (int i = 0; i <  buf_size16 - 1; i++) {
    g_string_append_printf(output, "U+%04x ", current_context_utf16[i]);
  }
  g_string_append(output, "]");
  km_core_context_items_dispose(context_items);
  g_free(current_context_utf16);
  g_free(current_context_utf8);
#if GLIB_CHECK_VERSION(2, 76, 0)
  return g_string_free_and_steal(output);
#else
  return g_string_free(output, FALSE);
#endif
}

static gboolean
client_supports_prefilter(IBusEngine *engine)
{
  g_assert(engine != NULL);
#ifdef IBUS_HAS_PREFILTER
  return (engine->client_capabilities & IBUS_CAP_PREFILTER) != 0;
#else
  return FALSE;
#endif
}

static gboolean
client_supports_surrounding_text(IBusEngine *engine) {
  g_assert(engine != NULL);
  return (engine->client_capabilities & IBUS_CAP_SURROUNDING_TEXT) != 0;
}

static void
set_context_if_needed(IBusEngine *engine) {
  IBusKeymanEngine *keyman = (IBusKeymanEngine *)engine;

  if (!client_supports_surrounding_text(engine)) {
    g_message("%s: not a compliant client app", __FUNCTION__);
    return;
  }

  IBusText *text;
  g_autofree gchar *application_context_utf8 = NULL;
  guint cursor_pos, anchor_pos, context_start, context_end;

  g_autofree gchar *debug_context = NULL;
  g_message("%s: current core context   : %s", __FUNCTION__, debug_context = get_context_debug(engine, NULL));

  ibus_engine_get_surrounding_text(engine, &text, &cursor_pos, &anchor_pos);

  context_end         = anchor_pos < cursor_pos ? anchor_pos : cursor_pos;
  context_start       = context_end > MAXCONTEXT_ITEMS ? context_end - MAXCONTEXT_ITEMS : 0;
  application_context_utf8 = g_utf8_substring(ibus_text_get_text(text), context_start, context_end);
  g_message("%s: new application context: |%s| (len:%u) cursor:%d anchor:%d", __FUNCTION__,
    application_context_utf8, context_end - context_start, cursor_pos, anchor_pos);

  km_core_cp *application_context_utf16 = g_utf8_to_utf16(application_context_utf8, -1, NULL, NULL, NULL);
  km_core_context_status result;
  result = km_core_state_context_set_if_needed(keyman->state, application_context_utf16);
  g_free(application_context_utf16);

  g_message("%s: context %s", __FUNCTION__,
    result == KM_CORE_CONTEXT_STATUS_UNCHANGED ? "unchanged"
    : result == KM_CORE_CONTEXT_STATUS_UPDATED ? "updated"
    : result == KM_CORE_CONTEXT_STATUS_CLEARED ? "cleared"
    : result == KM_CORE_CONTEXT_STATUS_ERROR   ? "error"
                                               : "invalid argument");
}

static void
initialize_queue(IBusKeymanEngine *keyman, int index, int count) {
  g_assert(keyman != NULL);
  g_assert(index >= 0 && index < MAX_QUEUE_SIZE);
  g_assert(count > 0 && count <= MAX_QUEUE_SIZE);
  g_assert(index + count <= MAX_QUEUE_SIZE);
  memset(&keyman->commit_queue[index], 0, sizeof(commit_queue_item) * count);
}

static void
ibus_keyman_engine_init(IBusKeymanEngine *keyman) {
  gdk_init(NULL, NULL);
  keyman->status_prop = ibus_property_new("status", PROP_TYPE_NORMAL, NULL, NULL, NULL, TRUE, FALSE, 0, NULL);
  g_object_ref_sink(keyman->status_prop);
  keyman->prop_list = ibus_prop_list_new();
  g_object_ref_sink(keyman->prop_list);
  ibus_prop_list_append(keyman->prop_list, keyman->status_prop);

  keyman->state = NULL;
}

static km_core_cp* get_base_layout()
{
  return u"en-US";

#if 0  // in the future when mnemonic layouts are to be supported
  const gchar *lang_env = g_getenv("LANG");
  gchar *lang;
  if (lang_env != NULL) {
    g_message("LANG=%s", lang_env);
    gchar **splitlang = g_strsplit(lang_env, ".", 2);
    g_message("before . is %s", splitlang[0]);
    if (g_strrstr(splitlang[0], "_")) {
      g_message("splitting %s", splitlang[0]);
      gchar **taglang = g_strsplit(splitlang[0], "_", 2);
      g_message("lang of tag is %s", taglang[0]);
      g_message("country of tag is %s", taglang[1]);
      lang = g_strjoin("-", taglang[0], taglang[1], NULL);
      g_strfreev(taglang);
    }
    else {
      lang = g_strdup(splitlang[0]);
    }
    g_strfreev(splitlang);
  }
  else {
    lang = strdup("en-US");
  }
  g_message("lang is %s", lang);
  km_core_cp *cp = g_utf8_to_utf16(lang, -1, NULL, NULL, NULL);
  return cp;
  // g_free(lang);
#endif
}

static km_core_status
setup_environment(IBusKeymanEngine *keyman)
{
  g_assert(keyman);
  g_message("%s: setting up environment", __FUNCTION__);

  // Allocate enough options for: 3 environments plus 1 pad struct of 0's
  km_core_option_item environment_opts[4] = {0};

  environment_opts[0].scope = KM_CORE_OPT_ENVIRONMENT;
  environment_opts[0].key   = KM_CORE_KMX_ENV_PLATFORM;
  environment_opts[0].value = u"linux desktop hardware native";

  environment_opts[1].scope = KM_CORE_OPT_ENVIRONMENT;
  environment_opts[1].key   = KM_CORE_KMX_ENV_BASELAYOUT;
  environment_opts[1].value = u"kbdus.dll";

  environment_opts[2].scope = KM_CORE_OPT_ENVIRONMENT;
  environment_opts[2].key   = KM_CORE_KMX_ENV_BASELAYOUTALT;
  environment_opts[2].value = get_base_layout();  // TODO: free when mnemonic layouts are to be supported


  km_core_status status = km_core_state_create(keyman->keyboard, environment_opts, &(keyman->state));
  if (status != KM_CORE_STATUS_OK) {
    g_warning("%s: problem creating km_core_state. Status is %u.", __FUNCTION__, status);
  }
  return status;
}

void
free_km_core_option_item(gpointer data) {
  if (!data)
    return;

  km_core_option_item *opt = (km_core_option_item *)data;
  g_free((km_core_cp *)opt->key);
  g_free((km_core_cp *)opt->value);
  g_free(opt);
}

static km_core_status
load_keyboard_options(IBusKeymanEngine *keyman) {
  g_assert(keyman);

  // Retrieve keyboard options from DConf
  // TODO: May need unique packageID and keyboard ID
  g_message("%s: Loading options for kb_name: %s", __FUNCTION__, keyman->kb_name);
  GQueue *queue_options = keyman_get_options_queue_fromdconf(keyman->kb_name, keyman->kb_name);
  int num_options       = g_queue_get_length(queue_options);
  if (num_options < 1) {
    g_queue_free_full(queue_options, free_km_core_option_item);
    return KM_CORE_STATUS_OK;
  }

  // Allocate enough options for: num_options plus 1 pad struct of 0's
  km_core_option_item *keyboard_opts = g_new0(km_core_option_item, num_options + 1);

  for (int i = 0; i < num_options; i++) {
    km_core_option_item *item = g_queue_pop_head(queue_options);
    keyboard_opts[i].scope = item->scope;
    keyboard_opts[i].key   = item->key;
    keyboard_opts[i].value = item->value;
  }

  // once we have the option list we can then update the options using the public api call
  km_core_status status = km_core_state_options_update(keyman->state, keyboard_opts);

  if (status != KM_CORE_STATUS_OK) {
    g_warning("%s: problem creating km_core_state. Status is %u.", __FUNCTION__, status);
  }
  g_queue_free_full(queue_options, free_km_core_option_item);
  g_free(keyboard_opts);
  return status;
}

static GObject*
ibus_keyman_engine_constructor(
  GType type,
  guint n_construct_params,
  GObjectConstructParam *construct_params
) {
    IBusKeymanEngine *keyman;
    IBusEngine *engine;
    const gchar *engine_name;
    gchar *p;
    g_autofree gchar *abs_kmx_path = NULL;

    g_debug("DAR: %s", __FUNCTION__);

    keyman = (IBusKeymanEngine *) G_OBJECT_CLASS (parent_class)->constructor (type,
                                                       n_construct_params,
                                                       construct_params);

    engine = (IBusEngine *) keyman;
    engine_name = ibus_engine_get_name (engine);
    g_assert (engine_name);
    g_message("DAR: %s %s", __FUNCTION__, engine_name);

    keyman->kb_name = NULL;
    keyman->ldmlfile = NULL;
    keyman->lalt_pressed = FALSE;
    keyman->lctrl_pressed = FALSE;
    keyman->ralt_pressed = FALSE;
    keyman->rctrl_pressed = FALSE;
    keyman->consecutive_backspaces = 0;
    initialize_queue(keyman, 0, MAX_QUEUE_SIZE);
    keyman->commit_item    = &keyman->commit_queue[0];
    gchar **split_name     = g_strsplit(engine_name, ":", 2);
    if (split_name[0] == NULL)
    {
        IBUS_OBJECT_CLASS (parent_class)->destroy ((IBusObject *)keyman);
        return NULL;
    }
    else if (split_name[1] == NULL)
    {
        abs_kmx_path = g_strdup(split_name[0]);
    }
    else
    {
        abs_kmx_path = g_strdup(split_name[1]);
    }

    g_strfreev(split_name);

    g_autofree gchar *kmx_file = g_path_get_basename(abs_kmx_path);
    p = rindex(kmx_file, '.'); // get id to use as dbus service name
    if (p) {
        keyman->kb_name = g_strndup(kmx_file, p-kmx_file);
        p = rindex(abs_kmx_path, '.');
        if (p)
        {
            g_autofree gchar *dir = g_path_get_dirname(abs_kmx_path);
            g_autofree gchar *ldmlfile = g_strdup_printf("%s/%s.ldml", dir, keyman->kb_name);
            if (g_file_test(ldmlfile, G_FILE_TEST_EXISTS))
            {
                keyman->ldmlfile = g_strdup(ldmlfile);
            }
        }
    }

    km_core_status status;

    status = km_core_keyboard_load(abs_kmx_path, &(keyman->keyboard));

    if (status != KM_CORE_STATUS_OK) {
      g_warning("%s: problem creating km_core_keyboard. Status is %u.", __FUNCTION__, status);
      ibus_keyman_engine_destroy(keyman);
      return NULL;
    }

    status = setup_environment(keyman);
    if (status != KM_CORE_STATUS_OK) {
      ibus_keyman_engine_destroy(keyman);
      return NULL;
    }

    status = load_keyboard_options(keyman);
    if (status != KM_CORE_STATUS_OK) {
      ibus_keyman_engine_destroy(keyman);
      return NULL;
    }

    set_context_if_needed(engine);

    return (GObject *) keyman;
}


static void
ibus_keyman_engine_destroy (IBusKeymanEngine *keyman)
{
    const gchar *engine_name;

    g_debug("DAR: %s", __FUNCTION__);
    engine_name = ibus_engine_get_name ((IBusEngine *) keyman);
    g_assert (engine_name);
    g_message("DAR: %s %s", __FUNCTION__, engine_name);

    if (keyman->prop_list) {
        g_debug("DAR: %s: unref keyman->prop_list", __FUNCTION__);
        g_object_unref (keyman->prop_list);
        keyman->prop_list = NULL;
    }

    if (keyman->status_prop) {
        g_debug("DAR: %s: unref keyman->status_prop", __FUNCTION__);
        g_object_unref (keyman->status_prop);
        keyman->status_prop = NULL;
    }

    if (keyman->state) {
        km_core_state_dispose(keyman->state);
        keyman->state = NULL;
    }

    if (keyman->keyboard) {
        km_core_keyboard_dispose(keyman->keyboard);
        keyman->keyboard = NULL;
    }

    g_free(keyman->kb_name);
    g_free(keyman->ldmlfile);

    IBUS_OBJECT_CLASS (parent_class)->destroy ((IBusObject *)keyman);
}

void ibus_keyman_set_text(IBusEngine *engine, const gchar *text)
{
    IBusKeymanEngine *keyman = (IBusKeymanEngine *)engine;
    if (!keyman) {
        g_error("%s: parameter `engine` is not an `IBusKeymanEngine` (%p)", __FUNCTION__, engine);
        return;
    }
    commit_string(keyman, text);
}

static void commit_string(IBusKeymanEngine *keyman, const gchar *string)
{
    IBusText *text;
    g_message("DAR: %s - %s", __FUNCTION__, string);
    text = ibus_text_new_from_static_string (string);
    g_object_ref_sink(text);
    ibus_engine_commit_text ((IBusEngine *)keyman, text);
    g_object_unref (text);
}

static void forward_backspace(IBusKeymanEngine *keyman, unsigned int state)
{
    g_message("DAR: %s: forward_backspace %d no keysym state %d", __FUNCTION__, KEYMAN_BACKSPACE, state);
    ibus_engine_forward_key_event((IBusEngine *)keyman, KEYMAN_BACKSPACE_KEYSYM, KEYMAN_BACKSPACE, state);
}

static gboolean
process_unicode_char_action(
  IBusKeymanEngine *keyman,
  const km_core_action_item *action_item
) {
  g_assert(g_unichar_type(action_item->character) != G_UNICODE_SURROGATE);
  gchar *utf8   = (gchar *)g_new0(gchar, 12);
  gint numbytes = g_unichar_to_utf8(action_item->character, utf8);
  if (numbytes > 12) {
    g_error("%s: g_unichar_to_utf8 overflowing buffer", __FUNCTION__);
    g_free(utf8);
  } else {
    g_message("%s: unichar:U+%04x, bytes:%d, string:%s", __FUNCTION__, action_item->character, numbytes, utf8);
    if (keyman->commit_item->char_buffer == NULL) {
      g_message("%s: setting buffer to converted unichar", __FUNCTION__);
      keyman->commit_item->char_buffer = utf8;
    } else {
      g_message("%s: appending converted unichar to CHAR buffer", __FUNCTION__);
      gchar *new_buffer = g_strjoin("", keyman->commit_item->char_buffer, utf8, NULL);
      g_free(keyman->commit_item->char_buffer);
      g_free(utf8);
      keyman->commit_item->char_buffer = new_buffer;
    }
    g_message("%s: CHAR buffer is now %s", __FUNCTION__, keyman->commit_item->char_buffer);
  }
  return TRUE;
}

static gboolean process_alert_action() {
  GdkDisplay *display = gdk_display_open(NULL);
  if (display != NULL) {
    gdk_display_beep(display);
    gdk_display_close(display);
  }
  return TRUE;
}

static gboolean
process_backspace_action(
  IBusEngine *engine,
  const km_core_action_item *action_items,
  int i,
  size_t num_action_items
) {
  IBusKeymanEngine *keyman = (IBusKeymanEngine *)engine;
  if (action_items[i].backspace.expected_type == KM_CORE_IT_MARKER) {
    g_message("%s: skipping marker type", __FUNCTION__);
  } else if (keyman->commit_item->char_buffer != NULL) {
    g_message("%s: removing one utf8 char from CHAR buffer", __FUNCTION__);
    glong end_pos = g_utf8_strlen(keyman->commit_item->char_buffer, -1);
    gchar *new_buffer;
    if (end_pos == 1) {
      new_buffer = NULL;
      g_message("%s: resetting CHAR buffer to NULL", __FUNCTION__);
    } else {
      new_buffer = g_utf8_substring(keyman->commit_item->char_buffer, 0, end_pos - 1);
      g_message("%s: changing CHAR buffer to :%s:", __FUNCTION__, new_buffer);
    }
    if (g_strcmp0(keyman->commit_item->char_buffer, new_buffer) == 0) {
      g_message("%s: oops, CHAR buffer hasn't changed", __FUNCTION__);
    }
    g_free(keyman->commit_item->char_buffer);
    keyman->commit_item->char_buffer = new_buffer;
  } else {
    g_message(
        "DAR: %s - client_capabilities=%x, %x", __FUNCTION__, engine->client_capabilities, IBUS_CAP_SURROUNDING_TEXT);

    if (client_supports_surrounding_text(engine)) {
      keyman->consecutive_backspaces++;
      g_message("%s: compliant app: increment consecutive backspaces to %d", __FUNCTION__, keyman->consecutive_backspaces);
    } else {
      g_message("%s: non-compliant app: forwarding backspace", __FUNCTION__);
      forward_backspace(keyman, 0);
    }
  }
  return TRUE;
}

static gboolean
finish_backspace_action(
  IBusEngine *engine
) {
  IBusKeymanEngine *keyman = (IBusKeymanEngine *)engine;
  if (keyman->consecutive_backspaces <= 0)
    return TRUE;

  g_assert(client_supports_surrounding_text(engine));
  g_message("%s: deleting surrounding text %d char", __FUNCTION__, keyman->consecutive_backspaces);
  ibus_engine_delete_surrounding_text(engine, -keyman->consecutive_backspaces, keyman->consecutive_backspaces);
  keyman->consecutive_backspaces = 0;
  return TRUE;
}

static gboolean
process_persist_action(
  IBusKeymanEngine *keyman,
  const km_core_action_item *action_item
) {
  // Save keyboard option
  if (!action_item->option)
    return TRUE;

  // Allocate for 1 option plus 1 pad struct of 0's
  km_core_option_item *keyboard_opts = g_new0(km_core_option_item, 2);
  memmove(&(keyboard_opts[0]), action_item->option, sizeof(km_core_option_item));
  km_core_status event_status = km_core_state_options_update(keyman->state, keyboard_opts);
  if (event_status != KM_CORE_STATUS_OK) {
    g_warning("%s: problem saving option for km_core_keyboard", __FUNCTION__);
  }
  g_free(keyboard_opts);

  // Put the keyboard option into DConf
  if (action_item->option->key != NULL && action_item->option->value != NULL) {
    g_message("%s: Saving keyboard option to DConf", __FUNCTION__);
    // Load the current keyboard options from DConf
    keyman_put_options_todconf(
        keyman->kb_name, keyman->kb_name, (gchar *)action_item->option->key, (gchar *)action_item->option->value);
  }
  return TRUE;
}

static gboolean
process_emit_keystroke_action(IBusKeymanEngine *keyman) {
  IBusEngine *engine = (IBusEngine *)keyman;
  if ((!client_supports_prefilter(engine) || client_supports_surrounding_text(engine)) &&
      keyman->commit_item->char_buffer != NULL) {
    commit_string(keyman, keyman->commit_item->char_buffer);
    g_free(keyman->commit_item->char_buffer);
    keyman->commit_item->char_buffer = NULL;
  }
  keyman->commit_item->emitting_keystroke = TRUE;
  return TRUE;
}

static gboolean
process_capslock_action(
  IBusKeymanEngine *keyman,
  const km_core_action_item *action_item
) {
  g_message("%s: %s caps-lock", __FUNCTION__, action_item->capsLock ? "Enable" : "Disable");

  set_capslock_indicator(action_item->capsLock);
  return TRUE;
}

static void
commit_text(IBusKeymanEngine *keyman) {
  g_assert(keyman != NULL);
  if (keyman->commit_item <= keyman->commit_queue)
    return;

  commit_queue_item *current_item = &keyman->commit_queue[0];
  if (current_item->char_buffer != NULL) {
    commit_string(keyman, current_item->char_buffer);
    g_free(current_item->char_buffer);
  }
  if (current_item->emitting_keystroke) {
    ibus_engine_forward_key_event((IBusEngine*)keyman, current_item->keyval, current_item->keycode, current_item->state);
  }
  keyman->commit_item--;
  memmove(keyman->commit_queue, &keyman->commit_queue[1], sizeof(commit_queue_item) * MAX_QUEUE_SIZE - 1);
  initialize_queue(keyman, MAX_QUEUE_SIZE - 1, 1);
}

static gboolean
process_end_action(IBusKeymanEngine *keyman) {
  g_assert(keyman != NULL);
  IBusEngine *engine = (IBusEngine *)keyman;
  if (client_supports_prefilter(engine) && !client_supports_surrounding_text(engine)) {
    guint state = keyman->commit_item->state;
    keyman->commit_item++;
    if (keyman->commit_item > &keyman->commit_queue[MAX_QUEUE_SIZE-1]) {
      g_error("Overflow of keyman commit_queue!");
      // TODO: log to Sentry
      keyman->commit_item--;
    }

    // Forward a fake key event to get the correct order of events so that any backspace key we
    // generated will be processed before the character we're adding. We need to send a
    // valid keyval/keycode combination so that it doesn't get swallowed by GTK but which
    // isn't very likely used in real keyboards. F24 seems to work for that.
    ibus_engine_forward_key_event((IBusEngine*)keyman,
      KEYMAN_NOCHAR_KEYSYM,
      KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL,
      (state & IBUS_RELEASE_MASK)
        ? IBUS_PREFILTER_MASK | IBUS_RELEASE_MASK
        : IBUS_PREFILTER_MASK);
  } else {
    if (keyman->commit_item->char_buffer != NULL) {
      commit_string(keyman, keyman->commit_item->char_buffer);
      g_free(keyman->commit_item->char_buffer);
      keyman->commit_item->char_buffer = NULL;
    }
    if (keyman->commit_item->emitting_keystroke) {
      keyman->commit_item->emitting_keystroke = FALSE;
      // We have an old ibus version without prefilter support, or a client that does support
      // surrounding text. In either case we return FALSE because we emitted a keystroke
      // so that the processing of the event will continue.
      return FALSE;
    }
  }

  // If we have a new ibus version that supports prefilter and a client that doesn't support
  // surrounding text (e.g. Chromium as of v104) we forwarded the key event with
  // IBUS_PREFILTER_MASK set and now return TRUE here to stop further processing.
  // With an old ibus version without prefilter support, or with a client that does support
  // surrounding text, we return TRUE if we completely processed the event and no further
  // processing should happen.
  return TRUE;
}

static gboolean
process_actions(
  IBusEngine *engine,
  const km_core_action_item *action_items,
  size_t num_action_items
) {
  IBusKeymanEngine *keyman = (IBusKeymanEngine *)engine;
  for (int i = 0; i < num_action_items; i++) {
    gboolean continue_with_next_action = TRUE;
    if (action_items[i].type != KM_CORE_IT_BACK && keyman->consecutive_backspaces > 0) {
      finish_backspace_action(engine);
    }
    switch (action_items[i].type) {
    case KM_CORE_IT_CHAR:
      g_message("CHAR action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_unicode_char_action(keyman, &action_items[i]);
      g_assert(continue_with_next_action == TRUE);
      break;
    case KM_CORE_IT_MARKER:
      g_message("MARKER action %d/%d", i + 1, (int)num_action_items);
      break;
    case KM_CORE_IT_ALERT:
      g_message("ALERT action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_alert_action();
      g_assert(continue_with_next_action == TRUE);
      break;
    case KM_CORE_IT_BACK:
      g_message("BACK action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_backspace_action(engine, action_items, i, num_action_items);
      g_assert(continue_with_next_action == TRUE);
      break;
    case KM_CORE_IT_PERSIST_OPT:
      g_message("PERSIST_OPT action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_persist_action(keyman, &action_items[i]);
      g_assert(continue_with_next_action == TRUE);
      break;
    case KM_CORE_IT_EMIT_KEYSTROKE:
      g_message("EMIT_KEYSTROKE action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_emit_keystroke_action(keyman);
      g_assert(continue_with_next_action == TRUE);
      break;
    case KM_CORE_IT_INVALIDATE_CONTEXT:
      g_message("INVALIDATE_CONTEXT action %d/%d", i + 1, (int)num_action_items);
      // no-op when context is maintained in core
      continue_with_next_action = TRUE;
      break;
    case KM_CORE_IT_CAPSLOCK:
      g_message("CAPSLOCK action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_capslock_action(keyman, &action_items[i]);
      g_assert(continue_with_next_action == TRUE);
      break;
    case KM_CORE_IT_END:
      g_message("END action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_end_action(keyman);
      break;
    default:
      g_warning("%s: Unknown action %d/%d(%d)", __FUNCTION__, i + 1, (int)num_action_items, action_items[i].type);
    }
    if (!continue_with_next_action)
      return FALSE;
  }
  return TRUE;
}

static gboolean
ibus_keyman_engine_process_key_event(
  IBusEngine *engine,
  guint keyval,
  guint keycode,
  guint state
) {
  IBusKeymanEngine *keyman = (IBusKeymanEngine *)engine;
  keyman->commit_item->keyval     = keyval;
  keyman->commit_item->keycode    = keycode;
  keyman->commit_item->state      = state;

  gboolean isKeyDown = !(state & IBUS_RELEASE_MASK);

  g_message("-----------------------------------------------------------------------------------------------------------------");
  g_message(
      "DAR: %s - keyval=0x%02x keycode=0x%02x, state=0x%02x, isKeyDown=%d, supports_prefilter=%d", __FUNCTION__, keyval, keycode,
      state, isKeyDown, client_supports_prefilter(engine));

  // This keycode is a fake keycode that we send when it's time to commit the text, ensuring the
  // correct output order of backspace and text.
  if (client_supports_prefilter(engine) && !client_supports_surrounding_text(engine) &&
      keycode == KEYMAN_F24_KEYCODE_OUTPUT_SENTINEL && (state & IBUS_PREFILTER_MASK)) {
    commit_text(keyman);
    return TRUE;
  }

  // REVIEW: why don't we handle these keys?
  switch (keycode) {
  case KEYMAN_LCTRL:
    keyman->lctrl_pressed = isKeyDown;
    break;
  case KEYMAN_RCTRL:
    keyman->rctrl_pressed = isKeyDown;
    break;
  case KEYMAN_LALT:
    keyman->lalt_pressed = isKeyDown;
    break;
  case KEYMAN_RALT:
    keyman->ralt_pressed = isKeyDown;
    break;
  }

  if (keycode < 0 || keycode > 255) {
    g_warning("%s: keycode %d out of range", __FUNCTION__, keycode);
    return FALSE;
  }

  if (keycode_to_vk[keycode] == 0) {  // key we don't handle
    return FALSE;
  }

  // keyman modifiers are different from X11/ibus
  uint16_t km_mod_state = 0;
  if (state & IBUS_SHIFT_MASK) {
    km_mod_state |= KM_CORE_MODIFIER_SHIFT;
  }
  if (state & IBUS_MOD5_MASK) {
    km_mod_state |= KM_CORE_MODIFIER_RALT;
    g_message("%s: modstate KM_CORE_MODIFIER_RALT from IBUS_MOD5_MASK", __FUNCTION__);
  }
  if (state & IBUS_MOD1_MASK) {
    if (keyman->ralt_pressed) {
      km_mod_state |= KM_CORE_MODIFIER_RALT;
      g_message("%s: modstate KM_CORE_MODIFIER_RALT from ralt_pressed", __FUNCTION__);
    }
    if (keyman->lalt_pressed) {
      km_mod_state |= KM_CORE_MODIFIER_LALT;
      g_message("%s: modstate KM_CORE_MODIFIER_LALT from lalt_pressed", __FUNCTION__);
    }
  }
  if (state & IBUS_CONTROL_MASK) {
    if (keyman->rctrl_pressed) {
      km_mod_state |= KM_CORE_MODIFIER_RCTRL;
      g_message("%s: modstate KM_CORE_MODIFIER_RCTRL from rctrl_pressed", __FUNCTION__);
    }
    if (keyman->lctrl_pressed) {
      km_mod_state |= KM_CORE_MODIFIER_LCTRL;
      g_message("%s: modstate KM_CORE_MODIFIER_LCTRL from lctrl_pressed", __FUNCTION__);
    }
  }
  if (state & IBUS_LOCK_MASK) {
    km_mod_state |= KM_CORE_MODIFIER_CAPS;
  }
  g_message("DAR: %s - km_mod_state=0x%x", __FUNCTION__, km_mod_state);
  g_autofree gchar *debug_context = NULL;
  g_message("%s: before process key event: %s", __FUNCTION__, debug_context = get_context_debug(engine, NULL));
  km_core_process_event(keyman->state, keycode_to_vk[keycode], km_mod_state, isKeyDown, KM_CORE_EVENT_FLAG_DEFAULT);
  g_message("%s: after process key event : %s", __FUNCTION__, debug_context = get_context_debug(engine, NULL));

  // km_core_state_action_items to get action items
  size_t num_action_items;
  g_free(keyman->commit_item->char_buffer);
  keyman->commit_item->char_buffer = NULL;
  const km_core_action_item *action_items = km_core_state_action_items(keyman->state, &num_action_items);

  if (!process_actions(engine, action_items, num_action_items) &&
      (!client_supports_prefilter(engine) || client_supports_surrounding_text(engine))) {
    // If we have an old ibus version without prefilter support, or a client that supports
    // surrounding text, and we forwarded a key event we want to return FALSE so that the
    // processing of the event continues.
    return FALSE;
  }

  g_message("%s: after processing all actions: %s", __FUNCTION__, debug_context = get_context_debug(engine, NULL));
  return TRUE;
}

static void
ibus_keyman_engine_set_surrounding_text (IBusEngine *engine,
                                            IBusText    *text,
                                            guint       cursor_pos,
                                            guint       anchor_pos)
{
    parent_class->set_surrounding_text(engine, text, cursor_pos, anchor_pos);
    set_context_if_needed(engine);
}

// static void ibus_keyman_engine_set_cursor_location (IBusEngine             *engine,
//                                              guint                    x,
//                                              guint                    y,
//                                              guint                    w,
//                                              guint                    h)
// {
//     g_message("ibus_keyman_engine_set_cursor_location");
//     //ibus_keyman_engine_reset(engine);
//     parent_class->set_cursor_location (engine, x, y, w, h);
// }

static void
ibus_keyman_engine_focus_in (IBusEngine *engine)
{
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;

    g_message("%s", __FUNCTION__);
    ibus_engine_register_properties (engine, keyman->prop_list);

    set_context_if_needed(engine);
    parent_class->focus_in (engine);
}

static void
ibus_keyman_engine_focus_out (IBusEngine *engine)
{
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;

    g_message("%s", __FUNCTION__);
    km_core_context_clear(km_core_state_context(keyman->state));
    parent_class->focus_out (engine);
}

static void
ibus_keyman_engine_reset (IBusEngine *engine)
{
    g_message("%s", __FUNCTION__);
    parent_class->reset (engine);
    set_context_if_needed(engine);
}



static void
ibus_keyman_engine_enable (IBusEngine *engine)
{
    const gchar *engine_name;
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;

    engine_name = ibus_engine_get_name (engine);
    g_assert (engine_name);
    g_message("WDG: %s %s: enabling surrounding context", __FUNCTION__, engine_name);
    ibus_engine_get_surrounding_text(engine, NULL, NULL, NULL);
    if (keyman->ldmlfile)
    {
        // own dbus name com.Keyman
        // expose properties LDMLFile and Name
        KeymanService *service = km_service_get_default(engine);
        km_service_set_ldmlfile (service, keyman->ldmlfile);
        km_service_set_name (service, keyman->kb_name);
    }
    parent_class->enable (engine);
}

static void
ibus_keyman_engine_disable (IBusEngine *engine)
{
    const gchar *engine_name;

    engine_name = ibus_engine_get_name (engine);
    g_assert (engine_name);
    g_message("WDG: %s %s", __FUNCTION__, engine_name);
    ibus_keyman_engine_focus_out (engine);
    // stop owning dbus name com.Keyman
    KeymanService *service = km_service_get_default(engine);
    km_service_set_ldmlfile (service, "");
    km_service_set_name (service, "None");
    // g_clear_object(&service);

    parent_class->disable (engine);
}

// static void
// ibus_keyman_engine_page_up (IBusEngine *engine)
// {
//     g_message("ibus_keyman_engine_page_up");
//     parent_class->page_up (engine);
//     set_context_if_needed(engine);
// }

// static void
// ibus_keyman_engine_page_down (IBusEngine *engine)
// {
//     g_message("ibus_keyman_engine_page_down");
//     parent_class->page_down (engine);
//     set_context_if_needed(engine);
// }

// static void
// ibus_keyman_engine_cursor_up (IBusEngine *engine)
// {
//     g_message("ibus_keyman_engine_cursor_up");
//     parent_class->cursor_up (engine);
//     set_context_if_needed(engine);
// }

// static void
// ibus_keyman_engine_cursor_down (IBusEngine *engine)
// {
//     g_message("ibus_keyman_engine_cursor_down");
//     parent_class->cursor_down (engine);
//     set_context_if_needed(engine);
// }

// static void
// ibus_keyman_engine_property_activate (IBusEngine  *engine,
//                                     const gchar *prop_name,
//                                     guint        prop_state)
// {
//     g_message("ibus_keyman_engine_property_activate");
//     parent_class->property_activate (engine, prop_name, prop_state);
// }
