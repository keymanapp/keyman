/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2009-2020 SIL International
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

#ifdef GDK_WINDOWING_X11
#include <gdk/gdkx.h>
#include <X11/XKBlib.h>
#endif

#ifdef GDK_WINDOWING_WAYLAND
#include <gdk/gdkwayland.h>
#endif

#include <keyman/keyboardprocessor.h>

#include "keymanutil.h"
#include "keyman-service.h"
#include "engine.h"
#include "keycodes.h"

#define MAXCONTEXT_ITEMS 128
#define KEYMAN_BACKSPACE 14
#define KEYMAN_BACKSPACE_KEYSYM  IBUS_KEY_BackSpace
#define KEYMAN_LCTRL 29 // 0x1D
#define KEYMAN_LALT  56 // 0x38
#define KEYMAN_RCTRL 97 // 0x61
#define KEYMAN_RALT 100 // 0x64

typedef struct _IBusKeymanEngine IBusKeymanEngine;
typedef struct _IBusKeymanEngineClass IBusKeymanEngineClass;

struct _IBusKeymanEngine {
	IBusEngine parent;

    /* members */
    km_kbp_keyboard *keyboard;
    km_kbp_state    *state;
    gchar           *ldmlfile;
    gchar           *kb_name;
    gchar           *char_buffer;
    gunichar         firstsurrogate;
    gboolean         lctrl_pressed;
    gboolean         rctrl_pressed;
    gboolean         lalt_pressed;
    gboolean         ralt_pressed;
    gboolean         emitting_keystroke;
    IBusLookupTable *table;
    IBusProperty    *status_prop;
    IBusPropList    *prop_list;
#ifdef GDK_WINDOWING_X11
    Display         *xdisplay;
#endif
#ifdef GDK_WINDOWING_WAYLAND
    GdkWaylandDisplay *wldisplay;
#endif
};

struct _IBusKeymanEngineClass {
	IBusEngineClass parent;
};

/* functions prototype */
static void	ibus_keyman_engine_class_init	    (IBusKeymanEngineClass    *klass);
static void	ibus_keyman_engine_init		    (IBusKeymanEngine		    *keyman);
static GObject*
            ibus_keyman_engine_constructor    (GType                   type,
                                             guint                   n_construct_params,
                                             GObjectConstructParam  *construct_params);
static void	ibus_keyman_engine_destroy		(IBusKeymanEngine		    *keyman);
static gboolean
			ibus_keyman_engine_process_key_event
                                            (IBusEngine             *engine,
                                             guint               	 keyval,
                                             guint               	 keycode,
                                             guint               	 state);
static void ibus_keyman_engine_focus_in       (IBusEngine             *engine);
static void ibus_keyman_engine_focus_out      (IBusEngine             *engine);
static void ibus_keyman_engine_reset          (IBusEngine             *engine);
static void ibus_keyman_engine_enable         (IBusEngine             *engine);
static void ibus_keyman_engine_disable        (IBusEngine             *engine);
static void ibus_keyman_engine_set_surrounding_text(
                                             IBusEngine               *engine,
                                             IBusText                  *text,
                                             guint                     cursor_pos,
                                             guint                     anchor_pos);
// static void ibus_keyman_engine_set_cursor_location (IBusEngine             *engine,
//                                              gint                    x,
//                                              gint                    y,
//                                              gint                    w,
//                                              gint                    h);
static void ibus_keyman_engine_set_capabilities(
                                            IBusEngine             *engine,
                                             guint                   caps);
// static void ibus_keyman_engine_page_up        (IBusEngine             *engine);
// static void ibus_keyman_engine_page_down      (IBusEngine             *engine);
// static void ibus_keyman_engine_cursor_up      (IBusEngine             *engine);
// static void ibus_keyman_engine_cursor_down    (IBusEngine             *engine);
static void ibus_keyman_engine_property_activate
                                            (IBusEngine             *engine,
                                             const gchar            *prop_name,
                                             guint                   prop_state);
static void ibus_keyman_engine_property_show
											(IBusEngine             *engine,
                                             const gchar            *prop_name);
static void ibus_keyman_engine_property_hide
											(IBusEngine             *engine,
                                             const gchar            *prop_name);

static void ibus_keyman_engine_commit_string
                                            (IBusKeymanEngine         *keyman,
                                             const gchar            *string);

static IBusEngineClass *parent_class = NULL;

GType
ibus_keyman_engine_get_type (void)
{
	static GType type = 0;

	static const GTypeInfo type_info = {
		sizeof (IBusKeymanEngineClass),
		(GBaseInitFunc)		NULL,
		(GBaseFinalizeFunc) NULL,
		(GClassInitFunc)	ibus_keyman_engine_class_init,
		NULL,
		NULL,
		sizeof (IBusKeymanEngine),
		0,
		(GInstanceInitFunc)	ibus_keyman_engine_init,
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

    engine_class->property_activate = ibus_keyman_engine_property_activate;
}

static gchar *get_current_context_text(km_kbp_context *context)
{
    size_t buf_size = 512;
    km_kbp_context_item *context_items;
    gchar *current_context_utf8 = g_new0(gchar, buf_size);
    if (km_kbp_context_get(context, &context_items) == KM_KBP_STATUS_OK) {
        km_kbp_context_items_to_utf8(context_items,
                            current_context_utf8,
                            &buf_size);
    }
    km_kbp_context_items_dispose(context_items);
    g_message("current context is:%lu:%lu:%s:", km_kbp_context_length(context), buf_size, current_context_utf8);
    return current_context_utf8;
}

static void reset_context(IBusEngine *engine)
{
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;
    IBusText *text;
    gchar *surrounding_text, *current_context_utf8;
    guint cursor_pos, anchor_pos, context_start, context_pos;
    km_kbp_context_item *context_items;
    km_kbp_context *context;

    g_message("reset_context");
    keyman->firstsurrogate = 0;
    context = km_kbp_state_context(keyman->state);
    if ((engine->client_capabilities & IBUS_CAP_SURROUNDING_TEXT) != 0)
    {
        current_context_utf8 = get_current_context_text(context);

        ibus_engine_get_surrounding_text(engine, &text, &cursor_pos, &anchor_pos);
        context_pos = anchor_pos < cursor_pos ? anchor_pos : cursor_pos;
        context_start = context_pos > MAXCONTEXT_ITEMS ? context_pos - MAXCONTEXT_ITEMS : 0;
        surrounding_text = g_utf8_substring(ibus_text_get_text(text), context_start, context_pos);
        g_message("new context is:%u:%s: cursor:%d anchor:%d", context_pos - context_start, surrounding_text, cursor_pos, anchor_pos);

        g_message(":%s:%s:", surrounding_text, current_context_utf8);
        if (!g_str_has_suffix(surrounding_text, current_context_utf8))
        {
            g_message("setting context because it has changed from expected");
            if (km_kbp_context_items_from_utf8(surrounding_text, &context_items) == KM_KBP_STATUS_OK) {
                km_kbp_context_set(context, context_items);
            }
            km_kbp_context_items_dispose(context_items);
        }
        g_free(surrounding_text);
        g_free(current_context_utf8);
    }
    else {
        km_kbp_context_clear(context);
    }
}

static void
ibus_keyman_engine_init(IBusKeymanEngine *keyman) {
  gdk_init(NULL, NULL);
  keyman->status_prop = ibus_property_new("status", PROP_TYPE_NORMAL, NULL, NULL, NULL, TRUE, FALSE, 0, NULL);
  g_object_ref_sink(keyman->status_prop);
  keyman->prop_list = ibus_prop_list_new();
  g_object_ref_sink(keyman->prop_list);
  ibus_prop_list_append(keyman->prop_list, keyman->status_prop);

  keyman->table = ibus_lookup_table_new(9, 0, TRUE, TRUE);
  g_object_ref_sink(keyman->table);
  keyman->state = NULL;
#ifdef GDK_WINDOWING_X11
  keyman->xdisplay = NULL;
#endif
#ifdef GDK_WINDOWING_WAYLAND
  keyman->wldisplay = NULL;
#endif

  GdkDisplay *gdkDisplay = gdk_display_get_default();
#ifdef GDK_WINDOWING_X11
  if (GDK_IS_X11_DISPLAY(gdkDisplay)) {
    g_debug("Using X11");
    keyman->xdisplay = GDK_DISPLAY_XDISPLAY(gdkDisplay);
  } else
#endif
#ifdef GDK_WINDOWING_WAYLAND
  if (GDK_IS_WAYLAND_DISPLAY(gdkDisplay)) {
    g_debug("Using Wayland");
    keyman->wldisplay = GDK_WAYLAND_DISPLAY(gdkDisplay);
  } else
#endif
  {
    g_error("unsupported GDK backend");
  }
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
    gchar *p, *abs_kmx_path;
    guint cursor_pos, anchor_pos;
    km_kbp_context_item *context_items;

    g_debug("DAR: ibus_keyman_engine_constructor");

    keyman = (IBusKeymanEngine *) G_OBJECT_CLASS (parent_class)->constructor (type,
                                                       n_construct_params,
                                                       construct_params);

    engine = (IBusEngine *) keyman;
    engine_name = ibus_engine_get_name (engine);
    g_assert (engine_name);
    g_message("DAR: ibus_keyman_engine_constructor %s", engine_name);

    keyman->kb_name = NULL;
    keyman->ldmlfile = NULL;
    keyman->firstsurrogate = 0;
    keyman->lalt_pressed = FALSE;
    keyman->lctrl_pressed = FALSE;
    keyman->ralt_pressed = FALSE;
    keyman->rctrl_pressed = FALSE;
    keyman->emitting_keystroke = FALSE;
    gchar **split_name = g_strsplit(engine_name, ":", 2);
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

    gchar *kmx_file = g_path_get_basename(abs_kmx_path);
    p = rindex(kmx_file, '.'); // get id to use as dbus service name
    if (p) {
        keyman->kb_name = g_strndup(kmx_file, p-kmx_file);
        p = rindex(abs_kmx_path, '.');
        if (p)
        {
            gchar *dir = g_path_get_dirname(abs_kmx_path);
            gchar *ldmlfile = g_strdup_printf("%s/%s.ldml", dir, keyman->kb_name);
            if (g_file_test(ldmlfile, G_FILE_TEST_EXISTS))
            {
                keyman->ldmlfile = g_strdup(ldmlfile);
            }
            g_free(dir);
            g_free(ldmlfile);
        }
    }
    g_free(kmx_file);

    // Retrieve keyboard options from DConf
    // TODO: May need unique packageID and keyboard ID
    g_message("Loading options for kb_name: %s", keyman->kb_name);
    GQueue *queue_options = keyman_get_options_queue_fromdconf(keyman->kb_name, keyman->kb_name);
    int num_options = g_queue_get_length(queue_options);

    // Allocate enough options for: 3 environments plus num_options plus 1 pad struct of 0's
    km_kbp_option_item *keyboard_opts = g_new0(km_kbp_option_item, KEYMAN_ENVIRONMENT_OPTIONS + num_options + 1);

    keyboard_opts[0].scope = KM_KBP_OPT_ENVIRONMENT;
    km_kbp_cp *cp = g_utf8_to_utf16 ("platform", -1, NULL, NULL, NULL);
    keyboard_opts[0].key = cp;
    cp = g_utf8_to_utf16 ("linux desktop hardware native", -1, NULL, NULL, NULL);
    keyboard_opts[0].value = cp;

    keyboard_opts[1].scope = KM_KBP_OPT_ENVIRONMENT;
    cp = g_utf8_to_utf16 ("baseLayout", -1, NULL, NULL, NULL);
    keyboard_opts[1].key = cp;
    cp = g_utf8_to_utf16 ("kbdus.dll", -1, NULL, NULL, NULL);
    keyboard_opts[1].value = cp;

    keyboard_opts[2].scope = KM_KBP_OPT_ENVIRONMENT;
    cp = g_utf8_to_utf16 ("baseLayoutAlt", -1, NULL, NULL, NULL);
    keyboard_opts[2].key = cp;
    #if 0 // in the future when mnemonic layouts are to be supported
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
    #endif
    cp = g_utf8_to_utf16 ("en-US", -1, NULL, NULL, NULL);
    // g_free(lang);
    keyboard_opts[2].value = cp;

    // If queue_options contains keyboard options, pop them into keyboard_opts[3] onward
    for(int i=0; i<num_options; i++)
    {
        memmove(&(keyboard_opts[KEYMAN_ENVIRONMENT_OPTIONS+i]), g_queue_pop_head(queue_options), sizeof(km_kbp_option_item));
    }

    // keyboard_opts[tail] already initialised to {0, 0, 0}

    km_kbp_status status_keyboard = km_kbp_keyboard_load(abs_kmx_path, &(keyman->keyboard));
    g_free(abs_kmx_path);

    if (status_keyboard != KM_KBP_STATUS_OK)
    {
        g_warning("problem creating km_kbp_keyboard");
    }

    km_kbp_status status_state = km_kbp_state_create(keyman->keyboard,
                                  keyboard_opts,
                                  &(keyman->state));
    if (status_state != KM_KBP_STATUS_OK)
    {
        g_warning("problem creating km_kbp_state");
    }
    for (int i=0; i < KEYMAN_ENVIRONMENT_OPTIONS + num_options + 1; i++) {
        g_free((km_kbp_cp *)keyboard_opts[i].key);
        g_free((km_kbp_cp *)keyboard_opts[i].value);
    }
    g_queue_free_full(queue_options, NULL);
    g_free(keyboard_opts);

    reset_context(engine);

    return (GObject *) keyman;
}


static void
ibus_keyman_engine_destroy (IBusKeymanEngine *keyman)
{
    const gchar *engine_name;

    g_debug("DAR: ibus_keyman_engine_destroy");
    engine_name = ibus_engine_get_name ((IBusEngine *) keyman);
    g_assert (engine_name);
    g_message("DAR: ibus_keyman_engine_destroy %s", engine_name);

    if (keyman->prop_list) {
        g_debug("DAR: unref keyman->prop_list");
        g_object_unref (keyman->prop_list);
        keyman->prop_list = NULL;
    }

    if (keyman->status_prop) {
        g_debug("DAR: unref keyman->status_prop");
        g_object_unref (keyman->status_prop);
        keyman->status_prop = NULL;
    }

    if (keyman->table) {
        g_debug("DAR: unref keyman->table");
        g_object_unref (keyman->table);
        keyman->table = NULL;
    }
    if (keyman->state) {
        km_kbp_state_dispose(keyman->state);
        keyman->state = NULL;
    }

    if (keyman->keyboard) {
        km_kbp_keyboard_dispose(keyman->keyboard);
        keyman->keyboard = NULL;
    }

    g_free(keyman->kb_name);
    g_free(keyman->ldmlfile);

    IBUS_OBJECT_CLASS (parent_class)->destroy ((IBusObject *)keyman);
}

static void
ibus_keyman_engine_commit_string (IBusKeymanEngine *keyman,
                                const gchar    *string)
{
    IBusText *text;
    g_message("DAR: ibus_keyman_engine_commit_string - %s", string);
    text = ibus_text_new_from_static_string (string);
    g_object_ref_sink(text);
    ibus_engine_commit_text ((IBusEngine *)keyman, text);
    g_object_unref (text);
}

static void forward_backspace(IBusKeymanEngine *keyman, unsigned int state)
{
    g_message("DAR: forward_backspace %d no keysym state %d", KEYMAN_BACKSPACE, state);
    ibus_engine_forward_key_event((IBusEngine *)keyman, KEYMAN_BACKSPACE_KEYSYM, KEYMAN_BACKSPACE, state);
}

static gboolean ok_for_single_backspace(const km_kbp_action_item *action_items, int i, size_t num_actions)
{
    for (int j=i+1; j < num_actions; j++) {
        if (action_items[i].type == KM_KBP_IT_BACK || action_items[i].type == KM_KBP_IT_CHAR || action_items[i].type == KM_KBP_IT_EMIT_KEYSTROKE) {
            return FALSE;
        }
    }
    return TRUE;
}

static gboolean
process_unicode_char_action(
  IBusKeymanEngine *keyman,
  const km_kbp_action_item *action_item
) {
  if (g_unichar_type(action_item->character) == G_UNICODE_SURROGATE) {
    if (keyman->firstsurrogate == 0) {
      keyman->firstsurrogate = action_item->character;
      g_message("first surrogate %d", keyman->firstsurrogate);
    } else {
      glong items_read, items_written;
      gunichar2 utf16_pair[2] = {keyman->firstsurrogate, action_item->character};
      gchar *utf8_pair        = g_utf16_to_utf8(utf16_pair, 2, &items_read, &items_written, NULL);
      if (keyman->char_buffer == NULL) {
        keyman->char_buffer = utf8_pair;
      } else {
        gchar *new_buffer = g_strjoin("", keyman->char_buffer, utf8_pair, NULL);
        g_free(keyman->char_buffer);
        g_free(utf8_pair);
        keyman->char_buffer = new_buffer;
      }
      keyman->firstsurrogate = 0;
    }
  } else {
    gchar *utf8   = (gchar *)g_new0(gchar, 12);
    gint numbytes = g_unichar_to_utf8(action_item->character, utf8);
    if (numbytes > 12) {
      g_error("g_unichar_to_utf8 overflowing buffer");
      g_free(utf8);
    } else {
      g_message("unichar:U+%04x, bytes:%d, string:%s", action_item->character, numbytes, utf8);
      if (keyman->char_buffer == NULL) {
        g_message("setting buffer to converted unichar");
        keyman->char_buffer = utf8;
      } else {
        g_message("appending converted unichar to CHAR buffer");
        gchar *new_buffer = g_strjoin("", keyman->char_buffer, utf8, NULL);
        g_free(keyman->char_buffer);
        g_free(utf8);
        keyman->char_buffer = new_buffer;
      }
      g_message("CHAR buffer is now %s", keyman->char_buffer);
    }
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
  const km_kbp_action_item *action_items,
  int i,
  size_t num_action_items
) {
  IBusKeymanEngine *keyman = (IBusKeymanEngine *)engine;
  if (action_items[i].backspace.expected_type == KM_KBP_IT_MARKER) {
    g_message("skipping marker type");
  } else if (keyman->char_buffer != NULL) {
    // ibus_keyman_engine_commit_string(keyman, keyman->char_buffer);
    g_message("removing one utf8 char from CHAR buffer");
    glong end_pos = g_utf8_strlen(keyman->char_buffer, -1);
    gchar *new_buffer;
    if (end_pos == 1) {
      new_buffer = NULL;
      g_message("resetting CHAR buffer to NULL");
    } else {
      new_buffer = g_utf8_substring(keyman->char_buffer, 0, end_pos - 1);
      g_message("changing CHAR buffer to :%s:", new_buffer);
    }
    if (g_strcmp0(keyman->char_buffer, new_buffer) == 0) {
      g_message("oops, CHAR buffer hasn't changed");
    }
    g_free(keyman->char_buffer);
    keyman->char_buffer = new_buffer;
  } else if (ok_for_single_backspace(action_items, i, num_action_items)) {
    // single backspace can be handled by ibus as normal
    g_message("no char actions, just single back");
    return FALSE;
  } else {
    g_message(
        "DAR: process_backspace_action - client_capabilities=%x, %x", engine->client_capabilities, IBUS_CAP_SURROUNDING_TEXT);

    if ((engine->client_capabilities & IBUS_CAP_SURROUNDING_TEXT) != 0) {
      g_message("deleting surrounding text 1 char");
      ibus_engine_delete_surrounding_text(engine, -1, 1);
    } else {
      g_message("forwarding backspace with reset context");
      km_kbp_context_item *context_items;
      km_kbp_context_get(km_kbp_state_context(keyman->state), &context_items);
      reset_context(engine);
      forward_backspace(keyman, 0);
      km_kbp_context_set(km_kbp_state_context(keyman->state), context_items);
      km_kbp_context_items_dispose(context_items);
    }
  }
  return TRUE;
}

static gboolean
process_persist_action(
  IBusKeymanEngine *keyman,
  const km_kbp_action_item *action_item
) {
  // Save keyboard option
  if (!action_item->option)
    return TRUE;

  // Allocate for 1 option plus 1 pad struct of 0's
  km_kbp_option_item *keyboard_opts = g_new0(km_kbp_option_item, 2);
  memmove(&(keyboard_opts[0]), action_item->option, sizeof(km_kbp_option_item));
  km_kbp_status event_status = km_kbp_state_options_update(keyman->state, keyboard_opts);
  if (event_status != KM_KBP_STATUS_OK) {
    g_warning("problem saving option for km_kbp_keyboard");
  }
  g_free(keyboard_opts);

  // Put the keyboard option into DConf
  if (action_item->option->key != NULL && action_item->option->value != NULL) {
    g_message("Saving keyboard option to DConf");
    // Load the current keyboard options from DConf
    keyman_put_options_todconf(
        keyman->kb_name, keyman->kb_name, (gchar *)action_item->option->key, (gchar *)action_item->option->value);
  }
  return TRUE;
}

static gboolean process_emit_keystroke_action(IBusKeymanEngine *keyman) {
  if (keyman->char_buffer != NULL) {
    ibus_keyman_engine_commit_string(keyman, keyman->char_buffer);
    g_free(keyman->char_buffer);
    keyman->char_buffer = NULL;
  }
  keyman->emitting_keystroke = TRUE;
  return TRUE;
}

static gboolean process_invalidate_context_action(IBusEngine *engine) {
  IBusKeymanEngine *keyman = (IBusKeymanEngine *)engine;
  reset_context(engine);
  return TRUE;
}

static gboolean
process_capslock_action(
  IBusKeymanEngine *keyman,
  const km_kbp_action_item *action_item
) {
  g_message("**** %s caps-lock", action_item->capsLock ? "Enable" : "Disable");

#ifdef GDK_WINDOWING_X11
  if (keyman->xdisplay) {
    XkbLockModifiers(keyman->xdisplay, XkbUseCoreKbd, LockMask, action_item->capsLock ? LockMask : 0);

    XSync(keyman->xdisplay, False);
  }
#endif
#ifdef GDK_WINDOWING_WAYLAND
  // TODO
  if (keyman->wldisplay) {

  }
#endif
  return TRUE;
}

static gboolean process_end_action(IBusKeymanEngine *keyman) {
  keyman->firstsurrogate = 0;
  if (keyman->char_buffer != NULL) {
    ibus_keyman_engine_commit_string(keyman, keyman->char_buffer);
    g_free(keyman->char_buffer);
    keyman->char_buffer = NULL;
  }
  if (keyman->emitting_keystroke) {
    keyman->emitting_keystroke = FALSE;
    return FALSE;
  }

  return TRUE;
}

static gboolean
process_actions(
  IBusEngine *engine,
  const km_kbp_action_item *action_items,
  size_t num_action_items
) {
  IBusKeymanEngine *keyman = (IBusKeymanEngine *)engine;
  for (int i = 0; i < num_action_items; i++) {
    gboolean continue_with_next_action = TRUE;
    switch (action_items[i].type) {
    case KM_KBP_IT_CHAR:
      g_message("CHAR action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_unicode_char_action(keyman, &action_items[i]);
      break;
    case KM_KBP_IT_MARKER:
      g_message("MARKER action %d/%d", i + 1, (int)num_action_items);
      break;
    case KM_KBP_IT_ALERT:
      g_message("ALERT action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_alert_action();
      break;
    case KM_KBP_IT_BACK:
      g_message("BACK action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_backspace_action(engine, action_items, i, num_action_items);
      break;
    case KM_KBP_IT_PERSIST_OPT:
      g_message("PERSIST_OPT action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_persist_action(keyman, &action_items[i]);
      break;
    case KM_KBP_IT_EMIT_KEYSTROKE:
      g_message("EMIT_KEYSTROKE action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_emit_keystroke_action(keyman);
      break;
    case KM_KBP_IT_INVALIDATE_CONTEXT:
      g_message("INVALIDATE_CONTEXT action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_invalidate_context_action(engine);
      break;
    case KM_KBP_IT_CAPSLOCK:
      g_message("CAPSLOCK action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_capslock_action(keyman, &action_items[i]);
      break;
    case KM_KBP_IT_END:
      g_message("END action %d/%d", i + 1, (int)num_action_items);
      continue_with_next_action = process_end_action(keyman);
      break;
    default:
      g_warning("Unknown action %d/%d(%d)", i + 1, (int)num_action_items, action_items[i].type);
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

  gboolean isKeyDown = !(state & IBUS_RELEASE_MASK);

  g_message("-----------------------------------------------------------------------------------------------------------------");
  g_message(
      "DAR: ibus_keyman_engine_process_key_event - keyval=0x%02x keycode=0x%02x, state=0x%02x, isKeyDown=%d, prefilter=%d", keyval, keycode,
      state, isKeyDown, (state & IBUS_PREFILTER_MASK) > 0);

  if (!(state & IBUS_PREFILTER_MASK)) {
    ibus_engine_forward_key_event(engine, keyval, keycode, state | IBUS_PREFILTER_MASK);
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
    g_warning("keycode %d out of range", keycode);
    return FALSE;
  }

  if (keycode_to_vk[keycode] == 0) {  // key we don't handle
    return FALSE;
  }

  // keyman modifiers are different from X11/ibus
  uint16_t km_mod_state = 0;
  if (state & IBUS_SHIFT_MASK) {
    km_mod_state |= KM_KBP_MODIFIER_SHIFT;
  }
  if (state & IBUS_MOD5_MASK) {
    km_mod_state |= KM_KBP_MODIFIER_RALT;
    g_message("modstate KM_KBP_MODIFIER_RALT from IBUS_MOD5_MASK");
  }
  if (state & IBUS_MOD1_MASK) {
    if (keyman->ralt_pressed) {
      km_mod_state |= KM_KBP_MODIFIER_RALT;
      g_message("modstate KM_KBP_MODIFIER_RALT from ralt_pressed");
    }
    if (keyman->lalt_pressed) {
      km_mod_state |= KM_KBP_MODIFIER_LALT;
      g_message("modstate KM_KBP_MODIFIER_LALT from lalt_pressed");
    }
  }
  if (state & IBUS_CONTROL_MASK) {
    if (keyman->rctrl_pressed) {
      km_mod_state |= KM_KBP_MODIFIER_RCTRL;
      g_message("modstate KM_KBP_MODIFIER_RCTRL from rctrl_pressed");
    }
    if (keyman->lctrl_pressed) {
      km_mod_state |= KM_KBP_MODIFIER_LCTRL;
      g_message("modstate KM_KBP_MODIFIER_LCTRL from lctrl_pressed");
    }
  }
  if (state & IBUS_LOCK_MASK) {
    km_mod_state |= KM_KBP_MODIFIER_CAPS;
  }
  g_message("before process key event");
  km_kbp_context *context = km_kbp_state_context(keyman->state);
  g_free(get_current_context_text(context));
  g_message("DAR: ibus_keyman_engine_process_key_event - km_mod_state=0x%x", km_mod_state);
  km_kbp_status event_status = km_kbp_process_event(keyman->state, keycode_to_vk[keycode], km_mod_state, isKeyDown);
  context                    = km_kbp_state_context(keyman->state);
  g_message("after process key event");
  g_free(get_current_context_text(context));

  // km_kbp_state_action_items to get action items
  size_t num_action_items;
  g_free(keyman->char_buffer);
  keyman->char_buffer                    = NULL;
  const km_kbp_action_item *action_items = km_kbp_state_action_items(keyman->state, &num_action_items);

  if (!process_actions(engine, action_items, num_action_items))
    return FALSE;

  context = km_kbp_state_context(keyman->state);
  g_message("after processing all actions");
  g_free(get_current_context_text(context));
  return TRUE;
}

static void
ibus_keyman_engine_set_surrounding_text (IBusEngine *engine,
                                            IBusText    *text,
                                            guint       cursor_pos,
                                            guint       anchor_pos)
{
    gchar *surrounding_text;
    guint context_start = cursor_pos > MAXCONTEXT_ITEMS ? cursor_pos - MAXCONTEXT_ITEMS : 0;
    g_message("ibus_keyman_engine_set_surrounding_text");
    if (cursor_pos != anchor_pos){
        g_message("ibus_keyman_engine_set_surrounding_text: There is a selection");
    }
    parent_class->set_surrounding_text (engine, text, cursor_pos, anchor_pos);
    surrounding_text = g_utf8_substring(ibus_text_get_text(text), context_start, cursor_pos);
    g_message("surrounding context is:%u:%s:", cursor_pos - context_start, surrounding_text);
    g_free(surrounding_text);
    reset_context(engine);
}

// static void ibus_keyman_engine_set_cursor_location (IBusEngine             *engine,
//                                              gint                    x,
//                                              gint                    y,
//                                              gint                    w,
//                                              gint                    h)
// {
//     g_message("ibus_keyman_engine_set_cursor_location");
//     //ibus_keyman_engine_reset(engine);
//     parent_class->set_cursor_location (engine, x, y, w, h);
// }

static void
ibus_keyman_engine_focus_in (IBusEngine *engine)
{
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;

    g_message("ibus_keyman_engine_focus_in");
    ibus_engine_register_properties (engine, keyman->prop_list);

    reset_context(engine);
    parent_class->focus_in (engine);
}

static void
ibus_keyman_engine_focus_out (IBusEngine *engine)
{
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;

    g_message("ibus_keyman_engine_focus_out");
    km_kbp_context_clear(km_kbp_state_context(keyman->state));
    parent_class->focus_out (engine);
}

static void
ibus_keyman_engine_reset (IBusEngine *engine)
{
    g_message("ibus_keyman_engine_reset");
    parent_class->reset (engine);
    ibus_keyman_engine_focus_in (engine);
}



static void
ibus_keyman_engine_enable (IBusEngine *engine)
{
    const gchar *engine_name;
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;

    engine_name = ibus_engine_get_name (engine);
    g_assert (engine_name);
    g_message("WDG: ibus_keyman_engine_enable %s", engine_name);
    g_message("enabling surrounding context");
    ibus_engine_get_surrounding_text(engine, NULL, NULL, NULL);
    if (keyman->ldmlfile)
    {
        // own dbus name com.Keyman
        // expose properties LDMLFile and Name
        KeymanService *service = km_service_get_default();
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
    g_message("WDG: ibus_keyman_engine_disable %s", engine_name);
    ibus_keyman_engine_focus_out (engine);
    // stop owning dbus name com.Keyman
    KeymanService *service = km_service_get_default();
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
//     reset_context(engine);
// }

// static void
// ibus_keyman_engine_page_down (IBusEngine *engine)
// {
//     g_message("ibus_keyman_engine_page_down");
//     parent_class->page_down (engine);
//     reset_context(engine);
// }

// static void
// ibus_keyman_engine_cursor_up (IBusEngine *engine)
// {
//     g_message("ibus_keyman_engine_cursor_up");
//     parent_class->cursor_up (engine);
//     reset_context(engine);
// }

// static void
// ibus_keyman_engine_cursor_down (IBusEngine *engine)
// {
//     g_message("ibus_keyman_engine_cursor_down");
//     parent_class->cursor_down (engine);
//     reset_context(engine);
// }

static void
ibus_keyman_engine_property_activate (IBusEngine  *engine,
                                    const gchar *prop_name,
                                    guint        prop_state)
{
    g_message("ibus_keyman_engine_property_activate");
    parent_class->property_activate (engine, prop_name, prop_state);
}


