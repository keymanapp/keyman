/* -*- mode: C; c-basic-offset: 4; indent-tabs-mode: nil; -*- */
/* vim:set et sts=4: */
/* ibus - The Input Bus
 * Copyright (C) 2008-2013 Peng Huang <shawn.p.huang@gmail.com>
 * Copyright (C) 2015-2021 Takao Fujiwara <takao.fujiwara1@gmail.com>
 * Copyright (C) 2008-2021 Red Hat, Inc.
 * Copyright (C) 2021-2022 SIL International
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

// This file is based on https://github.com/ibus/ibus/blob/master/client/gtk2/ibusimcontext.c

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "ibusimcontext.h"
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <ibus.h>
#include <string.h>

#ifdef GDK_WINDOWING_WAYLAND
#include <gdk/gdkwayland.h>
#endif

#ifdef DEBUG
#define IDEBUG g_debug
#else
#define IDEBUG(a...)
#endif

#define MAX_QUEUED_EVENTS 20

#ifndef IBUS_PREFILTER_MASK
#define IBUS_PREFILTER_MASK (1 << 23)
#endif

struct _IBusIMContext {
  GtkIMContext parent;

  /* instance members */
  GdkWindow *client_window;

  IBusInputContext *ibuscontext;

  GdkRectangle cursor_area;
  gboolean has_focus;

  guint32 time;
  gint caps;

  /* cancellable */
  GCancellable *cancellable;
  GQueue *events_queue;

  gboolean use_button_press_event;

  GMainLoop *thread_loop;

  // test properties
  GString *text;
};

static gboolean surrounding_text_supported = TRUE;

struct _IBusIMContextClass {
  GtkIMContextClass parent;
  /* class members */
};

typedef struct _timeout_data {
  GMainLoop *thread_loop;
  gboolean timed_out;
} timeout_data;

static GtkIMContext *_focus_im_context = NULL;
static IBusInputContext *_fake_context = NULL;
static GdkWindow *_input_window        = NULL;
static GtkWidget *_input_widget        = NULL;

/* functions prototype */
static void ibus_im_context_class_init(IBusIMContextClass *class);
static void ibus_im_context_class_fini(IBusIMContextClass *class);
static void ibus_im_context_init(GObject *obj);
static void ibus_im_context_notify(GObject *obj, GParamSpec *pspec);
static void ibus_im_context_finalize(GObject *obj);
static void ibus_im_context_reset(GtkIMContext *context);
static gboolean ibus_im_context_filter_keypress(GtkIMContext *context, GdkEventKey *key);
static void ibus_im_context_focus_in(GtkIMContext *context);
static void ibus_im_context_focus_out(GtkIMContext *context);
static void ibus_im_context_set_client_window(GtkIMContext *context, GdkWindow *client);
static void ibus_im_context_set_cursor_location(GtkIMContext *context, GdkRectangle *area);
static void ibus_im_context_set_surrounding(GtkIMContext *context, const gchar *text, int len, int cursor_index);
static void ibus_im_context_set_surrounding_with_selection(
    GtkIMContext *context,
    const gchar *text,
    int len,
    int cursor_index,
    int anchor_index);

/* static methods*/
static void _create_input_context(IBusIMContext *context);
static gboolean _set_cursor_location_internal(IBusIMContext *context);

static void _bus_connected_cb(IBusBus *bus, IBusIMContext *context);
static void _request_surrounding_text(IBusIMContext *context);
static gboolean _set_content_type(IBusIMContext *context);
static void _commit_text(IBusIMContext *context, const gchar *text);
static gboolean _retrieve_surrounding(IBusIMContext *context);
static gboolean _delete_surrounding(IBusIMContext *context, gint offset_from_cursor, guint nchars);
static gboolean _timeout_callback(gpointer *user_data);
static void _run_main_loop_with_timeout(IBusIMContext *ibusimcontext);

static GType _ibus_type_im_context     = 0;
static GtkIMContextClass *parent_class = NULL;

static IBusBus *_bus               = NULL;
static guint _daemon_name_watch_id = 0;
static gboolean _daemon_is_running = FALSE;

void
ibus_im_context_register_type(GTypeModule *type_module) {
  IDEBUG("%s", __FUNCTION__);

  static const GTypeInfo ibus_im_context_info = {
      sizeof(IBusIMContextClass),
      (GBaseInitFunc)NULL,
      (GBaseFinalizeFunc)NULL,
      (GClassInitFunc)ibus_im_context_class_init,
      (GClassFinalizeFunc)ibus_im_context_class_fini,
      NULL, /* class data */
      sizeof(IBusIMContext),
      0,
      (GInstanceInitFunc)ibus_im_context_init,
  };

  if (!_ibus_type_im_context) {
    if (type_module) {
      _ibus_type_im_context =
          g_type_module_register_type(type_module, GTK_TYPE_IM_CONTEXT, "IBusIMContext", &ibus_im_context_info, (GTypeFlags)0);
    } else {
      _ibus_type_im_context = g_type_register_static(GTK_TYPE_IM_CONTEXT, "IBusIMContext", &ibus_im_context_info, (GTypeFlags)0);
    }
  }
}

GType
ibus_im_context_get_type(void) {
  IDEBUG("%s", __FUNCTION__);

  if (_ibus_type_im_context == 0) {
    ibus_im_context_register_type(NULL);
  }

  g_assert(_ibus_type_im_context != 0);
  return _ibus_type_im_context;
}

IBusIMContext *
ibus_im_context_new(void) {
  IDEBUG("%s", __FUNCTION__);

  GObject *obj = g_object_new(IBUS_TYPE_IM_CONTEXT, NULL);
  return IBUS_IM_CONTEXT(obj);
}

static gboolean
_focus_in_cb(GtkWidget *widget, GdkEventFocus *event, gpointer user_data) {
  if (_focus_im_context == NULL && _fake_context != NULL) {
    ibus_input_context_focus_in(_fake_context);
  }
  return FALSE;
}

static gboolean
_focus_out_cb(GtkWidget *widget, GdkEventFocus *event, gpointer user_data) {
  if (_focus_im_context == NULL && _fake_context != NULL) {
    ibus_input_context_focus_out(_fake_context);
  }
  return FALSE;
}

static gboolean
ibus_im_context_commit_event(IBusIMContext *ibusimcontext, GdkEventKey *event) {
  IDEBUG("%s", __FUNCTION__);
  guint keyval          = 0;
  GdkModifierType state = 0;
  int i;
  GdkModifierType no_text_input_mask;
  gunichar ch;

  if (event->type == GDK_KEY_RELEASE)
    return FALSE;
  keyval = event->keyval;
  state  = event->state;

  /* Ignore modifier key presses */
  for (i = 0; i < G_N_ELEMENTS(IBUS_COMPOSE_IGNORE_KEYLIST); i++)
    if (keyval == IBUS_COMPOSE_IGNORE_KEYLIST[i])
      return FALSE;
  no_text_input_mask =
      gdk_keymap_get_modifier_mask(gdk_keymap_get_for_display(gdk_display_get_default()), GDK_MODIFIER_INTENT_NO_TEXT_INPUT);
  if (state & no_text_input_mask || keyval == GDK_KEY_Return || keyval == GDK_KEY_ISO_Enter || keyval == GDK_KEY_KP_Enter) {
    return FALSE;
  }
  ch = ibus_keyval_to_unicode(keyval);
  if (ch != 0 && !g_unichar_iscntrl(ch)) {
    IBusText *text = ibus_text_new_from_unichar(ch);
    IDEBUG("%s: text=%p", __FUNCTION__, text);
    _request_surrounding_text(ibusimcontext);
    _commit_text(ibusimcontext, text->text);
    return TRUE;
  }
  return FALSE;
}

struct _ProcessKeyEventData {
  GdkEvent *event;
  IBusIMContext *ibusimcontext;
};

typedef struct _ProcessKeyEventData ProcessKeyEventData;

static void
_process_key_event_done(GObject *object, GAsyncResult *res, gpointer user_data) {
  IBusInputContext *context = (IBusInputContext *)object;

  ProcessKeyEventData *data = (ProcessKeyEventData *)user_data;
  GdkEvent *event           = data->event;
  GError *error             = NULL;

  g_slice_free(ProcessKeyEventData, data);
  gboolean retval = ibus_input_context_process_key_event_async_finish(context, res, &error);

  if (error != NULL) {
    g_warning("Process Key Event failed: %s.", error->message);
    g_error_free(error);
  }

  if (retval == FALSE) {
    ((GdkEventKey *)event)->state |= IBUS_IGNORED_MASK;
    gdk_event_put(event);
  }
  gdk_event_free(event);
}

static gboolean
_process_key_event(IBusInputContext *context, GdkEventKey *event, IBusIMContext *ibusimcontext) {
  guint state;
  guint keyval             = 0;
  guint16 hardware_keycode = 0;
  guint keycode            = 0;
  gboolean retval          = FALSE;

  state = event->state;
  if (event->type == GDK_KEY_RELEASE)
    state |= IBUS_RELEASE_MASK;
  keyval           = event->keyval;
  hardware_keycode = event->hardware_keycode;
  keycode          = hardware_keycode;

  ProcessKeyEventData *data = g_slice_new0(ProcessKeyEventData);
  data->event               = gdk_event_copy((GdkEvent *)event);
  data->ibusimcontext       = ibusimcontext;
  ibus_input_context_process_key_event_async(context, keyval, keycode, state, -1, NULL, _process_key_event_done, data);

  retval = TRUE;

  if (retval)
    event->state |= IBUS_HANDLED_MASK;
  else
    event->state |= IBUS_IGNORED_MASK;

  return retval;
}

/* emit "retrieve-surrounding" glib signal of GtkIMContext, if
 * context->caps has IBUS_CAP_SURROUNDING_TEXT and the current IBus
 * engine needs surrounding-text.
 */
static void
_request_surrounding_text(IBusIMContext *context) {
  if (context && (context->caps & IBUS_CAP_SURROUNDING_TEXT) != 0 && context->ibuscontext != NULL &&
      ibus_input_context_needs_surrounding_text(context->ibuscontext)) {
    _retrieve_surrounding(context);
  } else {
    // g_debug("%s has no capability of surrounding-text feature", g_get_prgname());
  }
}

static gboolean
_set_content_type(IBusIMContext *context) {
  if (context->ibuscontext != NULL) {
    GtkInputPurpose purpose;
    GtkInputHints hints;

    g_object_get(G_OBJECT(context), "input-purpose", &purpose, "input-hints", &hints, NULL);

    ibus_input_context_set_content_type(context->ibuscontext, purpose, hints);
  }
  return TRUE;
}

static gboolean
_get_boolean_env(const gchar *name, gboolean defval) {
  const gchar *value = g_getenv(name);

  if (value == NULL)
    return defval;

  if (g_strcmp0(value, "") == 0 || g_strcmp0(value, "0") == 0 || g_strcmp0(value, "false") == 0 ||
      g_strcmp0(value, "False") == 0 || g_strcmp0(value, "FALSE") == 0)
    return FALSE;

  return TRUE;
}

static void
daemon_name_appeared(GDBusConnection *connection, const gchar *name, const gchar *owner, gpointer data) {
  if (!g_strcmp0(ibus_bus_get_service_name(_bus), IBUS_SERVICE_PORTAL)) {
    _daemon_is_running = TRUE;
    return;
  }
  /* If ibus-daemon is running and run ssh -X localhost,
   * daemon_name_appeared() is called but ibus_get_address() == NULL
   * because the hostname and display number are different between
   * ibus-daemon and clients. So IBusBus would not be connected and
   * ibusimcontext->ibuscontext == NULL and ibusimcontext->events_queue
   * could go beyond MAX_QUEUED_EVENTS . */
  _daemon_is_running = (ibus_get_address() != NULL);
}

static void
daemon_name_vanished(GDBusConnection *connection, const gchar *name, gpointer data) {
  _daemon_is_running = FALSE;
}

static void
ibus_im_context_class_init(IBusIMContextClass *class) {
  IDEBUG("%s", __FUNCTION__);

  GtkIMContextClass *im_context_class = GTK_IM_CONTEXT_CLASS(class);
  GObjectClass *gobject_class         = G_OBJECT_CLASS(class);

  parent_class = (GtkIMContextClass *)g_type_class_peek_parent(class);

  im_context_class->reset               = ibus_im_context_reset;
  im_context_class->focus_in            = ibus_im_context_focus_in;
  im_context_class->focus_out           = ibus_im_context_focus_out;
  im_context_class->filter_keypress     = ibus_im_context_filter_keypress;
  im_context_class->set_client_window   = ibus_im_context_set_client_window;
  im_context_class->set_cursor_location = ibus_im_context_set_cursor_location;
  im_context_class->set_surrounding     = ibus_im_context_set_surrounding;
  gobject_class->notify                 = ibus_im_context_notify;
  gobject_class->finalize               = ibus_im_context_finalize;

  /* init bus object */
  if (_bus == NULL) {
    _bus = ibus_bus_new_async_client();

    g_signal_connect(_bus, "connected", G_CALLBACK(_bus_connected_cb), NULL);
  }

  _daemon_name_watch_id = g_bus_watch_name(
      G_BUS_TYPE_SESSION, ibus_bus_get_service_name(_bus), G_BUS_NAME_WATCHER_FLAGS_NONE, daemon_name_appeared,
      daemon_name_vanished, NULL, NULL);
}

static void
ibus_im_context_class_fini(IBusIMContextClass *class) {
  g_bus_unwatch_name(_daemon_name_watch_id);
}

static void
ibus_im_context_init(GObject *obj) {
  IDEBUG("%s", __FUNCTION__);

  IBusIMContext *ibusimcontext = IBUS_IM_CONTEXT(obj);

  ibusimcontext->client_window = NULL;

  // Init cursor area
  ibusimcontext->cursor_area.x      = -1;
  ibusimcontext->cursor_area.y      = -1;
  ibusimcontext->cursor_area.width  = 0;
  ibusimcontext->cursor_area.height = 0;

  ibusimcontext->ibuscontext = NULL;
  ibusimcontext->has_focus   = FALSE;
  ibusimcontext->time        = GDK_CURRENT_TIME;
  ibusimcontext->caps        = IBUS_CAP_FOCUS | IBUS_CAP_AUXILIARY_TEXT | IBUS_CAP_PROPERTY;
  if (surrounding_text_supported) {
    ibusimcontext->caps |= IBUS_CAP_SURROUNDING_TEXT;
  }

  ibusimcontext->events_queue = g_queue_new();

  if (ibus_bus_is_connected(_bus)) {
    _create_input_context(ibusimcontext);
  }

  g_signal_connect(_bus, "connected", G_CALLBACK(_bus_connected_cb), obj);

  _daemon_is_running = (ibus_get_address() != NULL);
}

static void
ibus_im_context_notify(GObject *obj, GParamSpec *pspec) {
  IDEBUG("%s", __FUNCTION__);

  if (g_strcmp0(pspec->name, "input-purpose") == 0 || g_strcmp0(pspec->name, "input-hints") == 0) {
    _set_content_type(IBUS_IM_CONTEXT(obj));
  }
}

static void
ibus_im_context_finalize(GObject *obj) {
  IDEBUG("%s", __FUNCTION__);

  IBusIMContext *ibusimcontext = IBUS_IM_CONTEXT(obj);

  g_signal_handlers_disconnect_by_func(_bus, G_CALLBACK(_bus_connected_cb), obj);

  if (ibusimcontext->cancellable != NULL) {
    /* Cancel any ongoing create input context request */
    g_cancellable_cancel(ibusimcontext->cancellable);
    g_object_unref(ibusimcontext->cancellable);
    ibusimcontext->cancellable = NULL;
  }

  if (ibusimcontext->ibuscontext) {
    ibus_proxy_destroy((IBusProxy *)ibusimcontext->ibuscontext);
  }

  ibus_im_context_set_client_window((GtkIMContext *)ibusimcontext, NULL);

  g_queue_free_full(ibusimcontext->events_queue, (GDestroyNotify)gdk_event_free);

  G_OBJECT_CLASS(parent_class)->finalize(obj);
}

static gboolean
ibus_im_context_filter_keypress(GtkIMContext *context, GdkEventKey *event) {
  IDEBUG("%s", __FUNCTION__);

  IBusIMContext *ibusimcontext = IBUS_IM_CONTEXT(context);

  if (event->state & IBUS_HANDLED_MASK)
    return TRUE;

  /* Do not call gtk_im_context_filter_keypress() because
   * gtk_im_context_simple_filter_keypress() binds Ctrl-Shift-u
   */
  if (event->state & IBUS_IGNORED_MASK && !(event->state & IBUS_PREFILTER_MASK))
    return ibus_im_context_commit_event(ibusimcontext, event);

  /* XXX it is a workaround for some applications do not set client
   * window. */
  if (ibusimcontext->client_window == NULL && event->window != NULL)
    gtk_im_context_set_client_window((GtkIMContext *)ibusimcontext, event->window);

  _request_surrounding_text(ibusimcontext);

  ibusimcontext->time = event->time;

  if (ibusimcontext->ibuscontext) {
    gboolean result = _process_key_event(ibusimcontext->ibuscontext, event, ibusimcontext);
    if (result) {
      _run_main_loop_with_timeout(ibusimcontext);
    }
    return result;
  }

  /* At this point we _should_ be waiting for the IBus context to be
   * created or the connection to IBus to be established. If that's
   * the case we queue events to be processed when the IBus context
   * is ready. */
  g_return_val_if_fail(ibusimcontext->cancellable != NULL || ibus_bus_is_connected(_bus) == FALSE, FALSE);
  g_queue_push_tail(ibusimcontext->events_queue, gdk_event_copy((GdkEvent *)event));

  if (g_queue_get_length(ibusimcontext->events_queue) > MAX_QUEUED_EVENTS) {
    g_warning("Events queue growing too big, will start to drop.");
    gdk_event_free((GdkEvent *)g_queue_pop_head(ibusimcontext->events_queue));
  }

  return TRUE;
}

static void
ibus_im_context_focus_in(GtkIMContext *context) {
  IBusIMContext *ibusimcontext = (IBusIMContext *)context;
  GtkWidget *widget            = NULL;

  IDEBUG("%s: context=%p", __FUNCTION__, context);

  if (ibusimcontext->has_focus)
    return;

  /* don't set focus on password entry */
  if (ibusimcontext->client_window != NULL) {
    gdk_window_get_user_data(ibusimcontext->client_window, (gpointer *)&widget);
  }

  if (widget && GTK_IS_ENTRY(widget) && !gtk_entry_get_visibility(GTK_ENTRY(widget))) {
    return;
  }
  /* Do not call gtk_im_context_focus_out() here.
   * google-chrome's notification popup window (Pushbullet)
   * takes the focus and the popup window disappears.
   * So other applications lose the focus because
   * ibusimcontext->has_focus is FALSE if
   * gtk_im_context_focus_out() is called here when
   * _focus_im_context != context.
   */
  if (_focus_im_context == NULL) {
    /* focus out fake context */
    if (_fake_context != NULL) {
      ibus_input_context_focus_out(_fake_context);
    }
  }

  ibusimcontext->has_focus = TRUE;
  if (ibusimcontext->ibuscontext) {
    if (!_set_content_type(ibusimcontext)) {
      ibusimcontext->has_focus = FALSE;
      return;
    }
    ibus_input_context_focus_in(ibusimcontext->ibuscontext);
  }

  /* set_cursor_location_internal() will get origin from X server,
   * it blocks UI. So delay it to idle callback. */
  g_idle_add_full(
      G_PRIORITY_DEFAULT_IDLE, (GSourceFunc)_set_cursor_location_internal, g_object_ref(ibusimcontext),
      (GDestroyNotify)g_object_unref);

  /* retrieve the initial surrounding-text (regardless of whether
   * the current IBus engine needs surrounding-text) */
  _request_surrounding_text(ibusimcontext);

  g_object_add_weak_pointer((GObject *)context, (gpointer *)&_focus_im_context);
  _focus_im_context = context;
}

static void
ibus_im_context_focus_out(GtkIMContext *context) {
  IDEBUG("%s", __FUNCTION__);
  IBusIMContext *ibusimcontext = (IBusIMContext *)context;

  if (ibusimcontext->has_focus == FALSE) {
    return;
  }

  if (_focus_im_context) {
    g_object_remove_weak_pointer((GObject *)context, (gpointer *)&_focus_im_context);
    _focus_im_context = NULL;
  }

  ibusimcontext->has_focus = FALSE;
  if (ibusimcontext->ibuscontext) {
    ibus_input_context_focus_out(ibusimcontext->ibuscontext);
  }

  /* focus in the fake ic */
  if (_fake_context != NULL) {
    ibus_input_context_focus_in(_fake_context);
  }
}

static void
ibus_im_context_reset(GtkIMContext *context) {
  IDEBUG("%s", __FUNCTION__);

  IBusIMContext *ibusimcontext = IBUS_IM_CONTEXT(context);

  if (ibusimcontext->ibuscontext) {
    ibus_input_context_reset(ibusimcontext->ibuscontext);
  }
}

/* Use the button-press-event signal until GtkIMContext always emits the reset
 * signal.
 * https://gitlab.gnome.org/GNOME/gtk/merge_requests/460
 */
static gboolean
ibus_im_context_button_press_event_cb(GtkWidget *widget, GdkEventButton *event, IBusIMContext *ibusimcontext) {
  IDEBUG("%s", __FUNCTION__);
  if (event->button != 1)
    return FALSE;

  if (ibusimcontext->ibuscontext) {
    ibus_input_context_reset(ibusimcontext->ibuscontext);
  }
  return FALSE;
}

static void
_connect_button_press_event(IBusIMContext *ibusimcontext, gboolean do_connect) {
  GtkWidget *widget = NULL;

  g_assert(ibusimcontext->client_window);
  gdk_window_get_user_data(ibusimcontext->client_window, (gpointer *)&widget);
  /* firefox needs GtkWidget instead of GtkWindow */
  if (GTK_IS_WIDGET(widget)) {
    if (do_connect) {
      g_signal_connect(widget, "button-press-event", G_CALLBACK(ibus_im_context_button_press_event_cb), ibusimcontext);
      ibusimcontext->use_button_press_event = TRUE;
    } else {
      g_signal_handlers_disconnect_by_func(widget, G_CALLBACK(ibus_im_context_button_press_event_cb), ibusimcontext);
      ibusimcontext->use_button_press_event = FALSE;
    }
  }
}

static void
ibus_im_context_set_client_window(GtkIMContext *context, GdkWindow *client) {
  IBusIMContext *ibusimcontext;

  IDEBUG("%s", __FUNCTION__);

  ibusimcontext = IBUS_IM_CONTEXT(context);

  if (ibusimcontext->client_window) {
    if (ibusimcontext->use_button_press_event)
      _connect_button_press_event(ibusimcontext, FALSE);
    g_object_unref(ibusimcontext->client_window);
    ibusimcontext->client_window = NULL;
  }

  if (client != NULL) {
    ibusimcontext->client_window = g_object_ref(client);
    if (!ibusimcontext->use_button_press_event)
      _connect_button_press_event(ibusimcontext, TRUE);
  }
}

static void
_set_rect_scale_factor_with_window(GdkRectangle *area, GdkWindow *window) {
  int scale_factor;

  g_assert(area);
  g_assert(GDK_IS_WINDOW(window));

  scale_factor = gdk_window_get_scale_factor(window);
  area->x *= scale_factor;
  area->y *= scale_factor;
  area->width *= scale_factor;
  area->height *= scale_factor;
}

static gboolean
_set_cursor_location_internal(IBusIMContext *ibusimcontext) {
  GdkRectangle area;

  if (ibusimcontext->client_window == NULL || ibusimcontext->ibuscontext == NULL) {
    return FALSE;
  }

  area = ibusimcontext->cursor_area;

  if (GDK_IS_WAYLAND_DISPLAY(gdk_display_get_default())) {
    gdouble px, py;
    GdkWindow *parent;
    GdkWindow *window = ibusimcontext->client_window;

    while ((parent = gdk_window_get_effective_parent(window)) != NULL) {
      gdk_window_coords_to_parent(window, area.x, area.y, &px, &py);
      area.x = px;
      area.y = py;
      window = parent;
    }

    _set_rect_scale_factor_with_window(&area, ibusimcontext->client_window);
    ibus_input_context_set_cursor_location_relative(ibusimcontext->ibuscontext, area.x, area.y, area.width, area.height);
    return FALSE;
  }

  if (area.x == -1 && area.y == -1 && area.width == 0 && area.height == 0) {
    area.x = 0;
    area.y += gdk_window_get_height(ibusimcontext->client_window);
  }
  gdk_window_get_root_coords(ibusimcontext->client_window, area.x, area.y, &area.x, &area.y);
  _set_rect_scale_factor_with_window(&area, ibusimcontext->client_window);
  ibus_input_context_set_cursor_location(ibusimcontext->ibuscontext, area.x, area.y, area.width, area.height);
  return FALSE;
}

static void
ibus_im_context_set_cursor_location(GtkIMContext *context, GdkRectangle *area) {
  IDEBUG("%s", __FUNCTION__);

  IBusIMContext *ibusimcontext = IBUS_IM_CONTEXT(context);

  /* The area is the relative coordinates and this has to get the absolute
   * ones in _set_cursor_location_internal() since GTK 4.0.
   */
  if (ibusimcontext->cursor_area.x == area->x && ibusimcontext->cursor_area.y == area->y &&
      ibusimcontext->cursor_area.width == area->width && ibusimcontext->cursor_area.height == area->height) {
    return;
  }
  ibusimcontext->cursor_area = *area;
  _set_cursor_location_internal(ibusimcontext);
}

static guint
get_selection_anchor_point(IBusIMContext *ibusimcontext, guint cursor_pos, guint surrounding_text_len) {
  if (cursor_pos <= 0) {
    return ibusimcontext->text->len;
  }
  return cursor_pos;
}

static void
ibus_im_context_set_surrounding(GtkIMContext *context, const gchar *text, int len, int cursor_index) {
  ibus_im_context_set_surrounding_with_selection(context, text, len, cursor_index, cursor_index);
}

static void
ibus_im_context_set_surrounding_with_selection(
    GtkIMContext *context,
    const gchar *text,
    int len,
    int cursor_index,
    int anchor_index) {
  g_return_if_fail(context != NULL);
  g_return_if_fail(IBUS_IS_IM_CONTEXT(context));
  g_return_if_fail(text != NULL);
  g_return_if_fail(strlen(text) >= len);
  g_return_if_fail(0 <= cursor_index && cursor_index <= len);

  IBusIMContext *ibusimcontext = IBUS_IM_CONTEXT(context);

  if (ibusimcontext->ibuscontext) {
    IBusText *ibustext;
    guint cursor_pos;
    guint utf8_len;
    gchar *p;

    p          = g_strndup(text, len);
    cursor_pos = g_utf8_strlen(p, cursor_index);
    utf8_len   = g_utf8_strlen(p, len);
    ibustext   = ibus_text_new_from_string(p);
    IDEBUG("%s: ibustext=%p", __FUNCTION__, ibustext);
    g_free(p);

    gint anchor_pos = get_selection_anchor_point(ibusimcontext, cursor_pos, utf8_len);
    ibus_input_context_set_surrounding_text(ibusimcontext->ibuscontext, ibustext, cursor_pos, anchor_pos);
  }
}

static void
_bus_connected_cb(IBusBus *bus, IBusIMContext *ibusimcontext) {
  IDEBUG("%s", __FUNCTION__);
  if (ibusimcontext)
    _create_input_context(ibusimcontext);
}

static void
_ibus_context_commit_text_cb(IBusInputContext *ibuscontext, IBusText *text, IBusIMContext *ibusimcontext) {
  IDEBUG("%s: text=%p", __FUNCTION__, text);

  _request_surrounding_text(ibusimcontext);
  _commit_text(ibusimcontext, ibus_text_get_text(text));
  g_main_loop_quit(ibusimcontext->thread_loop);
}

static void
_commit_text(IBusIMContext *ibusimcontext, const gchar *text) {
  // g_signal_emit(ibusimcontext, _signal_commit_id, 0, text->text);
  ibus_im_test_set_text(ibusimcontext, text);
}

static gboolean
_retrieve_surrounding(IBusIMContext *ibusimcontext) {
  // No-op in our simulation since we store the text
  return TRUE;
}

static gboolean
_delete_surrounding(IBusIMContext *ibusimcontext, gint offset_from_cursor, guint nchars) {
  // g_signal_emit(ibusimcontext, _signal_delete_surrounding_id, 0);
  if (offset_from_cursor < 0) {
    for (guint val = nchars;
         val <= ibusimcontext->text->len && (guint)ibusimcontext->text->str[ibusimcontext->text->len - val] >= 0x80;
         val++) {
      nchars = val;
    }
    g_string_erase(ibusimcontext->text, ibusimcontext->text->len - nchars, nchars);
  } else {
    g_string_erase(ibusimcontext->text, offset_from_cursor, nchars);
  }
  return TRUE;
}

static gboolean
_key_is_modifier(guint keyval) {
  /* See gdkkeys-x11.c:_gdk_keymap_key_is_modifier() for how this
   * really should be implemented */

  switch (keyval) {
  case GDK_KEY_Shift_L:
  case GDK_KEY_Shift_R:
  case GDK_KEY_Control_L:
  case GDK_KEY_Control_R:
  case GDK_KEY_Caps_Lock:
  case GDK_KEY_Shift_Lock:
  case GDK_KEY_Meta_L:
  case GDK_KEY_Meta_R:
  case GDK_KEY_Alt_L:
  case GDK_KEY_Alt_R:
  case GDK_KEY_Super_L:
  case GDK_KEY_Super_R:
  case GDK_KEY_Hyper_L:
  case GDK_KEY_Hyper_R:
  case GDK_KEY_ISO_Lock:
  case GDK_KEY_ISO_Level2_Latch:
  case GDK_KEY_ISO_Level3_Shift:
  case GDK_KEY_ISO_Level3_Latch:
  case GDK_KEY_ISO_Level3_Lock:
  case GDK_KEY_ISO_Level5_Shift:
  case GDK_KEY_ISO_Level5_Latch:
  case GDK_KEY_ISO_Level5_Lock:
  case GDK_KEY_ISO_Group_Shift:
  case GDK_KEY_ISO_Group_Latch:
  case GDK_KEY_ISO_Group_Lock:
    return TRUE;
  default:
    return FALSE;
  }
}

/* Copy from gdk */
static GdkEventKey *
_create_gdk_event(IBusIMContext *ibusimcontext, guint keyval, guint keycode, guint state) {
  gunichar c = 0;
  gchar buf[8];

  GdkEventKey *event = (GdkEventKey *)gdk_event_new((state & IBUS_RELEASE_MASK) ? GDK_KEY_RELEASE : GDK_KEY_PRESS);

  if (ibusimcontext && ibusimcontext->client_window)
    event->window = g_object_ref(ibusimcontext->client_window);
  else if (_input_window)
    event->window = g_object_ref(_input_window);

  /* The time is copied the latest value from the previous
   * GdkKeyEvent in filter_keypress().
   *
   * We understand the best way would be to pass the all time value
   * to IBus functions process_key_event() and IBus DBus functions
   * ProcessKeyEvent() in IM clients and IM engines so that the
   * _create_gdk_event() could get the correct time values.
   * However it would causes to change many functions and the time value
   * would not provide the useful meanings for each IBus engines but just
   * pass the original value to ForwardKeyEvent().
   * We use the saved value at the moment.
   *
   * Another idea might be to have the time implementation in X servers
   * but some Xorg uses clock_gettime() and others use gettimeofday()
   * and the values would be different in each implementation and
   * locale/remote X server. So probably that idea would not work. */
  if (ibusimcontext) {
    event->time = ibusimcontext->time;
  } else {
    event->time = GDK_CURRENT_TIME;
  }

  event->send_event       = FALSE;
  event->state            = state;
  event->keyval           = keyval;
  event->string           = NULL;
  event->length           = 0;
  event->hardware_keycode = (keycode != 0) ? keycode + 8 : 0;
  event->group            = 0;
  event->is_modifier      = _key_is_modifier(keyval);

  if (keyval != GDK_KEY_VoidSymbol)
    c = gdk_keyval_to_unicode(keyval);

  if (c) {
    gsize bytes_written;
    gint len;

    /* Apply the control key - Taken from Xlib
     */
    if (event->state & GDK_CONTROL_MASK) {
      if ((c >= '@' && c < '\177') || c == ' ')
        c &= 0x1F;
      else if (c == '2') {
#if GLIB_CHECK_VERSION(2, 68, 0)
        event->string = g_memdup2("\0\0", 2);
#else
        event->string = g_memdup("\0\0", 2);
#endif
        event->length = 1;
        buf[0]        = '\0';
        goto out;
      } else if (c >= '3' && c <= '7')
        c -= ('3' - '\033');
      else if (c == '8')
        c = '\177';
      else if (c == '/')
        c = '_' & 0x1F;
    }

    len      = g_unichar_to_utf8(c, buf);
    buf[len] = '\0';

    event->string = g_locale_from_utf8(buf, len, NULL, &bytes_written, NULL);
    if (event->string)
      event->length = bytes_written;
  } else if (keyval == GDK_KEY_Escape) {
    event->length = 1;
    event->string = g_strdup("\033");
  } else if (keyval == GDK_KEY_Return || keyval == GDK_KEY_KP_Enter) {
    event->length = 1;
    event->string = g_strdup("\r");
  }

  if (!event->string) {
    event->length = 0;
    event->string = g_strdup("");
  }
out:
  return event;
}

static void
_ibus_context_forward_key_event_cb(
    IBusInputContext *ibuscontext,
    guint keyval,
    guint keycode,
    guint state,
    IBusIMContext *ibusimcontext) {
  IDEBUG("%s", __FUNCTION__);
  g_debug("_ibus_context_forward_key_event_cb");
  if (keycode == 0 && ibusimcontext->client_window) {
    GdkDisplay *display = gdk_window_get_display(ibusimcontext->client_window);
    GdkKeymap *keymap   = gdk_keymap_get_for_display(display);
    GdkKeymapKey *keys  = NULL;
    gint n_keys         = 0;
    if (gdk_keymap_get_entries_for_keyval(keymap, keyval, &keys, &n_keys))
      keycode = keys->keycode;
    else
      g_warning("Failed to parse keycode from keyval %x", keyval);
  }
  GdkEventKey *event = _create_gdk_event(ibusimcontext, keyval, keycode, state);
  ibus_im_context_filter_keypress(ibusimcontext, event);

  if (!surrounding_text_supported && keyval == IBUS_KEY_BackSpace && ibusimcontext->text->len > 0) {
    int index = ibusimcontext->text->len - 1;
    int len   = 1;
    do {
      guchar c = (guchar)ibusimcontext->text->str[index];
      // delete a single UTF-8 codepoint, code unit (byte) by code unit
      if (c <= 0x7F) { // ASCII character
        break;
      } else if (c >= 0x80 && c <= 0xBF) { // Trailing byte of multi-byte character
        index--;
        len++;
      } else if (c >= 0xC2 && c <= 0xF4) { // First byte of 2-/3-/4-byte character
        break;
      } else {
        g_error("Illegal code unit value 0x%2x in %s", c, ibusimcontext->text->str);
        g_test_fail();
      }
    } while (index >= 0);
    g_string_erase(ibusimcontext->text, index, len);
  }

  gdk_event_free((GdkEvent *)event);
}

static void
_ibus_context_delete_surrounding_text_cb(
    IBusInputContext *ibuscontext,
    gint offset_from_cursor,
    guint nchars,
    IBusIMContext *ibusimcontext) {
  IDEBUG("%s: offset %d, nchar: %d", __FUNCTION__, offset_from_cursor, nchars);
  _delete_surrounding(ibusimcontext, offset_from_cursor, nchars);
}

static void
_ibus_context_destroy_cb(IBusInputContext *ibuscontext, IBusIMContext *ibusimcontext) {
  IDEBUG("%s", __FUNCTION__);
  g_assert(ibusimcontext->ibuscontext == ibuscontext);

  g_object_unref(ibusimcontext->ibuscontext);
  ibusimcontext->ibuscontext = NULL;
}

static gboolean
_timeout_callback(gpointer *user_data) {
  timeout_data *data = (timeout_data *)user_data;
  g_main_loop_quit(data->thread_loop);
  data->timed_out = TRUE;
  return FALSE;
}

static void
_run_main_loop_with_timeout(IBusIMContext *ibusimcontext) {
  timeout_data data;
  data.thread_loop = ibusimcontext->thread_loop;
  data.timed_out   = FALSE;
  int timeout_id   = g_timeout_add(1000 /*ms*/, (GSourceFunc)_timeout_callback, &data);
  g_main_loop_run(ibusimcontext->thread_loop);
  if (!data.timed_out) {
    g_source_remove(timeout_id);
  }
}

static void
_create_input_context(IBusIMContext *ibusimcontext) {
  IDEBUG("%s", __FUNCTION__);

  g_assert(ibusimcontext->ibuscontext == NULL);

  IBusInputContext *context = ibus_bus_create_input_context(_bus, "gtk-im");
  if (context == NULL) {
    g_warning("Create input context failed.");
  } else {
    ibusimcontext->ibuscontext = context;

    g_signal_connect(ibusimcontext->ibuscontext, "commit-text", G_CALLBACK(_ibus_context_commit_text_cb), ibusimcontext);
    g_signal_connect(
        ibusimcontext->ibuscontext, "forward-key-event", G_CALLBACK(_ibus_context_forward_key_event_cb), ibusimcontext);
    g_signal_connect(
        ibusimcontext->ibuscontext, "delete-surrounding-text", G_CALLBACK(_ibus_context_delete_surrounding_text_cb),
        ibusimcontext);
    g_signal_connect(ibusimcontext->ibuscontext, "destroy", G_CALLBACK(_ibus_context_destroy_cb), ibusimcontext);

    ibus_input_context_set_capabilities(ibusimcontext->ibuscontext, ibusimcontext->caps);

    if (ibusimcontext->has_focus) {
      /* The time order is _create_input_context() ->
       * ibus_im_context_notify() -> ibus_im_context_focus_in() ->
       * _create_input_context_done()
       * so _set_content_type() is called at the beginning here
       * because ibusimcontext->ibuscontext == NULL before. */
      _set_content_type(ibusimcontext);

      ibus_input_context_focus_in(ibusimcontext->ibuscontext);
      _set_cursor_location_internal(ibusimcontext);
    }

    if (!g_queue_is_empty(ibusimcontext->events_queue)) {
      GdkEventKey *event;
      while ((event = g_queue_pop_head(ibusimcontext->events_queue))) {
        _process_key_event(context, event, ibusimcontext);
        gboolean result = _process_key_event(context, event, ibusimcontext);
        if (result) {
          _run_main_loop_with_timeout(ibusimcontext);
        }
        gdk_event_free((GdkEvent *)event);
      }
    }
  }
}

void
ibus_im_test_set_thread_loop(IBusIMContext *context, GMainLoop *loop) {
  context->thread_loop = loop;
}

void
ibus_im_test_set_text(IBusIMContext *context, const gchar *text) {
  if (!context->text) {
    context->text = g_string_new("");
  }

  g_string_append(context->text, text);
  ibus_im_context_set_surrounding_with_selection(
      (GtkIMContext *)context, context->text->str, context->text->len, context->text->len, context->text->len);
}

const gchar *
ibus_im_test_get_text(IBusIMContext *context) {
  if (context->text) {
    return context->text->str;
  }
  return NULL;
}

void
ibus_im_test_clear_text(IBusIMContext *context) {
  if (context->text) {
    g_string_free(context->text, TRUE);
    context->text = NULL;
  }
}

void ibus_im_test_set_surrounding_text_supported(gboolean supported) {
  surrounding_text_supported = supported;
}
