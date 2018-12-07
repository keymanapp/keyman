/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2009-2018 SIL International
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
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <keyman/keyboardprocessor.h>

#include "keymanutil.h"
#include "keyman-service.h"
#include "engine.h"

#define Keyman_Pass_Backspace_To_IBus 254 // unused keycode to forward backspace back to ibus

typedef struct _IBusKeymanEngine IBusKeymanEngine;
typedef struct _IBusKeymanEngineClass IBusKeymanEngineClass;

struct _IBusKeymanEngine {
	IBusEngine parent;

    /* members */
    km_kbp_keyboard *keyboard;
    km_kbp_state    *state;
    gchar           *ldmlfile;
    gchar           *kb_name;
    IBusLookupTable *table;
    IBusProperty    *status_prop;
    IBusPropList    *prop_list;
    Display         *display;

};

struct _IBusKeymanEngineClass {
	IBusEngineClass parent;
};

/* functions prototype */
static void	ibus_keyman_engine_class_init	    (IBusKeymanEngineClass    *klass);
static void	ibus_keyman_engine_init		    (IBusKeymanEngine		    *kmfl);
static GObject*
            ibus_keyman_engine_constructor    (GType                   type,
                                             guint                   n_construct_params,
                                             GObjectConstructParam  *construct_params);
static void	ibus_keyman_engine_destroy		(IBusKeymanEngine		    *kmfl);
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
                                            (IBusKeymanEngine         *kmfl,
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
    //engine_class->set_cursor_location = ibus_keyman_engine_set_cursor_location;


    engine_class->focus_in = ibus_keyman_engine_focus_in;
    engine_class->focus_out = ibus_keyman_engine_focus_out;

    // engine_class->page_up = ibus_keyman_engine_page_up;
    // engine_class->page_down = ibus_keyman_engine_page_down;

    // engine_class->cursor_up = ibus_keyman_engine_cursor_up;
    // engine_class->cursor_down = ibus_keyman_engine_cursor_down;

    engine_class->property_activate = ibus_keyman_engine_property_activate;
}

static void read_context(IBusEngine *engine)
{
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;
    IBusText *text;
    gchar *surrounding_text;
    guint cursor_pos, anchor_pos;
    km_kbp_context_item *context_items;

    if ((engine->client_capabilities & IBUS_CAP_SURROUNDING_TEXT) != 0)
    {
        g_message("reading context");
        ibus_engine_get_surrounding_text(engine, &text, &cursor_pos, &anchor_pos);
        surrounding_text = g_utf8_substring(ibus_text_get_text(text), 0, cursor_pos);
        g_message("new context is:%s", surrounding_text);
        if (km_kbp_context_items_from_utf8(surrounding_text, &context_items) == KM_KBP_STATUS_OK) {
            km_kbp_context_set(km_kbp_state_context(keyman->state), context_items);
        }
        km_kbp_context_items_dispose(context_items);
        g_free(surrounding_text);
    }
}

static void
ibus_keyman_engine_init (IBusKeymanEngine *kmfl)
{
    kmfl->status_prop = ibus_property_new ("status",
                                           PROP_TYPE_NORMAL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           TRUE,
                                           FALSE,
                                           0,
                                           NULL);
    g_object_ref_sink(kmfl->status_prop);
    kmfl->prop_list = ibus_prop_list_new ();
    g_object_ref_sink(kmfl->prop_list);
    ibus_prop_list_append (kmfl->prop_list,  kmfl->status_prop);

    kmfl->table = ibus_lookup_table_new (9, 0, TRUE, TRUE);
    g_object_ref_sink(kmfl->table);
    kmfl->state = NULL;
}

static GObject*
ibus_keyman_engine_constructor (GType                   type,
                              guint                   n_construct_params,
                              GObjectConstructParam  *construct_params)
{
    IBusKeymanEngine *keyman;
    IBusEngine *engine;
    IBusText *text;
    const gchar *engine_name;
    gchar *surrounding_text, *p, *abs_kmx_path;
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

    km_kbp_option_item options[1] = {KM_KBP_OPTIONS_END};

    km_kbp_status status_keyboard = km_kbp_keyboard_load(abs_kmx_path, &(keyman->keyboard));
    g_free(abs_kmx_path);

    if (status_keyboard != KM_KBP_STATUS_OK)
    {
        g_warning("problem creating km_kbp_keyboard");
    }

    km_kbp_status status_state = km_kbp_state_create(keyman->keyboard,
                                  options,
                                  &(keyman->state));
    if (status_state != KM_KBP_STATUS_OK)
    {
        g_warning("problem creating km_kbp_state");
    }

    read_context(engine);

    keyman->display  = XOpenDisplay(NULL);
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

    if (keyman->display) {
        XCloseDisplay(keyman->display);
        keyman->display = NULL;
    }
    g_free(keyman->kb_name);
    g_free(keyman->ldmlfile);
     
    IBUS_OBJECT_CLASS (parent_class)->destroy ((IBusObject *)keyman);
}

static void
ibus_keyman_engine_commit_string (IBusKeymanEngine *kmfl,
                                const gchar    *string)
{
    IBusText *text;
    g_message("DAR: ibus_keyman_engine_commit_string - %s", string);
    text = ibus_text_new_from_static_string (string);
    g_object_ref_sink(text);
    ibus_engine_commit_text ((IBusEngine *)kmfl, text);
    g_object_unref (text);
}

int is_key_pressed(Display * display, char *key_vec, KeySym keysym)
{
    unsigned char keycode;
    keycode = XKeysymToKeycode(display, keysym);
    // TE: Some documentation on the format of keycode might be helpful
    // DG: Indeed
    return key_vec[keycode >> 3] & (1 << (keycode & 7));
}

static void forward_keysym(IBusKeymanEngine *engine, unsigned int keysym, unsigned int state)
{
    unsigned char keycode;
    g_debug("DAR: forward_keysym");
    keycode = XKeysymToKeycode(engine->display, keysym);
    ibus_engine_forward_key_event((IBusEngine *)engine, keysym, keycode-8, state);
}

static void forward_keycode(IBusKeymanEngine *engine, unsigned char keycode, unsigned int state)
{
    g_debug("DAR: forward_keycode no keysym");
    ibus_engine_forward_key_event((IBusEngine *)engine, 0, keycode, state);
}


// from android/KMEA/app/src/main/java/com/tavultesoft/kmea/KMHardwareKeyboardInterpreter.java
// uses kernel keycodes which are X11 keycode - 8
//private static final
//  int scanCodeMap[] = {
static km_kbp_virtual_key const keycode_to_vk[256] = {
    0,      //        padding = 0x00;
    KM_KBP_VKEY_ESC,      //        public static final int KEY_ESC = 0x01;
    KM_KBP_VKEY_1,    //        public static final int KEY_1 = 0x02;
    KM_KBP_VKEY_2,    //        public static final int KEY_2 = 0x03;
    KM_KBP_VKEY_3,    //        public static final int KEY_3 = 0x04;
    KM_KBP_VKEY_4,    //        public static final int KEY_4 = 0x05;
    KM_KBP_VKEY_5,    //        public static final int KEY_5 = 0x06;
    KM_KBP_VKEY_6,    //        public static final int KEY_6 = 0x07;
    KM_KBP_VKEY_7,    //        public static final int KEY_7 = 0x08;
    KM_KBP_VKEY_8,    //        public static final int KEY_8 = 0x09;
    KM_KBP_VKEY_9,    //        public static final int KEY_9 = 0x0A;
    KM_KBP_VKEY_0,    //        public static final int KEY_0 = 0x0B;
    KM_KBP_VKEY_HYPHEN,    //        public static final int KEY_MINUS = 0x0C;
    KM_KBP_VKEY_EQUAL,    //        public static final int KEY_EQUALS = 0x0D;
    KM_KBP_VKEY_BKSP,      //        public static final int KEY_BACKSPACE = 0x0E;
    KM_KBP_VKEY_TAB,      //        public static final int KEY_TAB = 0x0F;
    KM_KBP_VKEY_Q,    //        public static final int KEY_Q = 0x10;
    KM_KBP_VKEY_W,    //        public static final int KEY_W = 0x11;
    KM_KBP_VKEY_E,    //        public static final int KEY_E = 0x12;
    KM_KBP_VKEY_R,    //        public static final int KEY_R = 0x13;
    KM_KBP_VKEY_T,    //        public static final int KEY_T = 0x14;
    KM_KBP_VKEY_Y,    //        public static final int KEY_Y = 0x15;
    KM_KBP_VKEY_U,    //        public static final int KEY_U = 0x16;
    KM_KBP_VKEY_I,    //        public static final int KEY_I = 0x17;
    KM_KBP_VKEY_O,    //        public static final int KEY_O = 0x18;
    KM_KBP_VKEY_P,    //        public static final int KEY_P = 0x19;
    KM_KBP_VKEY_LBRKT,    //        public static final int KEY_LEFTBRACE = 0x1A;
    KM_KBP_VKEY_RBRKT,    //        public static final int KEY_RIGHTBRACE = 0x1B;
    KM_KBP_VKEY_ENTER,     //        public static final int KEY_ENTER = 0x1C;
    0,      //        public static final int KEY_LEFTCTRL = 0x1D;
    KM_KBP_VKEY_A,    //        public static final int KEY_A = 0x1E;
    KM_KBP_VKEY_S,    //        public static final int KEY_S = 0x1F;
    KM_KBP_VKEY_D,    //        public static final int KEY_D = 0x20;
    KM_KBP_VKEY_F,    //        public static final int KEY_F = 0x21;
    KM_KBP_VKEY_G,    //        public static final int KEY_G = 0x22;
    KM_KBP_VKEY_H,    //        public static final int KEY_H = 0x23;
    KM_KBP_VKEY_J,    //        public static final int KEY_J = 0x24;
    KM_KBP_VKEY_K,    //        public static final int KEY_K = 0x25;
    KM_KBP_VKEY_L,    //        public static final int KEY_L = 0x26;
    KM_KBP_VKEY_COLON,    //        public static final int KEY_SEMICOLON = 0x27;
    KM_KBP_VKEY_QUOTE,    //        public static final int KEY_APOSTROPHE = 0x28;
    KM_KBP_VKEY_BKQUOTE,    //        public static final int KEY_GRAVE = 0x29;
    0,      //        public static final int KEY_LEFTSHIFT = 0x2A;
    KM_KBP_VKEY_BKSLASH,    //        public static final int KEY_BACKSLASH = 0x2B;
    KM_KBP_VKEY_Z,    //        public static final int KEY_Z = 0x2C;
    KM_KBP_VKEY_X,    //        public static final int KEY_X = 0x2D;
    KM_KBP_VKEY_C,    //        public static final int KEY_C = 0x2E;
    KM_KBP_VKEY_V,    //        public static final int KEY_V = 0x2F;
    KM_KBP_VKEY_B,    //        public static final int KEY_B = 0x30;
    KM_KBP_VKEY_N,    //        public static final int KEY_N = 0x31;
    KM_KBP_VKEY_M,    //        public static final int KEY_M = 0x32;
    KM_KBP_VKEY_COMMA,    //        public static final int KEY_COMMA = 0x33;
    KM_KBP_VKEY_PERIOD,    //        public static final int KEY_DOT = 0x34;
    KM_KBP_VKEY_SLASH,    //        public static final int KEY_SLASH = 0x35;
    0,      //        public static final int KEY_RIGHTSHIFT = 0x36;
    KM_KBP_VKEY_NPSTAR,      //        public static final int KEY_KPASTERISK = 0x37;
    0,      //        public static final int KEY_LEFTALT = 0x38;
    KM_KBP_VKEY_SPACE,     //        public static final int KEY_SPACE = 0x39;
    0,      //        public static final int KEY_CAPSLOCK = 0x3A;
    KM_KBP_VKEY_F1,      //        public static final int KEY_F1 = 0x3B;
    KM_KBP_VKEY_F2,      //        public static final int KEY_F2 = 0x3C;
    KM_KBP_VKEY_F3,      //        public static final int KEY_F3 = 0x3D;
    KM_KBP_VKEY_F4,      //        public static final int KEY_F4 = 0x3E;
    KM_KBP_VKEY_F5,      //        public static final int KEY_F5 = 0x3F;
    KM_KBP_VKEY_F6,      //        public static final int KEY_F6 = 0x40;
    KM_KBP_VKEY_F7,      //        public static final int KEY_F7 = 0x41;
    KM_KBP_VKEY_F8,      //        public static final int KEY_F8 = 0x42;
    KM_KBP_VKEY_F9,      //        public static final int KEY_F9 = 0x43;
    KM_KBP_VKEY_F10,      //        public static final int KEY_F10 = 0x44;
    0,      //        public static final int KEY_NUMLOCK = 0x45;
    0,      //        public static final int KEY_SCROLLLOCK = 0x46;
    KM_KBP_VKEY_NP7,      //        public static final int KEY_KP7 = 0x47;
    KM_KBP_VKEY_NP8,      //        public static final int KEY_KP8 = 0x48;
    KM_KBP_VKEY_NP9,      //        public static final int KEY_KP9 = 0x49;
    KM_KBP_VKEY_NPMINUS,      //        public static final int KEY_KPMINUS = 0x4A;
    KM_KBP_VKEY_NP4,      //        public static final int KEY_KP4 = 0x4B;
    KM_KBP_VKEY_NP5,      //        public static final int KEY_KP5 = 0x4C;
    KM_KBP_VKEY_NP6,      //        public static final int KEY_KP6 = 0x4D;
    KM_KBP_VKEY_NPPLUS,      //        public static final int KEY_KPPLUS = 0x4E;
    KM_KBP_VKEY_NP1,      //        public static final int KEY_KP1 = 0x4F;
    KM_KBP_VKEY_NP2,      //        public static final int KEY_KP2 = 0x50;
    KM_KBP_VKEY_NP3,      //        public static final int KEY_KP3 = 0x51;
    KM_KBP_VKEY_NP0,      //        public static final int KEY_KP0 = 0x52;
    KM_KBP_VKEY_NPDOT,      //        public static final int KEY_KPDOT = 0x53;
    0,      //        padding 0x54;
    0,      //        public static final int KEY_ZENKAKUHANKAKU = 0x55;
    KM_KBP_VKEY_oE2,     //        public static final int KEY_102ND = 0x56;
    // additional on linux
    KM_KBP_VKEY_F11,      //        public static final int KEY_F11 = 0x57;
    KM_KBP_VKEY_F12      //        public static final int KEY_F12 = 0x58;

    // Many more KEYS currently not used by KMW...
  };

static void reset_context(IBusEngine *engine)
{
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;

    g_message("reset_context");
    km_kbp_context_clear(km_kbp_state_context(keyman->state));
    read_context(engine);
}

static gboolean
ibus_keyman_engine_process_key_event (IBusEngine     *engine,
                                    guint           keyval,
                                    guint           keycode,
                                    guint           state)
{
    IBusKeymanEngine *keyman = (IBusKeymanEngine *) engine;
    
    if (state & IBUS_RELEASE_MASK)
        return FALSE;

    if (keycode < 0 || keycode > 255)
    {
        g_warning("keycode %d out of range", keycode);
        return FALSE;
    }

    // keyman modifiers are different from X11/ibus
    uint16_t km_mod_state = 0;
    if (state & IBUS_SHIFT_MASK)
    {
        km_mod_state |= KM_KBP_MODIFIER_SHIFT;
    }
    if (state & IBUS_MOD5_MASK)
    {
        km_mod_state |= KM_KBP_MODIFIER_RALT;
        g_message("modstate KM_KBP_MODIFIER_RALT from IBUS_MOD5_MASK");
    }
    // if (state & IBUS_MOD1_MASK)
    // {
    //     km_mod_state |= KM_KBP_MODIFIER_ALT;
    //     g_message("modstate KM_KBP_MODIFIER_ALT");
    // }
    // if (state & IBUS_CONTROL_MASK) {
    //     km_mod_state |= KM_KBP_MODIFIER_CTRL;
    //     g_message("modstate KM_KBP_MODIFIER_CTRL");
    // }

    g_message("DAR: ibus_keyman_engine_process_key_event - keyval=%02i, keycode=%02i, state=%02x", keyval, keycode, state);

    // // Let the application handle user generated backspaces after resetting the kmfl history
    if (keycode == Keyman_Pass_Backspace_To_IBus) {
        //clear_history(keyman->state);
        g_message("IBUS_BackSpace");
        return FALSE;
    }

    if (keycode_to_vk[keycode] == 0) // key we don't handle
        return FALSE;

    // If a modifier key is pressed, check to see if it is a right modifier key
    // This is rather expensive so only do it if a shift state is active
    if (state & (/*IBUS_SHIFT_MASK |*/ IBUS_CONTROL_MASK | IBUS_MOD1_MASK)) {
        Display * m_display  = XOpenDisplay(NULL);;
        char key_vec[32];
        XQueryKeymap(m_display, key_vec);

        if (state & IBUS_MOD1_MASK) {
            if (is_key_pressed(m_display, key_vec, IBUS_Alt_R)) {
                km_mod_state |= KM_KBP_MODIFIER_RALT;
                g_message("modstate KM_KBP_MODIFIER_RALT from IBUS_Alt_R");
            }
            else {
                km_mod_state |= KM_KBP_MODIFIER_LALT;
                g_message("modstate KM_KBP_MODIFIER_LALT");
            }
        }

        if (state & IBUS_CONTROL_MASK) {
            if (is_key_pressed(m_display, key_vec, IBUS_Control_R)) {
                km_mod_state |= KM_KBP_MODIFIER_RCTRL;
                g_message("modstate KM_KBP_MODIFIER_RCTRL");
            }
            else {
                km_mod_state |= KM_KBP_MODIFIER_LCTRL;
                g_message("modstate KM_KBP_MODIFIER_LCTRL");
            }
        }
        // ignoring right shift
        // if ((state & IBUS_SHIFT_MASK) && is_key_pressed(m_display, key_vec, IBUS_Shift_R)) {
        //     right_modifier_state |= (IBUS_SHIFT_MASK << 8);
        // }
        XCloseDisplay(m_display);
    }
    g_message("DAR: ibus_keyman_engine_process_key_event - km_mod_state=%x", km_mod_state);
    km_kbp_status event_status = km_kbp_process_event(keyman->state,
                                   keycode_to_vk[keycode], km_mod_state);

    // km_kbp_state_action_items to get action items
    size_t num_action_items;
    gchar utf8[12];
    gint numbytes;
    const km_kbp_action_item *action_items = km_kbp_state_action_items(keyman->state,
                                                     &num_action_items);

    for (int i = 0; i < num_action_items; i++)
    {
        switch(action_items[i].type)
        {
            case KM_KBP_IT_CHAR:
                numbytes = g_unichar_to_utf8(action_items[i].character, utf8);
                if (numbytes > 12)
                {
                    g_error("g_unichar_to_utf8 overflowing buffer");
                }
                if (numbytes)
                {
                    utf8[numbytes] = 0;
                }
                g_message("CHAR action unichar:U+%04x, bytes:%d, string:%s", action_items[i].character, numbytes, utf8);
                ibus_keyman_engine_commit_string(keyman, utf8);
                break;
            case KM_KBP_IT_MARKER:
                g_message("MARKER action");
                break;
            case KM_KBP_IT_ALERT:
                g_message("ALERT action");
                break;
            case KM_KBP_IT_BACK:
                g_message("BACK action");
                g_message("DAR: ibus_keyman_engine_process_key_event - client_capabilities=%x, %x", engine->client_capabilities,  IBUS_CAP_SURROUNDING_TEXT);

                if ((engine->client_capabilities & IBUS_CAP_SURROUNDING_TEXT) != 0) {
                    g_message("deleting surrounding text 1 char");
                    ibus_engine_delete_surrounding_text(engine, -1, 1);
                } else {
                    g_message("forwarding backspace");
                    forward_keycode(keyman, Keyman_Pass_Backspace_To_IBus, 0);
                }
                break;
            case KM_KBP_IT_PERSIST_OPT:
                g_message("PERSIST_OPT action");
                break;
            case KM_KBP_IT_EMIT_KEYSTROKE:
                g_message("EMIT_KEYSTROKE action");
                return False;
            case KM_KBP_IT_INVALIDATE_CONTEXT:
                g_message("INVALIDATE_CONTEXT action");
                reset_context(engine);
                break;
            case KM_KBP_IT_END:
                g_message("END action");
                break;
            default:
                g_warning("Unknown action");
        }
    }
    return TRUE;
 }

static void
ibus_keyman_engine_set_surrounding_text (IBusEngine *engine,
                                            IBusText    *text,
                                            guint       cursor_pos,
                                            guint       anchor_pos)
{
    g_message("ibus_keyman_engine_set_surrounding_text");
    parent_class->set_surrounding_text (engine, text, cursor_pos, anchor_pos);
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

