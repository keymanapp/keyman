/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2009 SIL International
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

typedef struct _IBusKeymanEngine IBusKeymanEngine;
typedef struct _IBusKeymanEngineClass IBusKeymanEngineClass;

struct _IBusKeymanEngine {
	IBusEngine parent;

    /* members */
    km_kbp_keyboard *keyboard;
    km_kbp_state    *state;
    km_kbp_option   *options; // array of km_kbp_option
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
static void ibus_engine_set_cursor_location (IBusEngine             *engine,
                                             gint                    x,
                                             gint                    y,
                                             gint                    w,
                                             gint                    h);
static void ibus_keyman_engine_set_capabilities
                                            (IBusEngine             *engine,
                                             guint                   caps);
static void ibus_keyman_engine_page_up        (IBusEngine             *engine);
static void ibus_keyman_engine_page_down      (IBusEngine             *engine);
static void ibus_keyman_engine_cursor_up      (IBusEngine             *engine);
static void ibus_keyman_engine_cursor_down    (IBusEngine             *engine);
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
static GHashTable      *im_table = NULL;


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

    engine_class->focus_in = ibus_keyman_engine_focus_in;
    engine_class->focus_out = ibus_keyman_engine_focus_out;

    engine_class->page_up = ibus_keyman_engine_page_up;
    engine_class->page_down = ibus_keyman_engine_page_down;

    engine_class->cursor_up = ibus_keyman_engine_cursor_up;
    engine_class->cursor_down = ibus_keyman_engine_cursor_down;

    engine_class->property_activate = ibus_keyman_engine_property_activate;
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
//    kmfl->context = NULL;
}

static GObject*
ibus_keyman_engine_constructor (GType                   type,
                              guint                   n_construct_params,
                              GObjectConstructParam  *construct_params)
{
    IBusKeymanEngine *keyman;
    KInputMethod *im;
    const gchar *engine_name;
    
    g_debug("DAR: ibus_keyman_engine_constructor");
    
    keyman = (IBusKeymanEngine *) G_OBJECT_CLASS (parent_class)->constructor (type,
                                                       n_construct_params,
                                                       construct_params);

    engine_name = ibus_engine_get_name ((IBusEngine *) keyman);
    g_assert (engine_name);
    g_message("DAR: ibus_keyman_engine_constructor %s", engine_name);

    if (im_table == NULL) {
        im_table = g_hash_table_new_full (g_str_hash,
                                          g_str_equal,
                                          g_free,
                                          (GDestroyNotify) kinput_close_im);
    }

    im = (KInputMethod *) g_hash_table_lookup (im_table, engine_name);

    if (im == NULL) {
        im = kinput_open_im(engine_name);
        
        if (im != NULL)
            g_hash_table_insert (im_table, g_strdup (engine_name), im);

    }

    if (im == NULL) {
        g_warning ("Can not find Keyman keymap %s", engine_name);
        g_object_unref (keyman);
        return NULL;
    }

    keyman->options = g_new(km_kbp_option, 1); 
    //keyman->options[0] = KM_KBP_OPTIONS_END; ???

    km_kbp_status status_keyboard = km_kbp_keyboard_load(engine_name, &(keyman->keyboard));

    km_kbp_status status_state = km_kbp_state_create(keyman->keyboard,
                                  keyman->options,
                                  &(keyman->state));

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

    g_hash_table_remove(im_table, engine_name);
     
    IBUS_OBJECT_CLASS (parent_class)->destroy ((IBusObject *)keyman);
}

static void
ibus_keyman_engine_commit_string (IBusKeymanEngine *kmfl,
                                const gchar    *string)
{
    IBusText *text;
    g_debug("DAR: ibus_keyman_engine_commit_string - %s", string);
    text = ibus_text_new_from_static_string (string);
    g_object_ref_sink(text);
    ibus_engine_commit_text ((IBusEngine *)kmfl, text);
    g_object_unref (text);
}

int is_key_pressed(Display * display, char *key_vec, KeySym keysym)
{
    unsigned char keycode;
    keycode = XKeysymToKeycode(display, keysym);
    return key_vec[keycode >> 3] & (1 << (keycode & 7));
}

static void forward_key(IBusKeymanEngine *engine, unsigned int keysym, unsigned int state)
{
    unsigned char keycode;
    g_debug("DAR: forward_key");
    keycode = XKeysymToKeycode(engine->display, keysym);
    ibus_engine_forward_key_event((IBusEngine *)engine, keysym, keycode-8, state);
 }

static gboolean
ibus_keyman_engine_process_key_event (IBusEngine     *engine,
                                    guint           keyval,
                                    guint           keycode,
                                    guint           state)
{
    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;
    #if 0
    
    if (state & IBUS_RELEASE_MASK)
        return FALSE;

    g_debug("DAR: ibus_keyman_engine_process_key_event - keyval=%x, keycode=%x, state=%x", keyval, keycode, state);
    
    // If a modifier key is pressed, check to see if it is a right modifier key
    // This is rather expensive so only do it if a shift state is active
    int right_modifier_state = 0;    
    if (state & (IBUS_SHIFT_MASK | IBUS_CONTROL_MASK | IBUS_MOD1_MASK)) {
        Display * m_display  = XOpenDisplay(NULL);;
        char key_vec[32];
        XQueryKeymap(m_display, key_vec);

        if ((state & IBUS_MOD1_MASK) && is_key_pressed(m_display, key_vec, IBUS_Alt_R)) {
            right_modifier_state |= (IBUS_MOD1_MASK << 8);
        }

        if ((state & IBUS_CONTROL_MASK) && is_key_pressed(m_display, key_vec, IBUS_Control_R)) {
            right_modifier_state |= (IBUS_CONTROL_MASK << 8);
        }

        if ((state & IBUS_SHIFT_MASK) && is_key_pressed(m_display, key_vec, IBUS_Shift_R)) {
            right_modifier_state |= (IBUS_SHIFT_MASK << 8);
        }
        XCloseDisplay(m_display);
    }
    g_debug("DAR: ibus_keyman_engine_process_key_event - right_modifier_state=%x", right_modifier_state);
    // Let the application handle user generated backspaces after resetting the kmfl history
    if (keyval == IBUS_BackSpace) {
        clear_history(kmfl->context->p_kmsi);
    } else if (kmfl_interpret(kmfl->context->p_kmsi, keyval, state | right_modifier_state) == 1) {
        GList * p;
        for (p=kmfl->context->cmds; p != NULL; p = p->next) {
            Cmd * cmd = (Cmd *) p->data;
            if (cmd) {
                switch (cmd->opcode) {
                    case OUTPUT_STRING:
                        ibus_keyman_engine_commit_string(kmfl, cmd->cmdarg);
                        break;
                    case ERASE_CHAR:
		        g_debug("DAR: ibus_keyman_engine_process_key_event - client_capabilities=%x, %x, %x", engine->client_capabilities,  IBUS_CAP_SURROUNDING_TEXT, engine->client_capabilities & IBUS_CAP_SURROUNDING_TEXT);

                        if ((engine->client_capabilities & IBUS_CAP_SURROUNDING_TEXT) != 0) {
                            ibus_engine_delete_surrounding_text(engine, -1, 1);
                            if ((engine->client_capabilities & IBUS_CAP_SURROUNDING_TEXT) == 0) {
                                forward_key(kmfl, IBUS_BackSpace, 0);
                            }
                        } else 
                            forward_key(kmfl, IBUS_BackSpace, 0);
                        break;
                    case FORWARD_KEYEVENT:
                        break;
                    case OUTPUT_BEEP:
                        break;
                }
                if (cmd->cmdarg)
                    g_free(cmd->cmdarg);
                g_free(cmd);
            }
        }
        g_list_free(kmfl->context->cmds);
        kmfl->context->cmds=NULL;
        return TRUE;
    }
    #endif
    
    return FALSE;
 }

static void
ibus_keyman_engine_focus_in (IBusEngine *engine)
{
    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;

    ibus_engine_register_properties (engine, kmfl->prop_list);

    parent_class->focus_in (engine);
}

static void
ibus_keyman_engine_focus_out (IBusEngine *engine)
{
    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;

    parent_class->focus_out (engine);
}

static void
ibus_keyman_engine_reset (IBusEngine *engine)
{
    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;

    parent_class->reset (engine);
    ibus_keyman_engine_focus_in (engine);
}

static void
ibus_keyman_engine_enable (IBusEngine *engine)
{
    const gchar *engine_name;
    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;
    KInputMethod *im;

    engine_name = ibus_engine_get_name (engine);
    g_assert (engine_name);
    g_message("WDG: ibus_keyman_engine_enable %s", engine_name);
    im = (KInputMethod *) g_hash_table_lookup (im_table, engine_name);
    // own dbus name com.Keyman
    // expose properties LDMLFile and Name
    KeymanService *service = km_service_get_default();
    //const gchar *ldmlfile = "";
    km_service_set_ldmlfile (service, im->keyboard_ldmlfile);
    km_service_set_name (service, im->keyboard_name);
    parent_class->enable (engine);
}

static void
ibus_keyman_engine_disable (IBusEngine *engine)
{
    const gchar *engine_name;
    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;

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

static void
ibus_keyman_engine_page_up (IBusEngine *engine)
{
    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;

    parent_class->page_up (engine);
}

static void
ibus_keyman_engine_page_down (IBusEngine *engine)
{

    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;

    parent_class->page_down (engine);
}

static void
ibus_keyman_engine_cursor_up (IBusEngine *engine)
{

    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;

    parent_class->cursor_up (engine);
}

static void
ibus_keyman_engine_cursor_down (IBusEngine *engine)
{

    IBusKeymanEngine *kmfl = (IBusKeymanEngine *) engine;

    parent_class->cursor_down (engine);
}

static void
ibus_keyman_engine_property_activate (IBusEngine  *engine,
                                    const gchar *prop_name,
                                    guint        prop_state)
{
    parent_class->property_activate (engine, prop_name, prop_state);
}

