/** @file scim_kmfl_imengine.cpp
 * implementation of class KmflInstance.
 */

/*
 * KMFL Input Method for SCIM (Smart Common Input Method)
 *
 * Copyright (C) 2005 SIL International
 * based on source from SCIM Copyright (c) 2004 James Su <suzhe@tsinghua.org.cn>
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

#include <stdarg.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <kmfl/kmfl.h>
#include <kmfl/libkmfl.h>
#include "xkbmap.h"

#define Uses_SCIM_IMENGINE
#define Uses_SCIM_ICONV
#define Uses_SCIM_CONFIG_BASE
#define Uses_SCIM_CONFIG_PATH
#include "scim_kmfl_imengine_private.h"

#include <scim.h>
#include <queue>
#include "scim_kmfl_imengine.h"

#define scim_module_init kmfl_LTX_scim_module_init
#define scim_module_exit kmfl_LTX_scim_module_exit
#define scim_imengine_module_init kmfl_LTX_scim_imengine_module_init
#define scim_imengine_module_create_factory kmfl_LTX_scim_imengine_module_create_factory

#ifndef SCIM_KMFL_IMENGINE_MODULE_DATADIR
#define SCIM_KMFL_IMENGINE_MODULE_DATADIR "/usr/share/scim/kmfl"
#endif
#define SCIM_KMFL_MAX_KEYBOARD_NUMBER  64

#define KEY_AltRMask 0x10;

#define COMMIT_KEYCODE 0xFFFE
#define COMMIT_KEYMASK 0x0F

using namespace scim;
static unsigned int _scim_number_of_keyboards = 0;

static Pointer < KmflFactory >
    _scim_kmfl_imengine_factories[SCIM_KMFL_MAX_KEYBOARD_NUMBER];

static std::vector < String > _scim_system_keyboard_list;

static std::vector < String > _scim_user_keyboard_list;

static ConfigPointer _scim_config;

static Xkbmap xkbmap;

static const char *_DEFAULT_LOCALES = N_("en_US.UTF-8,"
                                         "en_AU.UTF-8,"
                                         "en_CA.UTF-8,"
                                         "en_GB.UTF-8,"
                                         "my_MM.UTF-8,"
                                         "zh_CN.UTF-8,zh_CN.GB18030,zh_CN.GBK,zh_CN.GB2312,zh_CN,"
                                         "zh_TW.UTF-8,zh_TW.Big5,zh_TW,"
                                         "zh_HK.UTF-8,zh_HK,"
                                         "ja_JP.UTF-8,ja_JP.eucJP,ja_JP.ujis,ja_JP,ja,"
                                         "ko_KR.UTF-8,ko_KR.eucKR,ko_KR");

static String get_dirname(const String & path)
{
    size_t dirend = path.find_last_of(SCIM_PATH_DELIM_STRING);

    if (dirend > 0) {
        return path.substr(0, dirend);
    } else {
        return String("");
    }
}

static void
_get_keyboard_list(std::vector < String > &keyboard_list,
                   const String & path)
{
    keyboard_list.clear();
    DIR *dir = opendir(path.c_str());

    if (dir != NULL) {
        struct dirent *file = readdir(dir);
        while (file != NULL) {
            struct stat filestat;
            String absfn = path + SCIM_PATH_DELIM_STRING + file->d_name;
            stat(absfn.c_str(), &filestat);


            // Only .kmfl and .kmn extensions are valid keyboard files
            if (S_ISREG(filestat.st_mode)
                && ((absfn.substr(absfn.length() - 5, 5) == ".kmfl" 
				     && kmfl_check_keyboard(absfn.c_str()) == 0) 
					|| absfn.substr(absfn.length() - 4, 4) == ".kmn")) {
                DBGMSG(1, "DAR: kmfl - found keyboard: %s\n",
                       absfn.c_str());

                keyboard_list.push_back(absfn);
            }

            file = readdir(dir);
        }
        closedir(dir);
    }
}

extern "C" {
    void scim_module_init(void) 
    {
#ifdef DEBUGGING
        kmfl_debug = 1;
#endif
        DBGMSG(1, "DAR/JD: kmfl - Kmfl Module init!!!\n");
    } 
    
    void scim_module_exit(void) 
    {
        DBGMSG(1, "DAR: kmfl - Kmfl Module exit\n");
        for (UINT i = 0; i < _scim_number_of_keyboards; ++i) {
            _scim_kmfl_imengine_factories[i].reset();
        }

        _scim_config.reset();
    }

    unsigned int scim_imengine_module_init(const ConfigPointer & config) 
    {
        DBGMSG(1, "DAR: kmfl - Kmfl IMEngine Module init\n");

        _scim_config = config;
        _get_keyboard_list(_scim_system_keyboard_list,
                           SCIM_KMFL_IMENGINE_MODULE_DATADIR);
        _get_keyboard_list(_scim_user_keyboard_list,
                           scim_get_home_dir() + SCIM_PATH_DELIM_STRING +
                           ".scim" + SCIM_PATH_DELIM_STRING + "kmfl");

        _scim_number_of_keyboards =
            _scim_system_keyboard_list.size() +
            _scim_user_keyboard_list.size();
        if (_scim_number_of_keyboards == 0) {
            DBGMSG(1, "DAR: kmfl - No valid keyboards found\n");
        }

        return _scim_number_of_keyboards;        // actually the number of files, may not all be valid 
    }

    IMEngineFactoryPointer scim_imengine_module_create_factory(unsigned int imengine) 
    {
        DBGMSG(1, "DAR: kmfl - Kmfl IMEngine Module Create Factory %d\n",
               imengine);

        if (imengine >= _scim_number_of_keyboards) {
            return 0;
        }

        if (_scim_kmfl_imengine_factories[imengine].null()) {
            _scim_kmfl_imengine_factories[imengine] = new KmflFactory();

            if (imengine < _scim_system_keyboard_list.size()) {
                if (!_scim_kmfl_imengine_factories[imengine]->
                    load_keyboard(_scim_system_keyboard_list[imengine],
                                  false))
					return 0;
            } else {
                if (!_scim_kmfl_imengine_factories[imengine]->
                    load_keyboard(_scim_user_keyboard_list
                                  [imengine -
                                   _scim_system_keyboard_list.size()],
                                  true))
					return 0;
            }

            if (!_scim_kmfl_imengine_factories[imengine]->valid()) {
                _scim_kmfl_imengine_factories[imengine].reset();
            }

            char buf[2];
            sprintf(buf, "%c", 21 + imengine);
            _scim_kmfl_imengine_factories[imengine]->
                set_uuid(String("d1534208-27e5-8ec4-b2cd-df0fb0d2275") +
                         String(buf));
        }
        return _scim_kmfl_imengine_factories[imengine];
    }
}

// Implementation of Kmfl
KmflFactory::KmflFactory()
{
    String current_locale = String (setlocale (LC_CTYPE, 0));
    
    if (current_locale.length() > 0) {
        set_locales(String(_(_DEFAULT_LOCALES)) + String(",") +
                current_locale);
    } else {
        set_locales(String(_(_DEFAULT_LOCALES)));
    }
}

KmflFactory::KmflFactory(const WideString & name,
                                     const String & locales)
{
    if (locales == String("default")) {
        String current_locale = String (setlocale (LC_CTYPE, 0));
        
        if (current_locale.length() > 0) {
            set_locales(String(_(_DEFAULT_LOCALES)) + String(",") +
                        current_locale);
        } else {
            set_locales(String(_(_DEFAULT_LOCALES)));
        }
    } else {
        set_locales(locales);
    }
}

KmflFactory::~KmflFactory()
{
    kmfl_unload_keyboard(m_keyboard_number);
}


bool KmflFactory::load_keyboard(const String & keyboard_file,
                                      bool user_keyboard)
{
    char buf[256];
    KMSI * p_kmsi;
    m_keyboard_file = keyboard_file;
    DBGMSG(1, "DAR/jd: kmfl loading %s\n", keyboard_file.c_str());
    if (keyboard_file.length()) {
        m_keyboard_number =
            kmfl_load_keyboard((char *) keyboard_file.c_str());
        if (m_keyboard_number >= 0) {
            m_name = WideString(utf8_mbstowcs(kmfl_keyboard_name(m_keyboard_number)));
            DBGMSG(1, "DAR/jd: kmfl - Keyboard %s loaded\n",
                   kmfl_keyboard_name(m_keyboard_number));
            
    		p_kmsi = kmfl_make_keyboard_instance(NULL);
    		
		    if (p_kmsi) {
        		kmfl_attach_keyboard(p_kmsi, m_keyboard_number);
		        *buf='\0';
		        kmfl_get_header(p_kmsi,SS_AUTHOR,buf,sizeof(buf) - 1);
		    	m_Author=String(buf);
		        *buf='\0';
		        kmfl_get_header(p_kmsi,SS_COPYRIGHT,buf,sizeof(buf) - 1);
		    	m_Copyright=String(buf);
		        *buf='\0';
		        kmfl_get_header(p_kmsi,SS_LANGUAGE,buf,sizeof(buf) - 1);
		    	m_Language=String(buf);
		        kmfl_detach_keyboard(p_kmsi);
		        kmfl_delete_keyboard_instance(p_kmsi);

		    }
		    if (m_Language.length() != 0)
	            set_languages(m_Language);
            return valid();
        }
        return false;
    }
    return false;
}

WideString KmflFactory::get_name() const
{
    return m_name;
}

WideString KmflFactory::get_authors() const
{
    return utf8_mbstowcs(m_Author);
}

WideString KmflFactory::get_credits() const
{
    return utf8_mbstowcs(m_Copyright);
}

String KmflFactory::get_language () const
{
    return scim_validate_language(m_Language);
}

WideString KmflFactory::get_help() const
{
    return utf8_mbstowcs(String(_("Hot Keys:\n\n"
                                  "  Esc:\n"
                                  "  reset the input method.\n")));
}

void KmflFactory::set_uuid(const String & suuid)
{
    uuid = suuid;
}

String KmflFactory::get_uuid() const
{
    return uuid;
}

String KmflFactory::get_icon_file() const
{
    String icon_file = kmfl_icon_file(m_keyboard_number);

    if (icon_file.length() == 0) {
        return String(SCIM_KMFL_IMENGINE_MODULE_DATADIR
                      SCIM_PATH_DELIM_STRING "icons" SCIM_PATH_DELIM_STRING
                      "default.png");
    } else {
        String full_path_to_icon_file =
            get_dirname(m_keyboard_file) +
            SCIM_PATH_DELIM_STRING "icons" SCIM_PATH_DELIM_STRING +
            icon_file;
        struct stat filestat;

        stat(full_path_to_icon_file.c_str(), &filestat);

        if (S_ISREG(filestat.st_mode)) {
            return full_path_to_icon_file;
        } else {
            return String("");
        }
    }
}

IMEngineInstancePointer
    KmflFactory::create_instance(const String & encoding,
                                              int id)
{
    return new KmflInstance(this, encoding, id);
}

// Implementation of KmflInstance
KmflInstance::KmflInstance(KmflFactory * factory,
                           const String & encoding, int id)
: IMEngineInstanceBase(factory, encoding, id), m_factory(factory),
  m_forward(false), m_focused(false), m_unicode(false), 
  m_changelayout(false), m_iconv(encoding), p_kmsi(NULL), m_currentsymbols(""), m_keyboardlayout(""), m_keyboardlayoutactive(false)
{
    m_display = XOpenDisplay(NULL);

    if (factory) {
        p_kmsi = kmfl_make_keyboard_instance(this);

        if (p_kmsi) {
            char buf[256];
            int keyboard_number = factory->get_keyboard_number();
            DBGMSG(1, "DAR: Loading keyboard %d\n", keyboard_number);

            kmfl_attach_keyboard(p_kmsi, keyboard_number);
            *buf='\0';
            if (kmfl_get_header(p_kmsi, SS_LAYOUT, buf, sizeof(buf) - 1)== 0) {                                
                m_keyboardlayout= buf;
                if (m_keyboardlayout.length() > 0) {
                    *buf='\0';
                    if (kmfl_get_header(p_kmsi,SS_MNEMONIC,buf,sizeof(buf) - 1) == 0) {
                        if (*buf != '1' && *buf != '2') {
                            m_changelayout= true;
                        }
                    } else {
                        m_changelayout= true;
                    }
                }
            }
        }
    }
    if (m_changelayout) {
            DBGMSG(1, "DAR: change layout is set, layout is %s\n", m_keyboardlayout.c_str());
    } else {
            DBGMSG(1, "DAR: change layout is not set\n");
    }

}

KmflInstance::~KmflInstance()
{
    restore_system_layout();
    if (p_kmsi) {
        kmfl_detach_keyboard(p_kmsi);
        kmfl_delete_keyboard_instance(p_kmsi);
    }
    p_kmsi = NULL;
    XCloseDisplay(m_display);
}
void KmflInstance::activate_keyboard_layout(void)
{
    if (!m_keyboardlayoutactive) {
        m_currentsymbols=xkbmap.getCurrentSymbols();
        DBGMSG(1, "DAR: changing layout from %s to %s\n", m_currentsymbols.c_str(), m_keyboardlayout.c_str());
        xkbmap.setLayout(m_keyboardlayout);
        m_keyboardlayoutactive= true;
    }
}

void KmflInstance::restore_system_layout(void)
{
    if (m_keyboardlayoutactive)        {
        DBGMSG(1, "DAR: changing layout from %s to %s\n", m_keyboardlayout.c_str(), m_currentsymbols.c_str());
        xkbmap.setSymbols(m_currentsymbols);
        m_keyboardlayoutactive=false;
    }
}

int KmflInstance::is_key_pressed(char *key_vec, KeySym keysym)
{
    unsigned char keycode;
    keycode = XKeysymToKeycode(m_display, keysym);
    return key_vec[keycode >> 3] & (1 << (keycode & 7));
}

bool KmflInstance::process_key_event(const KeyEvent & key)
{
    int mask;
    WideString context;
    int cursor;

    if (!m_focused) {
        return false;
    }

    DBGMSG(1, "DAR: kmfl - Keyevent, code: %x, mask: %x\n", key.code,
           key.mask);

    // Ignore key releases
    if (key.is_key_release()) {
        return true;
    }

    if (key.code == SCIM_KEY_Sys_Req && (key.mask & SCIM_KEY_ControlMask) && (key.mask & SCIM_KEY_AltMask)){
        DBGMSG(1, "DAR: kmfl -Reloading all keyboards\n");
        kmfl_reload_all_keyboards();
        return true;
    }

    if (key.code == SCIM_KEY_Print && (key.mask & SCIM_KEY_ControlMask)) {
        DBGMSG(1, "DAR: kmfl -Reloading keyboard %s\n", p_kmsi->kbd_name);
        kmfl_reload_keyboard(p_kmsi->keyboard_number);
        return true;
    }

    if (!m_forward) {
        // If a modifier key is pressed, check to see if it is a right modifier key
        // This is rather expensive so only do it if a shift state is active
        int right_modifier_mask = 0;
        if (key.mask & (SCIM_KEY_ShiftMask | SCIM_KEY_ControlMask | SCIM_KEY_Mod1Mask)) {
            char key_vec[32];
            XQueryKeymap(m_display, key_vec);

            if ((key.mask & SCIM_KEY_Mod1Mask) && is_key_pressed(key_vec, SCIM_KEY_Alt_R)) {
                right_modifier_mask |= (SCIM_KEY_Mod1Mask << 8);
            }

            if ((key.mask & SCIM_KEY_ControlMask) && is_key_pressed(key_vec, SCIM_KEY_Control_R)) {
                right_modifier_mask |= (SCIM_KEY_ControlMask << 8);
            }

            if ((key.mask & SCIM_KEY_ShiftMask) && is_key_pressed(key_vec, SCIM_KEY_Shift_R)) {
                right_modifier_mask |= (SCIM_KEY_ShiftMask << 8);
            }
        }

        mask = key.mask | right_modifier_mask;

        DBGMSG(1, "DAR: kmfl - keymask %x\n", mask);

        // Reset key
        if (key.code == SCIM_KEY_Pause) {
            reset();
            return true;
        }

        DBGMSG(1, "DAR: kmfl - Checking sequences for %d\n", key.code);

    	if (!deadkey_in_history(p_kmsi)) {
            if (get_surrounding_text (context, cursor, MAX_HISTORY, 0)) {
                UINT nItems= context.size ();
                ITEM items[MAX_HISTORY];

                DBGMSG(1, "DAR: kmfl -  get_surround_text: cursor at %d, length = %d, string %s\n", cursor, nItems, utf8_wcstombs(context).c_str());
                for (unsigned int i=0; i< nItems; ++i) {                                                                        
                    items[nItems - i - 1] =  MAKE_ITEM(ITEM_CHAR,context [i]);
                }
                set_history(p_kmsi, items, nItems);
            }
        }

        if (kmfl_interpret(p_kmsi, key.code, mask) == 1) {           
            return true;        
            // Not a modifier key, ie shift, ctrl, alt, etc
        } else if (!(key.code >= XK_Shift_L && key.code <= XK_Hyper_R)) {
            DBGMSG(1, "DAR: kmfl - key.code causing reset %x\n", key.code);
            reset();
        }
    }

    return false;
}

void KmflInstance::reset()
{

    DBGMSG(1, "DAR: kmfl - Reset called\n");

    // Clear the history for this instance (reset the context)
    clear_history(p_kmsi);

    m_iconv.set_encoding(get_encoding());
}

void KmflInstance::focus_in()
{
    if (m_changelayout && !m_forward) {
        activate_keyboard_layout();
    }
    m_focused = true;
    refresh_status_property();

    initialize_properties ();
}

void KmflInstance::focus_out()
{
    if (m_changelayout) {
        restore_system_layout();
    }
        
    m_focused = false;
}

void KmflInstance::toggle_input_status()
{
    DBGMSG(1, "DAR: kmfl - toggle_input_status\n");
}

void KmflInstance::trigger_property(const String &property)
{
    DBGMSG(1, "DAR: kmfl - trigger_property\n");
}

void KmflInstance::initialize_properties ()
{
    PropertyList proplist;

    proplist.push_back (m_factory->m_status_property);

    register_properties (proplist);

    refresh_status_property ();
}

void KmflInstance::refresh_status_property()
{
    if (m_focused) {
        if (m_forward) {
            m_factory->m_status_property.set_label(_("En"));
        } else if (m_unicode) {
            m_factory->m_status_property.set_label(_("Unicode"));
        } else {
            m_factory->m_status_property.set_label(get_encoding());
        }
        update_property (m_factory->m_status_property);
    }
        
}

void KmflInstance::forward_keyevent(unsigned int key, unsigned int state)
{
    KeyEvent fkey(key, state);
    
    DBGMSG(1, "DAR: kmfl - forward key event key=%x, state=%x\n", key,state);

    forward_key_event(fkey);
}

void KmflInstance::erase_char()
{
    KeyEvent backspacekey(SCIM_KEY_BackSpace, 0);

    WideString text;
    int cursor;
    bool result;
    
    DBGMSG(1, "DAR: kmfl - backspace\n");

    if (get_surrounding_text (text, cursor, 1, 0)) {
        if (!delete_surrounding_text(-1, 1)) {
            DBGMSG(1, "DAR: delete_surrounding_text failed...forwarding key event\n");

            forward_key_event(backspacekey);
            DBGMSG(1, "DAR: kmfl -  key event forwarded\n");
        }
    } else {
        forward_key_event(backspacekey);
        DBGMSG(1, "DAR: kmfl -  key event forwarded\n");
    }
}

void KmflInstance::output_string(const String & str)
{
    if (str.length() > 0) {
        DBGMSG(1, "DAR: kmfl - committing string %s\n", str.c_str());

        commit_string(utf8_mbstowcs(str));
    }
}

void KmflInstance::output_beep()
{
	beep();
}

extern "C" {

    void output_string(void *contrack, char *ptr) {
        if (ptr) {
            ((KmflInstance *) contrack)->output_string(ptr);
        }
    }

    void erase_char(void *contrack) {
        ((KmflInstance *) contrack)->erase_char();
    }

    void output_char(void *contrack, unsigned char byte) {
        if (byte == 8) {
            erase_char(contrack);
        } else {
            char s[2];
            s[0] = byte;
            s[1] = '\0';
            output_string(contrack, s);
        }
    }
    
    void forward_keyevent(void *contrack, unsigned int key, unsigned int state)
    {
        ((KmflInstance *) contrack)->forward_keyevent(key, state);
    }

    void output_beep(void *contrack) {
        DBGMSG(1, "DAR: kmfl - beep\n");
        ((KmflInstance *) contrack)->output_beep();
        
    }
}                                /* extern "c" */

/*
vi:ts=4:nowrap:ai:expandtab
*/
