/** @file scim_kmfl_imengine.h
 * definition of Kmfl related classes.
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

#if !defined (__SCIM_KMFL_IMENGINE_H)
#define __SCIM_KMFL_IMENGINE_H

using namespace scim;

class KmflFactory : public IMEngineFactoryBase
{
    WideString m_name;

    friend class KmflInstance;
    Property m_status_property;

public:
    KmflFactory ();
    KmflFactory (const WideString & name, const String & locales);
    virtual ~KmflFactory ();

    virtual WideString  get_name () const;
    virtual WideString  get_authors () const;
    virtual WideString  get_credits () const;
    virtual WideString  get_help () const;
    virtual String      get_uuid () const;
    virtual String      get_icon_file () const;
    virtual String      get_language () const;

    virtual IMEngineInstancePointer create_instance (const String& encoding, int id = -1);
	int get_keyboard_number()
    {
        return m_keyboard_number;
    }
    bool load_keyboard (const String &keyboard_file, bool user_keyboard);
    bool valid () const {
        return true;
    }
    void set_uuid(const String & suuid);

private:
    int m_keyboard_number; 	
    String m_keyboard_file;				
    String uuid;
    String m_Language;
    String m_Author;
    String m_Copyright;

};

class KmflInstance : public IMEngineInstanceBase
{
    Pointer <KmflFactory> m_factory;

    bool m_forward;
    bool m_focused;
    bool m_unicode;
    bool m_changelayout;
	
    IConvert m_iconv;
    KMSI *p_kmsi;	// Pointer to the current imengine instance

    Display * m_display;
    String m_currentsymbols;
    String m_keyboardlayout;
    bool m_keyboardlayoutactive;

public:
    KmflInstance (KmflFactory *factory,
                           const String& encoding,
                           int id = -1);
    virtual ~KmflInstance ();

    virtual bool process_key_event (const KeyEvent& key);

    virtual void reset ();
    virtual void focus_in ();
    virtual void focus_out ();

    // These functions are not used for scim_kmfl_imengine
    virtual void move_preedit_caret (unsigned int pos){};
    virtual void select_lookup_table (unsigned int item){};
    virtual void lookup_table_page_up(){};
    virtual void lookup_table_page_down(){};
    virtual void select_candidate(unsigned int candidate){};
    virtual void update_lookup_table_page_size (unsigned int page_size){};
    virtual void toggle_full_width_punctuation (){};
    virtual void toggle_full_width_letter (){};
    virtual void trigger_property(const String &property);
    virtual void toggle_input_status ();
    void output_string(const String&str);
    void erase_char ();
    void forward_keyevent(unsigned int key, unsigned int state);
    void output_beep ();

private:
    int create_lookup_table (int start = 0);
    void refresh_status_property ();
    void initialize_properties ();
    int is_key_pressed(char * key_vec, KeySym keysym);

    String get_multibyte_string (const WideString& preedit);
    ucs4_t get_unicode_value (const WideString& preedit);
    void activate_keyboard_layout(void);
    void restore_system_layout(void);
    

};

#endif
/*
vi:ts=4:nowrap:ai:expandtab
*/
