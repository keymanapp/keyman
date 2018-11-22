/* vim:set et sts=4: */

/*
 * Keyman Input Method for IBUS (The Input Bus)
 *
 * Copyright (C) 2018 SIL International
 *
 * kmflutil is dual licensed under the MIT or GPL licenses as described below.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * MIT license
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * OR
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */

#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <string.h>


#include "keymanutil.h"
#include "kmpdetails.h"

#if 0
//Globally loaded keyboards
#define MAX_KEYBOARDS	64		// maximum number of keyboards that can be loaded
KInputKeyboard *p_installed_kbd[MAX_KEYBOARDS]={NULL};
unsigned int n_keyboards=0;
#endif

static GHashTable      *im_table = NULL;

#define N_(text) text
static gchar * get_dirname(const gchar * path)
{
    gchar * dirend = g_strrstr(path, "/");

    if (dirend) {
        return g_strndup(path, dirend-path);
    } else {
        return g_strdup("");
    }
}

// change to keyman_get_kmpdirs_fromdir
// returns list of directories with kmp.json
GList * keyman_get_kmpdirs_fromdir( GList *keyboard_list, const gchar * path)
{
//    g_message("KMFL: getting from dir: %s", path);
    DIR *dir = opendir(path);

    if (dir != NULL) {
        struct dirent *file = readdir(dir);
        while (file != NULL) {
            struct stat filestat;
            gchar * absfn = g_strdup_printf("%s/%s", path, file->d_name);
            stat(absfn, &filestat);

            if (S_ISDIR(filestat.st_mode))
            {
                if(g_strcmp0(file->d_name, ".") != 0 && g_strcmp0(file->d_name, "..") != 0)
                    keyboard_list = keyman_get_kmpdirs_fromdir(keyboard_list, absfn);
            }
            // Looking for kmp.json
            else if (S_ISREG(filestat.st_mode) && g_strcmp0(file->d_name, "kmp.json") == 0)
            {
                g_message("adding kmp path %s", path);
                keyboard_list=g_list_append(keyboard_list, g_strdup(path));
            }
            g_free(absfn);

            file = readdir(dir);
        }
        closedir(dir);
    }
    return keyboard_list;
}
#if 0
GList * keyman_get_keyboard_list( const gchar * path)
{
    GList * keyboard_list=NULL;
    keyboard_list = keyman_get_keyboard_fromdir(keyboard_list, path);
    return keyboard_list;
}
#endif
gchar * keyman_get_icon_file(const gchar *kmx_file)
{
    // Now there will only be the .png which will have been extracted from the .kmx during installation
    struct stat filestat;
    gchar *filename, *full_path_to_icon_file, *p;

    p=rindex(kmx_file,'.');
    filename = g_strndup(kmx_file, p-kmx_file);
    full_path_to_icon_file=g_strdup_printf("%s.ico.png", filename);
    g_free(filename);
    stat(full_path_to_icon_file, &filestat);

    if (!S_ISREG(filestat.st_mode)) {
        g_free(full_path_to_icon_file);
        full_path_to_icon_file=g_strdup("/usr/share/keyman/icons/default.png");
    }

    return full_path_to_icon_file;
}

#if 0
gchar * keyman_get_kvk_file(KInputMethod * im)
{
    gchar *p, *filename, *full_path_to_kvk_file;
    gchar *kmx_file, *kvk_file;
    struct stat filestat;

    kmx_file  = g_path_get_basename(im->keyboard_filename);
    p=rindex(kmx_file,'.');
    if (p==NULL)
    {
        g_debug("WDG: couldn't find . in kmx filename %s", kmx_file);
        g_free(kmx_file);
        return g_strdup("");
    }
    if (strncmp(p, ".kmx", 4) != 0)
    {
        g_debug("WDG: keyboard is not a kmx. Filename: %s", kmx_file);
        g_free(kmx_file);
        return g_strdup("");
    }
    filename = g_strndup(kmx_file, p-kmx_file);
    full_path_to_kvk_file=g_strdup_printf("%s/%s.kvk", get_dirname(im->keyboard_filename), filename);
    // don't forget to stat for the file before returning
    stat(full_path_to_kvk_file, &filestat);

    if (!S_ISREG(filestat.st_mode)) {
        g_debug("WDG: couldn't find kvk file %s", full_path_to_kvk_file);
        kvk_file=g_strdup("");
    }
    else
    {
        kvk_file=g_strdup_printf("%s.kvk", filename);
    }
    g_free(full_path_to_kvk_file);
    g_free(filename);
    return kvk_file;
}


gchar * keyman_get_ldml_file(KInputMethod * im)
{
	// kvk details from the json?
	// or will it be available in the keyboardprocessor API?
    // just use same name as kmx different extension for now
    gchar * full_path_to_ldml_file=NULL, *p, *filename;
    struct stat filestat;

    if(g_strcmp0(im->keyboard_visualkeyboard, "") == 0)
    {
        g_debug("WDG: no kvk(s) so no ldml file");
        return g_strdup("");
    }
    p=rindex(im->keyboard_visualkeyboard,'.');
    if (p==NULL)
    {
        g_debug("WDG: couldn't find . in vk filename %s", im->keyboard_visualkeyboard);
        return g_strdup("");
    }
    if (strncmp(p, ".kvk", 4) != 0) // sometimes kmn have kvks as the "visual keyboard"
    {
        g_debug("WDG: visual keyboard is not a kvk. Filename: %s", im->keyboard_visualkeyboard);
        return g_strdup("");
    }
    filename = g_strndup(im->keyboard_visualkeyboard, p-(im->keyboard_visualkeyboard));
    full_path_to_ldml_file=g_strdup_printf("%s/%s.ldml", get_dirname(im->keyboard_filename), filename);
    g_free(filename);
    // don't forget to stat for the file before returning
    stat(full_path_to_ldml_file, &filestat);

    if (!S_ISREG(filestat.st_mode)) {
        g_debug("WDG: couldn't find ldml file %s", full_path_to_ldml_file);
        g_free(full_path_to_ldml_file);
        full_path_to_ldml_file=g_strdup("");
    }
    return full_path_to_ldml_file;
}
#endif

#if 0
void keyman_get_keyboard_info(KInputMethod * im) 
{
    kmp_details details;
    g_assert(im != NULL);
    get_kmp_details(im->keyboard_kmp_json, &details);
	// either get these from json?
	// or the keyboardprocessor API from the kmx?
    im->keyboard_name = g_strdup(details.info.name); // should be details.keyboards name
    im->keyboard_author = g_strdup(details.info.author_desc);
    im->keyboard_copyright = g_strdup(details.info.copyright);
    im->keyboard_language=g_strdup("en"); // ???
    im->keyboard_description=g_strdup("Dummy keyboard to test API"); // ???
    im->keyboard_license = g_strdup("MIT"); // ???
    im->keyboard_visualkeyboard=keyman_get_kvk_file(im);
    im->keyboard_keyboardversion=g_strdup(details.info.version); // should be details.keyboards version
    im->keyboard_layout=g_strdup("us"); // ???
    im->keyboard_icon_filename = keyman_get_icon_file(im);
    im->keyboard_ldmlfile=keyman_get_ldml_file(im);
    free_kmp_details(&details);

    #if 0
    char buf[1024];
    KMSI * p_kmsi;

    im->keyboard_name = g_strdup(kmfl_keyboard_name(im->keyboard_number));
    p_kmsi = kmfl_make_keyboard_instance(NULL);
    kmfl_attach_keyboard(p_kmsi, im->keyboard_number);
    
    *buf='\0';
    kmfl_get_header(p_kmsi,SS_AUTHOR,buf,sizeof(buf) - 1);
    im->keyboard_author=g_strdup(buf);
    
    *buf='\0';
    kmfl_get_header(p_kmsi,SS_COPYRIGHT,buf,sizeof(buf) - 1);
    im->keyboard_copyright=g_strdup(buf);
    
    *buf='\0';
    kmfl_get_header(p_kmsi,SS_LANGUAGE,buf,sizeof(buf) - 1);
    im->keyboard_language=g_strdup(buf);
    
    *buf='\0';
    kmfl_get_header(p_kmsi,SS_MESSAGE,buf,sizeof(buf) - 1);
    im->keyboard_description=g_strdup(buf);

    if (g_strrstr(buf, "license") ||  g_strrstr(buf, "License") || g_strrstr(buf, "LICENSE"))
        im->keyboard_license=g_strdup(buf);
    else
        im->keyboard_license=g_strdup("");

    *buf='\0';
    kmfl_get_header(p_kmsi,SS_VISUALKEYBOARD,buf,sizeof(buf) - 1);
    im->keyboard_visualkeyboard=g_strdup(buf);

    *buf='\0';
    kmfl_get_header(p_kmsi,SS_KEYBOARDVERSION,buf,sizeof(buf) - 1);
    im->keyboard_keyboardversion=g_strdup(buf);

    *buf='\0';
    kmfl_get_header(p_kmsi,SS_LAYOUT,buf,sizeof(buf) - 1);
    if (*buf != '\0')    
        im->keyboard_layout=g_strdup(buf);
    else
        im->keyboard_layout=g_strdup("us");

    kmfl_detach_keyboard(p_kmsi);
    kmfl_delete_keyboard_instance(p_kmsi);
    im->keyboard_icon_filename = kmfl_get_icon_file(im);
    im->keyboard_ldmlfile = kmfl_get_ldml_file(im);
    #endif
}

void keyman_free_keyboard_info(KInputMethod * im)
{
    g_assert(im != NULL);  
    g_free(im->keyboard_filename);  
    g_free(im->keyboard_name);
    g_free(im->keyboard_author);
    g_free(im->keyboard_copyright);
    g_free(im->keyboard_language);
    g_free(im->keyboard_description);
    g_free(im->keyboard_icon_filename);
    g_free(im->keyboard_layout);
    g_free(im->keyboard_license);
    g_free(im->keyboard_visualkeyboard);
    g_free(im->keyboard_keyboardversion);
    g_free(im->keyboard_ldmlfile);
}
#endif

static IBusEngineDesc *
ibus_keyman_engine_new (gchar * file_name,
                      gchar *name,
                      gchar *description,
                      gchar *copyright,
                      gchar *lang,
                      gchar *license,
                      gchar *author,
                      gchar *icon,
                      gchar *layout)
{
	// anything else to add to the engine description that we can get from
	// either json or keyboardprocessor API?
	// esp languages
    IBusEngineDesc *engine_desc;
    gchar * desc;

    if (description == NULL) {
        desc = g_strdup_printf("%s", copyright);
    }
    else {
        desc = g_strdup_printf("%s\n%s", description, copyright);
    }

    // use varargs instead to put version in
    engine_desc = ibus_engine_desc_new (file_name, // any other proposal for the "engine name" 
                                   name, // longname
                                   desc,
                                   lang,
                                   license ? license : "",
                                   author ? author : "",
                                   icon ? icon : "",
                                   layout);

    return engine_desc;
}

GList * 
ibus_keyman_add_engines(GList * engines, GList * kmpdir_list)
{
    GList *p, *l;

    for (p=kmpdir_list; p != NULL; p = p->next) {
        gchar * kmp_dir = (gchar *) p->data;
        // TODO: get the other info from the filename from kmpdetails
        //KInputMethod im;
        //im.keyboard_filename = g_strdup(keyboard_filename);
        //keyman_get_keyboard_info(&im);

        g_message("getting kmp details for %s", kmp_dir);
        kmp_details *details = g_new0(kmp_details, 1);
        get_kmp_details(kmp_dir, details);
        g_message("got kmp details for %s", kmp_dir);

        for (l=details->keyboards; l != NULL; l = l->next) {
            gchar *lang=NULL;
            kmp_keyboard *keyboard = (kmp_keyboard *) l->data;
            g_message("got keyboard %s", keyboard->name);
            g_message("got keyboard kmx %s", keyboard->kmx_file);
            gchar *abs_kmx = g_strjoin("/", kmp_dir, keyboard->kmx_file, NULL);
            g_message("getting language for %s", abs_kmx);

            if (keyboard->languages != NULL)
            {
                // Only gets the first language because ibus can only handle one
                kmp_language *language = (kmp_language *) keyboard->languages->data;
                if (language->id != NULL) {
                    gchar **tagparts = g_strsplit(language->id, "-", 2);
                    lang = g_strdup(tagparts[0]);
                    g_strfreev(tagparts);
                }
            }
            gchar *json_file = g_strjoin(".", keyboard->id, "json", NULL);
            keyboard_details *kbd_details = g_new0(keyboard_details, 1);
            get_keyboard_details(kmp_dir, json_file, kbd_details);
            g_free(json_file);

            g_message("adding engine %s", abs_kmx);
            engines = g_list_append (engines,
                ibus_keyman_engine_new (abs_kmx, // kmx full path
                        keyboard->name, // longname
                        kbd_details->description, // description
                        details->info.copyright, // copyright if available
                        lang, // language, most are ignored by ibus except major languages
                        kbd_details->license, // license
                        details->info.author_desc, // author name only, not email
                        keyman_get_icon_file(abs_kmx), // icon full path
                        "en")); // layout defaulting to en (en-US)
            g_message("added engine %s", abs_kmx);
            free_keyboard_details(kbd_details);
            g_free(kbd_details);
            g_free(abs_kmx);
            g_free(lang);
        }
        g_message("freeing details");
        free_kmp_details(details);
        g_free(details);
        g_message("finished adding engines");
    }
    return engines;
}

GList *
ibus_keyman_list_engines (void)
{
    GList *engines = NULL;
    GList *keyboard_list;
    gchar *local_keyboard_path, *xdgenv;

/*
    if (im_table == NULL) {
        im_table = g_hash_table_new_full (g_str_hash,
                                          g_str_equal,
                                          g_free,
                                          (GDestroyNotify) keyman_free_keyboard_info);
    }
*/
    g_message("adding from /usr/share/keyman");
    keyboard_list = keyman_get_kmpdirs_fromdir(NULL, "/usr/share/keyman");
    g_message("adding from /usr/share/keyman");
    keyboard_list = keyman_get_kmpdirs_fromdir(keyboard_list, "/usr/local/share/keyman");
    xdgenv = getenv("XDG_DATA_HOME");
    if (xdgenv != NULL){
        local_keyboard_path= g_strdup_printf("%s/keyman", xdgenv);
    }
    else {
        xdgenv = getenv("HOME");
        local_keyboard_path= g_strdup_printf("%s/.local/share/keyman", xdgenv);
    }
    g_message("adding from %s", local_keyboard_path);
    keyboard_list = keyman_get_kmpdirs_fromdir(keyboard_list, local_keyboard_path);
    g_free(local_keyboard_path);
    engines = ibus_keyman_add_engines(engines, keyboard_list);
    g_list_free(keyboard_list);

    return engines;
}

IBusComponent *
ibus_keyman_get_component (void)
{
    GList *engines, *p;
    IBusComponent *component;

    component = ibus_component_new ("org.freedesktop.IBus.Keyman",
                                    N_("Keyman"),
                                    "10.99.0",
                                    "GPL",
                                    "Daniel Glassey <wdg@debian.org>",
                                    "http://www.keyman.com",
                                    "",
                                    "ibus-keyman");

    engines = ibus_keyman_list_engines ();

    for (p = engines; p != NULL; p = p->next) {
        ibus_component_add_engine (component, (IBusEngineDesc *) p->data);
    }

    g_list_free (engines);
    return component;
}
#if 0
KInputMethod * kinput_open_im(const gchar * keyboard_filename)
{
    KInputMethod * im = g_new(KInputMethod, 1);
    
    if (im != NULL) {
        im->keyboard_number = keyman_load_keyboard(keyboard_filename);
        if (im->keyboard_number < 0) {
            g_free(im);
            im = NULL;
        } else {
            im->keyboard_filename = g_strdup(keyboard_filename);
            keyman_get_keyboard_info(im) ;
        }
    }
    return im;
}
#endif
#if 0
void kinput_close_im(KInputMethod * im)
{
    g_assert(im != NULL);
    keyman_free_keyboard_info(im);
    keyman_unload_keyboard(im->keyboard_number);
    g_free(im);
}
#endif
// reimplement
#if 0
void output_string(void *contrack, char *ptr) {
    KInputContext * ic = (KInputContext *)contrack;
    g_debug("DAR: output_string - ic->cmds=%p", ic->cmds);
    if (ptr) {
        Cmd * cmd = g_new(Cmd, 1);
        cmd->opcode=OUTPUT_STRING;
        cmd->cmdarg=g_strdup(ptr);
        ic->cmds = g_list_append(ic->cmds, cmd);
    }
}
void erase_char(void *contrack) {
    KInputContext * ic = (KInputContext *)contrack;
    g_debug("DAR: erase_char - ic->cmds=%p", ic->cmds);
    Cmd * cmd = g_new(Cmd, 1);
    cmd->opcode = ERASE_CHAR;
    cmd->cmdarg=NULL;
    ic->cmds = g_list_append(ic->cmds, cmd);
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

}

void output_beep(void *contrack) {
    KInputContext * ic = (KInputContext *)contrack;
    Cmd * cmd = g_new(Cmd, 1);
    cmd->opcode = OUTPUT_BEEP;
    cmd->cmdarg=NULL;
    ic->cmds = g_list_append(ic->cmds, cmd);    
}
#endif


/*
 * Create an input context
 */
// not relevant now, context managed by state
#if 0
KInputContext *
keyman_create_ic(KInputMethod *im)
{
    KInputContext * ic;
    g_debug("DAR: keyman_create_ic");
    ic = g_new(KInputContext, 1);
    ic->cmds = NULL;
    //ic->p_kmsi=kmfl_make_keyboard_instance(ic);
    //kmfl_attach_keyboard(ic->p_kmsi, im->keyboard_number);

    return ic;
}


/*
 * Destroy an input context
 */
void
keyman_destroy_ic(KInputContext *ic)
{
    GList * p;
    g_debug("DAR: keyman_destroy_ic");
    //kmfl_detach_keyboard(ic->p_kmsi);
    //kmfl_delete_keyboard_instance(ic->p_kmsi);
    
    if (ic->cmds) {
        for (p=ic->cmds; p != NULL; p = p->next) {
            Cmd * cmd = (Cmd *)p->data;
            if (cmd) {
                if (cmd->cmdarg)
                    g_free(cmd->cmdarg);
                g_free(cmd);
            }
        }
        g_list_free(ic->cmds);
        ic->cmds=NULL;
    }
}

KInputKeyboard * keyman_load_keyboard_from_file(const gchar *filename)
{
	KInputKeyboard *p_kbd = g_new(KInputKeyboard, 1);
	g_message("DAR: keyman_load_keyboard_from_file %s\n",filename);
	FILE *fp;
	char version_string[6]={0};
	unsigned int filelen, kbver=0;
	struct stat fstat;
	const char * extension;
    int errcode;

	DBGMSG(1,"DAR: kmfl_load_keyboard_from_file %s\n",filename);

    extension = strrchr(filename, '.');
    
    // Get the file size
    if(stat(filename,&fstat) != 0) 
        return NULL;
    filelen = fstat.st_size;

    // Allocate memory for the installed keyboard
    if((p_kbd=(XKEYBOARD *)malloc(filelen)) == NULL) 
        return NULL;

    // Open the file
    if((fp=fopen(filename,"rb")) != NULL) 
    {
        if (fread(p_kbd, 1, filelen, fp) != filelen)
        {
            fclose(fp);
            return NULL;
        }
        fclose(fp);
        memcpy(version_string,p_kbd->version,4); // Copy to ensure terminated
        kbver = (unsigned)atoi(version_string);
    }
    // Check the loaded file is valid and has the correct version
    if((memcmp(p_kbd->id,"KMFL",4) != 0) 
        || (p_kbd->version[4] != *FILE_VERSION)
        || (kbver < (unsigned)atoi(BASE_VERSION))
        || (kbver > (unsigned)atoi(LAST_VERSION)))
    {
        DBGMSG(1, "Invalid version %s\n", version_string);
        free(p_kbd); 
        return NULL;
    }

    DBGMSG(1,"DAR: kmfl_load_keyboard_from_file - %s loaded\n",filename);

    return p_kbd;
}


int keyman_load_keyboard(const gchar *keyboard_filename)
{
	KInputKeyboard *p_kbd;
	int keyboard_number;
	
	// Check number of installed keyboards
	if(n_keyboards >= MAX_KEYBOARDS) return -1;
	
	// initialize the installed keyboards array
	if(n_keyboards == 0)
		memset(p_installed_kbd, 0, sizeof(KInputKeyboard *) * MAX_KEYBOARDS);
	
	p_kbd = keyman_load_keyboard_from_file(keyboard_filename);

	if (p_kbd == NULL)
		return -1;

	// Find an empty slot
	for (keyboard_number=0;keyboard_number < MAX_KEYBOARDS; keyboard_number++)
		if (p_installed_kbd[keyboard_number] == NULL)
			break;
		
	// Sanity check
	if (keyboard_number == MAX_KEYBOARDS) {
		g_warning("Could not find an empty keyboard slot even though there was supposed to be one\n");
		free(p_kbd);
		return -1;
	}
	
	// Copy pointer and increment number of installed keyboards
    p_kbd->keyboard_filename = g_strdup(keyboard_filename);
    p_kbd->name = g_strdup_printf("dummy%d", keyboard_number);
	p_installed_kbd[keyboard_number] = p_kbd;
	
	n_keyboards++;
	g_message("Keyboard %s loaded\n",p_kbd->name);

	return keyboard_number;	
}

// Unload a keyboard that has been installed
int keyman_unload_keyboard(int keyboard_number) 
{
    KInputKeyboard *p_kbd=p_installed_kbd[keyboard_number];

    if (p_kbd == NULL) 
        return -1;

    // // Enumerate instances and ensure that no instances are using this keyboard
    // for(p=p_first_instance; p; p=p->next)
    // {
    //     if(p->keyboard_number == keyboard_number) return 1;
    // }

        
    // Remove keyboard from list and free memory
    g_message("Keyboard %s unloaded\n",p_kbd->name);
    g_free(p_kbd->name);
    g_free(p_kbd->keyboard_filename);
    g_free(p_kbd);

    p_installed_kbd[keyboard_number]=NULL;

    n_keyboards--;

    return 0;
}

int keyman_check_keyboard(gchar *absfn)
{
    return 0;
}
#endif


#ifdef DEBUG
#include <locale.h>

int main ()
{
    IBusComponent *component;
    GString *output;

    setlocale (LC_ALL, "");
    ibus_init ();

    component = ibus_keyman_get_component ();

    output = g_string_new ("");

    ibus_component_output (component, output, 1);

    g_debug ("\n%s", output->str);

    g_string_free (output, TRUE);
    g_object_unref (component);

    return 0;
}
#endif

