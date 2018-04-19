/** @file scim_kmfl_imengine_setup.cpp
 * implementation of Setup Module of KMFL imengine module.
 */

/*
 * KMFL Input Method for SCIM (Smart Common Input Method)
 *
 * Copyright (C) 2005 SIL International
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
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

 
#define Uses_SCIM_CONFIG_BASE


#include <string.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <kmfl/kmfl.h>
#include <kmfl/libkmfl.h>
#include <fstream>
#include <errno.h>
#include <setjmp.h>
#include <kmfl/kmflcomp.h>
#include <kmfl/kmflutfconv.h>
#include "kmfl_private.h"
#include <scim.h>
#include <gtk/scimkeyselection.h>

using namespace scim;

#define scim_module_init kmfl_imengine_setup_LTX_scim_module_init
#define scim_module_exit kmfl_imengine_setup_LTX_scim_module_exit

#define scim_setup_module_create_ui       kmfl_imengine_setup_LTX_scim_setup_module_create_ui
#define scim_setup_module_get_category    kmfl_imengine_setup_LTX_scim_setup_module_get_category
#define scim_setup_module_get_name        kmfl_imengine_setup_LTX_scim_setup_module_get_name
#define scim_setup_module_get_description kmfl_imengine_setup_LTX_scim_setup_module_get_description
#define scim_setup_module_load_config     kmfl_imengine_setup_LTX_scim_setup_module_load_config
#define scim_setup_module_save_config     kmfl_imengine_setup_LTX_scim_setup_module_save_config
#define scim_setup_module_query_changed   kmfl_imengine_setup_LTX_scim_setup_module_query_changed


#define SCIM_CONFIG_IMENGINE_KEYBOARD_MODE_SWITCH_KEY         "/IMEngine/KMFL/ModeSwitchKey"
#define SCIM_KMFL_USER_KEYBOARDS_DIR 						SCIM_PATH_DELIM_STRING ".scim" SCIM_PATH_DELIM_STRING "kmfl"
#define LIST_ICON_SIZE 20

static GtkWidget *create_setup_window();
static void load_config(const ConfigPointer & config);
static void save_config(const ConfigPointer & config);
static bool query_changed();

static void destroy_all_keyboards();

// Module Interface.
extern "C" {
    void scim_module_init(void) {
        fprintf(stderr, "scim_module_init\n");
    } 
    
    void scim_module_exit(void) {
        fprintf(stderr, "scim_module_exit start\n");
	destroy_all_keyboards();
        fprintf(stderr, "scim_module_exit end\n");
    }

    GtkWidget *scim_setup_module_create_ui(void) {
        fprintf(stderr, "scim_module_create_ui\n");
	return create_setup_window();
    }

    String scim_setup_module_get_category(void) {
        fprintf(stderr, "scim_module_get_category\n");
	return String("IMEngine");
    }

    String scim_setup_module_get_name(void) {
        fprintf(stderr, "scim_module_get_name\n");
	return String(_("KMFL"));
    }

    String scim_setup_module_get_description(void) {
        fprintf(stderr, "scim_module_get_description\n");
	return String(_
		      ("A IMEngine Module which uses kmfl input method file."));
    }

    void scim_setup_module_load_config(const ConfigPointer & config) {
	load_config(config);
    }

    void scim_setup_module_save_config(const ConfigPointer & config) {
	save_config(config);
    }

    bool scim_setup_module_query_changed() {
	return query_changed();
    }
}				// extern "C"

// Internal data structure
struct KeyboardConfigData {
    const char *key;
    const char *label;
    const char *title;
    const char *tooltip;
    GtkWidget *entry;
    GtkWidget *button;
    String data;
};

enum {
    TABLE_COLUMN_ICON = 0,
    TABLE_COLUMN_NAME,
    TABLE_COLUMN_FILE,
    TABLE_COLUMN_TYPE,
    TABLE_COLUMN_KEYBOARD,
    TABLE_COLUMN_IS_USER,
    TABLE_NUM_COLUMNS
};

struct KeyboardPropertiesData {
    String name;
    String author;
    String icon;
    String locales;
    String copyright;
};

// Internal data declaration.
static bool __have_changed = false;

static GtkTooltips *__widget_tooltips = 0;

static GtkWidget *__widget_keyboard_list_view = 0;
static GtkListStore *__widget_keyboard_list_model = 0;

static GtkWidget *__widget_keyboard_install_button = 0;
static GtkWidget *__widget_keyboard_delete_button = 0;
static GtkWidget *__widget_keyboard_properties_button = 0;

static KeyboardConfigData __config_keyboards[] = {
    {
     // key
     SCIM_CONFIG_IMENGINE_KEYBOARD_MODE_SWITCH_KEY,
     // label
     N_("_Mode switch:"),
     // title
     N_("Select mode switch keys"),
     // tooltip
     N_("The key events to change current input mode. "
	"Multiple key events should be separated by comma."),
     // entry
     NULL,
     // button
     NULL,
     // data
     "Alt+Shift_L+KeyRelease," "Alt+Shift_R+KeyRelease,"
     "Shift+Shift_L+KeyRelease," "Shift+Shift_R+KeyRelease"},
    {
     // key
     NULL,
     // label
     NULL,
     // title
     NULL,
     // tooltip
     NULL,
     // entry
     NULL,
     // button
     NULL,
     // data
     ""},
};

// Declaration of internal functions.
static void on_default_editable_changed(GtkEditable * editable,
					gpointer user_data);

static void on_default_key_selection_clicked(GtkButton * button,
					     gpointer user_data);

static void on_keyboard_list_selection_changed(GtkTreeSelection *
					       selection,
					       gpointer user_data);

static void on_keyboard_install_clicked(GtkButton * button,
					gpointer user_data);

static void on_keyboard_delete_clicked(GtkButton * button,
				       gpointer user_data);

static void on_keyboard_properties_clicked(GtkButton * button,
					   gpointer user_data);

static gint run_keyboard_properties_dialog(XKEYBOARD * keyboard,
					   KeyboardPropertiesData & data,
					   bool editable);

static GdkPixbuf *scale_pixbuf(GdkPixbuf ** pixbuf, int width, int height);

static void setup_widget_value();

static GtkWidget *create_kmfl_management_page();

static void get_keyboard_list(std::vector < String > &keyboard_list,
			      const String & path);

static XKEYBOARD *load_kmfl_file(const String & file);

static void add_keyboard_to_list(XKEYBOARD * keyboard, const String & dir,
				 const String & file, bool user);

static void delete_keyboard_from_list(GtkTreeModel * model,
				      GtkTreeIter * iter);

static void load_all_keyboards();

static bool test_file_modify(const String & file);

static bool test_file_unlink(const String & file);

static void show_restart_hint()
{
    GtkWidget *dialog;

    dialog = gtk_message_dialog_new (NULL,
                            GTK_DIALOG_MODAL,
                            GTK_MESSAGE_INFO,
                            GTK_BUTTONS_OK,
                            _("Please restart any applications currently "
                              "using KMFL for your changes to take effect."));

    gtk_dialog_run (GTK_DIALOG (dialog));

    gtk_widget_destroy (dialog);
}

static void restart_scim()
{
    FILE *in;
    char buff[512];

    if (!(in = popen("scim-config-agent -c global -g /DefaultConfigModule", "r"))) 
    {
        return;
    }

    /* read the output to get the default config module */
    if (fgets(buff, sizeof(buff), in) != NULL) 
    {
    		int result;

		String defaultconfigmodule(buff);
		
		defaultconfigmodule=defaultconfigmodule.substr(0, defaultconfigmodule.length()-1);
		
		String command("/usr/lib/scim-1.0/scim-launcher -d -c " + defaultconfigmodule + " -e all -f socket --no-stay");
		
		String pkill("pkill -f \""+command+"\"");

		result=system(pkill.c_str());
		result=system(command.c_str());	
		show_restart_hint();
	}
	
    pclose(in);    
}

// from scim_utility.cpp since scim-0.8.0 does not have scim_make_dir
bool
make_dir (const String &dir)
{
    std::vector <String> paths;
    String path;

    scim_split_string_list (paths, dir, SCIM_PATH_DELIM);

    for (size_t i = 0; i < paths.size (); ++i) {
        path += SCIM_PATH_DELIM_STRING + paths [i];

        //Make the dir if it's not exist.
        if (access (path.c_str (), R_OK) != 0) {
            mkdir (path.c_str (), S_IRUSR | S_IWUSR | S_IXUSR);
            if (access (path.c_str (), R_OK) != 0) {
                return false;
            }
        }
    }
    return true;
}

static GtkListStore *create_kmfl_list_model()
{
    GtkListStore *model;

    model = gtk_list_store_new(TABLE_NUM_COLUMNS, GDK_TYPE_PIXBUF,
			       G_TYPE_STRING, G_TYPE_STRING,
			       G_TYPE_STRING, G_TYPE_POINTER,
			       G_TYPE_BOOLEAN);

    return model;
}

static GtkWidget *create_kmfl_management_page()
{
    GtkWidget *page;
    GtkWidget *vbox;
    GtkWidget *label;
    GtkWidget *scrolledwindow;
    GtkWidget *hbox;
    GtkWidget *button;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column, *namecolumn;
    GtkTreeSelection *selection;

    page = gtk_vbox_new(FALSE, 0);
    gtk_widget_show(page);

    label = gtk_label_new(_("The installed keyboards:"));
    gtk_widget_show(label);
    gtk_box_pack_start(GTK_BOX(page), label, FALSE, FALSE, 2);
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
    gtk_misc_set_padding(GTK_MISC(label), 2, 2);

    hbox = gtk_hbox_new(FALSE, 0);
    gtk_widget_show(hbox);
    gtk_box_pack_start(GTK_BOX(page), hbox, TRUE, TRUE, 0);

    scrolledwindow = gtk_scrolled_window_new(NULL, NULL);
    gtk_widget_show(scrolledwindow);
    gtk_box_pack_start(GTK_BOX(hbox), scrolledwindow, TRUE, TRUE, 0);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwindow),
				   GTK_POLICY_AUTOMATIC,
				   GTK_POLICY_ALWAYS);
    gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW
					(scrolledwindow),
					GTK_SHADOW_ETCHED_IN);

    // Create table list view
    __widget_keyboard_list_model = create_kmfl_list_model();
    __widget_keyboard_list_view =
	gtk_tree_view_new_with_model(GTK_TREE_MODEL
				     (__widget_keyboard_list_model));
    gtk_widget_show(__widget_keyboard_list_view);
    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW
				      (__widget_keyboard_list_view), TRUE);
    gtk_tree_view_set_rules_hint(GTK_TREE_VIEW
				 (__widget_keyboard_list_view), TRUE);
    gtk_container_add(GTK_CONTAINER(scrolledwindow),
		      __widget_keyboard_list_view);

    // Create name column
    namecolumn = column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_reorderable(column, TRUE);
    gtk_tree_view_column_set_sizing(column,
				    GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_sort_column_id(column, TABLE_COLUMN_NAME);

    gtk_tree_view_column_set_title(column, _("Name"));

    renderer = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "pixbuf",
					TABLE_COLUMN_ICON, NULL);

    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text",
					TABLE_COLUMN_NAME, NULL);

    gtk_tree_view_append_column(GTK_TREE_VIEW
				(__widget_keyboard_list_view), column);

    // Create type column.
    column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_reorderable(column, TRUE);
    gtk_tree_view_column_set_sizing(column,
				    GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_sort_column_id(column, TABLE_COLUMN_TYPE);

    gtk_tree_view_column_set_title(column, _("Type"));

    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text",
					TABLE_COLUMN_TYPE, NULL);

    gtk_tree_view_append_column(GTK_TREE_VIEW
				(__widget_keyboard_list_view), column);

    // Create file column.
    column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_reorderable(column, TRUE);
    gtk_tree_view_column_set_sizing(column,
				    GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_sort_column_id(column, TABLE_COLUMN_FILE);

    gtk_tree_view_column_set_title(column, _("File"));

    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text",
					TABLE_COLUMN_FILE, NULL);

    gtk_tree_view_append_column(GTK_TREE_VIEW
				(__widget_keyboard_list_view), column);

    selection =
	gtk_tree_view_get_selection(GTK_TREE_VIEW
				    (__widget_keyboard_list_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

    g_signal_connect(G_OBJECT(selection), "changed",
		     G_CALLBACK(on_keyboard_list_selection_changed), 0);

    // Create buttons.

    vbox = gtk_vbox_new(FALSE, 0);
    gtk_widget_show(vbox);
    gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, TRUE, 4);

    button = gtk_button_new_with_mnemonic(_("_Install"));
    gtk_widget_show(button);
    gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(button), 2);
    gtk_tooltips_set_tip(__widget_tooltips, button,
			 _("Install a new keyboard."), NULL);
    g_signal_connect(G_OBJECT(button), "clicked",
		     G_CALLBACK(on_keyboard_install_clicked), 0);
    __widget_keyboard_install_button = button;


    button = gtk_button_new_with_mnemonic(_("_Delete"));
    gtk_widget_show(button);
    gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(button), 2);
    gtk_tooltips_set_tip(__widget_tooltips, button,
			 _("Delete the selected keyboard."), NULL);
    g_signal_connect(G_OBJECT(button), "clicked",
		     G_CALLBACK(on_keyboard_delete_clicked), 0);
    __widget_keyboard_delete_button = button;

    button = gtk_button_new_with_mnemonic(_("_Properties"));
    gtk_widget_show(button);
    gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(button), 2);
    gtk_tooltips_set_tip(__widget_tooltips, button,
			 _("Edit the properties of the selected table."),
			 NULL);
    g_signal_connect(G_OBJECT(button), "clicked",
		     G_CALLBACK(on_keyboard_properties_clicked), 0);
    __widget_keyboard_properties_button = button;

    gtk_tree_view_column_clicked (namecolumn);
    return page;
}

static GtkWidget *create_setup_window()
{
    static GtkWidget *window = 0;

    if (!window) {
	GtkWidget *notebook;
	GtkWidget *label;
	GtkWidget *page;

	__widget_tooltips = gtk_tooltips_new();

	// Create the Notebook.
	notebook = gtk_notebook_new();
	gtk_widget_show(notebook);

	// Create the second page.
	page = create_kmfl_management_page();

	// Create the label for this note page.
	label = gtk_label_new(_("Keyboard Management"));
	gtk_widget_show(label);

	// Append this page.
	gtk_notebook_append_page(GTK_NOTEBOOK(notebook), page, label);

	window = notebook;

	setup_widget_value();
    }

    return window;
}

void setup_widget_value()
{

    for (int i = 0; __config_keyboards[i].key; ++i) {
	if (__config_keyboards[i].entry) {
	    gtk_entry_set_text(GTK_ENTRY
			       (__config_keyboards[i].entry),
			       __config_keyboards[i].data.c_str());
	}
    }

}

void 
load_config(const ConfigPointer & config)
{
     fprintf(stderr, "Loading config\n");

    if (!config.null()) {
	for (int i = 0; __config_keyboards[i].key; ++i) {
	    __config_keyboards[i].data =
		config->
		read(String(__config_keyboards[i].key),
		     __config_keyboards[i].data);
	}

	setup_widget_value();

	load_all_keyboards();

	__have_changed = false;
    }
    fprintf(stderr, "Loaded config\n");
}

void 
save_config(const ConfigPointer & config)
{
    if (!config.null()) {

	for (int i = 0; __config_keyboards[i].key; ++i) {
	    config->write(String(__config_keyboards[i].key),
			  __config_keyboards[i].data);
	}
	__have_changed = false;
    }
}

bool 
query_changed()
{
    if (__have_changed)
	return true;

    GtkTreeIter iter;
    if (__widget_keyboard_list_model
	&&
	gtk_tree_model_get_iter_first(GTK_TREE_MODEL
				      (__widget_keyboard_list_model),
				      &iter)) {

	XKEYBOARD *keyboard;

	do {
	    gtk_tree_model_get(GTK_TREE_MODEL
			       (__widget_keyboard_list_model),
			       &iter, TABLE_COLUMN_KEYBOARD, &keyboard, -1);
	    /*
	     * if (lib->updated ()) return true; 
	     */
	}
	while (gtk_tree_model_iter_next
	       (GTK_TREE_MODEL(__widget_keyboard_list_model), &iter));
    }
    return false;
}

static void
on_default_editable_changed(GtkEditable * editable, gpointer user_data)
{
    String *str = static_cast < String * >(user_data);

    if (str) {
	*str = String(gtk_entry_get_text(GTK_ENTRY(editable)));
	__have_changed = true;
    }
}

static void
on_default_key_selection_clicked(GtkButton * button, gpointer user_data)
{
    KeyboardConfigData *data =
	static_cast < KeyboardConfigData * >(user_data);

    if (data) {
	GtkWidget *dialog = scim_key_selection_dialog_new(_(data->title));
	gint result;

	scim_key_selection_dialog_set_keys(SCIM_KEY_SELECTION_DIALOG
					   (dialog),
					   gtk_entry_get_text
					   (GTK_ENTRY(data->entry)));

	result = gtk_dialog_run(GTK_DIALOG(dialog));

	if (result == GTK_RESPONSE_OK) {
	    const gchar *keys =
		scim_key_selection_dialog_get_keys
		(SCIM_KEY_SELECTION_DIALOG(dialog));

	    if (!keys)
		keys = "";

	    if (strcmp
		(keys, gtk_entry_get_text(GTK_ENTRY(data->entry))) != 0)
		gtk_entry_set_text(GTK_ENTRY(data->entry), keys);
	}

	gtk_widget_destroy(dialog);
    }
}

// Table manager related functions.
static bool 
test_file_modify(const String & file)
{
    if (access(file.c_str(), W_OK) != 0 && errno != ENOENT) {
	return false;
    }

    return true;
}

static bool test_file_unlink(const String & file)
{
    String path;
    String::size_type pos = file.rfind(SCIM_PATH_DELIM);

    if (pos != String::npos) {
	path = file.substr(0, pos);
    }

    if (!path.length()) {
	path = SCIM_PATH_DELIM_STRING;
    }

    if (access(path.c_str(), W_OK) != 0) {
	return false;
    }

    return true;
}

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
get_keyboard_list(std::vector < String > &keyboard_list,
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

	    if (S_ISREG(filestat.st_mode)
		&& ((absfn.substr(absfn.length() - 5,
				5) == ".kmfl"
		&& kmfl_check_keyboard(absfn.c_str()) == 0)
		|| absfn.substr(absfn.length() - 4, 4) == ".kmn")) {
		keyboard_list.push_back(absfn);
            }

	    file = readdir(dir);
	}
	closedir(dir);
    }
}

static GdkPixbuf *
scale_pixbuf(GdkPixbuf ** pixbuf, int width, int height)
{
    if (pixbuf && *pixbuf) {
	if (gdk_pixbuf_get_width(*pixbuf) != width || 
            gdk_pixbuf_get_height(*pixbuf) != height) {
	    
            GdkPixbuf *dest = gdk_pixbuf_scale_simple(*pixbuf, width,
						      height,
						      GDK_INTERP_BILINEAR);
	    gdk_pixbuf_unref(*pixbuf);
	    *pixbuf = dest;
	}
	return *pixbuf;
    }
    return 0;
}

bool 
filecopy(String srcfile, String destfile)
{
    std::ifstream src;
    std::ofstream dest;
    src.open(srcfile.c_str(), std::ios::in | std::ios::binary);

    if (src) {
	dest.open(destfile.c_str(),
		  std::ios::out | std::ios::trunc | std::ios::binary);
	if (dest) {
	    dest << src.rdbuf();
	    return dest.good();
	}
    }
    return false;
}

XKEYBOARD *
load_kmfl_file(const String & file)
{
    XKEYBOARD *keyboard = 0;
    unsigned int filelen, kbver = 0;
    char version_string[6] = { 0 };
    struct stat fstat;
    FILE *fp;
    const char * extension;
    int errcode;
    
    extension = strrchr(file.c_str(), '.');
   
    if (extension && (strcmp(extension, ".kmn") == 0))
    {

       errcode = setjmp(fatal_error_buf);

        if (errcode == 0)
        {
	    compile_keyboard_to_buffer(file.c_str(), (void **) &keyboard);
	    memcpy(version_string,keyboard->version,3); // Copy to ensure terminated
            kbver = (unsigned)atoi(version_string);
	}
	else
	{
	    return NULL;
	}
    }
    else
    {
	if (file.length()) {
	    // Get the file size
	    if (stat(file.c_str(), &fstat) != 0) {
		return NULL;
	    }
	    
	    filelen = fstat.st_size;
    
	    // Allocate memory for the installed keyboard
	    if ((keyboard = (XKEYBOARD *) malloc(filelen)) == NULL) {
		return NULL;
	    }
    
	    // Open the file
	    if ((fp = fopen(file.c_str(), "rb")) != NULL) {
		if (fread(keyboard, 1, filelen, fp)> 0) {
			memcpy(version_string, keyboard->version, 3);	// Copy to ensure terminated
			kbver = (unsigned) atoi(version_string);
			fclose(fp);
		} else {
			fclose(fp);
			free(keyboard);
			return NULL;
		}
	    }
	    // Check the loaded file is valid and has the correct version
	    if ((memcmp(keyboard->id, "KMFL", 4) != 0)
		|| (keyboard->version[3] != *FILE_VERSION)
		|| (kbver < (unsigned) atoi(BASE_VERSION))
		|| (kbver > (unsigned) atoi(LAST_VERSION))) {
		free(keyboard);
		return NULL;
	    }
	}
    }
    return keyboard;
}

static String
get_static_store(XKEYBOARD * p_kbd, int hdrID)
{
    XSTORE *stores;
    XGROUP *groups, *gp;
    XRULE *rules;
    ITEM *strings;
    UTF32 *p32;
    UTF8 *p8;
    unsigned int n, nrules;
    static char static_store[256];
    *static_store = 0;

    if (p_kbd != NULL) {
	stores = (XSTORE *) (p_kbd + 1);
	groups = (XGROUP *) (stores + p_kbd->nstores);
	rules = (XRULE *) (groups + p_kbd->ngroups);

	for (n = nrules = 0, gp = groups; n < p_kbd->ngroups; n++, gp++) {
	    nrules += gp->nrules;
	}

	strings = (ITEM *) (rules + nrules);

	if (stores[SS_BITMAP].len >= 0) {
	    p32 = strings + stores[hdrID].items;
	    p8 = (UTF8 *) static_store;
	    IConvertUTF32toUTF8((const UTF32 **) &p32,
			       p32 + stores[hdrID].len, &p8,
			       p8 + 255);
	    *p8 = 0;
	}
    }
    
    return String(static_store);    
}

static String
get_icon_name(XKEYBOARD * p_kbd)
{
    return get_static_store(p_kbd, SS_BITMAP);
}

static String
get_icon_file(String icon_name, bool user)
{
    String icon_file;

    if (icon_name.length() == 0) {
	icon_file =
	    SCIM_KMFL_SYSTEM_KEYBOARDS_DIR SCIM_PATH_DELIM_STRING
	    "icons" SCIM_PATH_DELIM_STRING "default.png";
    } else if (user) {
	icon_file =
	    scim_get_home_dir() +
	    SCIM_KMFL_USER_KEYBOARDS_DIR SCIM_PATH_DELIM_STRING
	    "icons" SCIM_PATH_DELIM_STRING + icon_name;
    } else {
	icon_file =
	    SCIM_KMFL_SYSTEM_KEYBOARDS_DIR SCIM_PATH_DELIM_STRING
	    "icons" SCIM_PATH_DELIM_STRING + icon_name;
    }
    
    return icon_file;
}

static void
add_keyboard_to_list(XKEYBOARD * keyboard, const String & dir,
		     const String & file, bool user)
{
    fprintf(stderr, "Adding %s to list\n", keyboard->name);

    if (!keyboard || !__widget_keyboard_list_model) {
	return;
    }

    GtkTreeIter iter;
    GdkPixbuf *pixbuf;
    gchar *name;
    
    String icon_file=get_icon_file(get_icon_name(keyboard), user);
    
    fprintf(stderr, "DAR: loading icon file %s\n", icon_file.c_str());

    pixbuf = gdk_pixbuf_new_from_file(icon_file.c_str(), NULL);

    scale_pixbuf(&pixbuf, LIST_ICON_SIZE, LIST_ICON_SIZE);

    name = g_strdup(keyboard->name);

    gtk_list_store_append(__widget_keyboard_list_model, &iter);

    gtk_list_store_set(__widget_keyboard_list_model, &iter,
		       TABLE_COLUMN_ICON, pixbuf, TABLE_COLUMN_NAME,
		       name, TABLE_COLUMN_FILE, file.c_str(),
		       TABLE_COLUMN_TYPE, user ? _("User") : _("System"),
		       TABLE_COLUMN_KEYBOARD, keyboard,
		       TABLE_COLUMN_IS_USER, user, -1);
    g_free(name);

    if (pixbuf) {
	g_object_unref(pixbuf);
    }

    fprintf(stderr, "Added %s to list\n", keyboard->name);

}

static void load_all_keyboards()
{
    fprintf(stderr, "Loading all keyboards\n");

    if (!__widget_keyboard_list_model) {
	return;
    }

    std::vector < String > usr_keyboards;
    std::vector < String > sys_keyboards;
    std::vector < String >::iterator it;
    XKEYBOARD *keyboard;

    String sys_dir(SCIM_KMFL_SYSTEM_KEYBOARDS_DIR);
    String usr_dir(scim_get_home_dir() + SCIM_KMFL_USER_KEYBOARDS_DIR);

    destroy_all_keyboards();

    get_keyboard_list(sys_keyboards, sys_dir);
    get_keyboard_list(usr_keyboards, usr_dir);

    for (it = sys_keyboards.begin(); it != sys_keyboards.end(); ++it) {
	if ((keyboard = load_kmfl_file(*it)) != 0) {
	    add_keyboard_to_list(keyboard, sys_dir, *it, false);
        }
    }

    for (it = usr_keyboards.begin(); it != usr_keyboards.end(); ++it) {
	if ((keyboard = load_kmfl_file(*it)) != 0) {
	    add_keyboard_to_list(keyboard, usr_dir, *it, true);
        }
    }
    fprintf(stderr, "Loaded all keyboards\n");
}

static gboolean
keyboard_list_destroy_iter_func(GtkTreeModel * model,
				GtkTreePath * path,
				GtkTreeIter * iter, gpointer data)
{
    XKEYBOARD *keyboard;
    gtk_tree_model_get(model, iter, TABLE_COLUMN_KEYBOARD, &keyboard, -1);

    if (keyboard) {
	free(keyboard);
	gtk_list_store_set(GTK_LIST_STORE(model), iter,
			   TABLE_COLUMN_KEYBOARD, 0, -1);
    }

    return FALSE;
}

static void
delete_keyboard_from_list(GtkTreeModel * model, GtkTreeIter * iter)
{
    if (model && iter) {
	keyboard_list_destroy_iter_func(model, 0, iter, 0);
	gtk_list_store_remove(GTK_LIST_STORE(model), iter);
    }
}

static void destroy_all_keyboards()
{
    if (__widget_keyboard_list_model) {
	gtk_tree_model_foreach(GTK_TREE_MODEL
			       (__widget_keyboard_list_model),
			       keyboard_list_destroy_iter_func, 0);
	gtk_list_store_clear(__widget_keyboard_list_model);
    }
}

static void
on_keyboard_list_selection_changed(GtkTreeSelection *
				   selection, gpointer user_data)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    gchar *file = 0;
    bool can_unlink;

    if (__widget_keyboard_delete_button) {
	if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
	    gtk_tree_model_get(model, &iter, TABLE_COLUMN_FILE, &file, -1);
        }

	if (file) {
	    can_unlink = test_file_unlink(file);
	    g_free(file);
	} else {
	    can_unlink = false;
	}

	gtk_widget_set_sensitive(__widget_keyboard_delete_button,
				 can_unlink);
    }
}

static bool
find_keyboard_in_list_by_file(const String & file,
			      GtkTreeIter * iter_found)
{
    GtkTreeIter iter;

    if (__widget_keyboard_list_model &&
	gtk_tree_model_get_iter_first(GTK_TREE_MODEL
				      (__widget_keyboard_list_model),
				      &iter)) {
	do {
	    gchar *fn;
	    gtk_tree_model_get(GTK_TREE_MODEL
			       (__widget_keyboard_list_model),
			       &iter, TABLE_COLUMN_FILE, &fn, -1);
	    if (String(fn) == file) {
		g_free(fn);

		if (iter_found) {
		    *iter_found = iter;
                }

		return true;
	    }
	    g_free(fn);
	}
	while (gtk_tree_model_iter_next
	       (GTK_TREE_MODEL(__widget_keyboard_list_model), &iter));
    }
    return false;
}

static bool
find_keyboard_in_list_by_xkeyboard(XKEYBOARD * xkeyboard,
				   GtkTreeIter * iter_found)
{
    GtkTreeIter iter;

    if (__widget_keyboard_list_model && xkeyboard
	&&
	gtk_tree_model_get_iter_first(GTK_TREE_MODEL
				      (__widget_keyboard_list_model),
				      &iter)) {
	do {
	    XKEYBOARD *keyboard;

	    gtk_tree_model_get(GTK_TREE_MODEL
			       (__widget_keyboard_list_model),
			       &iter, TABLE_COLUMN_KEYBOARD, &keyboard, -1);

	    if (strcmp(keyboard->name, xkeyboard->name) == 0) {
		if (iter_found) {
		    *iter_found = iter;
                }
		return true;
	    }
	}
	while (gtk_tree_model_iter_next
	       (GTK_TREE_MODEL(__widget_keyboard_list_model), &iter));
    }
    return false;
}

static void
on_keyboard_install_clicked(GtkButton * button, gpointer user_data)
{
    GtkWidget *file_selection;
    GtkWidget *msg;
    GtkFileFilter *filter;
    GtkTreeIter iter;
    String file;
    String new_file;
    String path;
    gint result;
    XKEYBOARD *keyboard;
    String::size_type pos;
    bool user_keyboard = true;

    String sys_dir(SCIM_KMFL_SYSTEM_KEYBOARDS_DIR);
    String usr_dir(scim_get_home_dir() + SCIM_KMFL_USER_KEYBOARDS_DIR);

    // Select the keyboard file.
    file_selection = gtk_file_chooser_dialog_new ("Please select the keyboard file to be installed.",
				      NULL,
				      GTK_FILE_CHOOSER_ACTION_OPEN,
				      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
				      GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
				      NULL);

    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, _("Keyboard Files"));
    gtk_file_filter_add_pattern (filter, "*.kmn");
    gtk_file_filter_add_pattern (filter, "*.kmfl");
    gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (file_selection), filter);

    filter = gtk_file_filter_new ();
    gtk_file_filter_set_name (filter, _("All Files"));
    gtk_file_filter_add_pattern (filter, "*");
    gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (file_selection), filter);

    if (gtk_dialog_run (GTK_DIALOG (file_selection)) == GTK_RESPONSE_ACCEPT)
    {
        char * filename;
        filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (file_selection));
        file = String(filename);
        g_free (filename);
    } else {    
    	gtk_widget_destroy(file_selection);
    	return;
    }

    gtk_widget_destroy(file_selection);

    pos = file.rfind(SCIM_PATH_DELIM);

    new_file = usr_dir + SCIM_PATH_DELIM_STRING;

    // Check if the file is already in the table directories.
    if (pos != String::npos) {
	path = file.substr(0, pos);
	if (!path.length()) {
	    path = SCIM_PATH_DELIM_STRING;
    }

	if (path == sys_dir || path == usr_dir) {
	    msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
					 GTK_MESSAGE_ERROR,
					 GTK_BUTTONS_CLOSE,
					 _
					 ("Failed to install the keyboard! "
					  "It's already in kmfl file directory."));
	    gtk_dialog_run(GTK_DIALOG(msg));
	    gtk_widget_destroy(msg);
	    return;
	}
	new_file += file.substr(pos + 1);
    } else {
	new_file += file;
    }

    path = usr_dir;

    // Load the table into memory.
    if ((keyboard = load_kmfl_file(file)) == 0) {
	msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
				     GTK_MESSAGE_ERROR,
				     GTK_BUTTONS_CLOSE,
				     _
				     ("Failed to load the keyboard file!"));
	gtk_dialog_run(GTK_DIALOG(msg));
	gtk_widget_destroy(msg);
	return;
    }

    fprintf(stderr, "DAR: Checking for %s\n", keyboard->name);

    // Find if there is a keyboard with same name was already installed.
    if (find_keyboard_in_list_by_xkeyboard(keyboard, &iter)) {
	gchar *fn;

	gtk_tree_model_get(GTK_TREE_MODEL
			   (__widget_keyboard_list_model), &iter,
			   TABLE_COLUMN_FILE, &fn, -1);
	new_file = String(fn);
	g_free(fn);

	if (!test_file_modify(new_file)) {
	    msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
					 GTK_MESSAGE_ERROR,
					 GTK_BUTTONS_CLOSE,
					 _
					 ("Failed to install the keyboard! "
					  "Another version of this keyboard was already installed."));

	    gtk_dialog_run(GTK_DIALOG(msg));
	    gtk_widget_destroy(msg);

	    free(keyboard);
	    return;
	}

	msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
				     GTK_MESSAGE_QUESTION,
				     GTK_BUTTONS_OK_CANCEL,
				     _
				     ("Another version of this keyboard was already installed. "
				      "Do you want to replace it with the new one?"));
	result = gtk_dialog_run(GTK_DIALOG(msg));
	gtk_widget_destroy(msg);

	if (result != GTK_RESPONSE_OK) {
	    free(keyboard);
	    return;
	}

	delete_keyboard_from_list(GTK_TREE_MODEL
				  (__widget_keyboard_list_model), &iter);

	pos = new_file.rfind(SCIM_PATH_DELIM);
	if (pos != String::npos && pos != 0) {
	    path = new_file.substr(0, pos);
	} else {
	    path = SCIM_PATH_DELIM_STRING;
        }

	if (path == sys_dir) {
	    user_keyboard = false;
        }
    }
    // Find if the file is already existed.
    if (find_keyboard_in_list_by_file(new_file, &iter)) {
	if (!test_file_modify(new_file)) {
	    msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
					 GTK_MESSAGE_ERROR,
					 GTK_BUTTONS_CLOSE,
					 _
					 ("Failed to install the keyboard! "
					  "A keyboard with the same file name was already installed."));

	    gtk_dialog_run(GTK_DIALOG(msg));
	    gtk_widget_destroy(msg);

	    free(keyboard);
	    return;
	}

	msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
				     GTK_MESSAGE_QUESTION,
				     GTK_BUTTONS_OK_CANCEL,
				     _
				     ("A keyboard with the same file name was already installed. "
				      "Do you want to overwrite it?"));
	result = gtk_dialog_run(GTK_DIALOG(msg));
	gtk_widget_destroy(msg);

	if (result != GTK_RESPONSE_OK) {
	    free(keyboard);
	    return;
	}

	delete_keyboard_from_list(GTK_TREE_MODEL
				  (__widget_keyboard_list_model), &iter);
    }

    if (!make_dir(path) && !make_dir(path+SCIM_PATH_DELIM_STRING+"icons")) {
	msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
				     GTK_MESSAGE_ERROR,
				     GTK_BUTTONS_CLOSE,
				     _
				     ("Failed to install the table to %s!"),
				     new_file.c_str());
	gtk_dialog_run(GTK_DIALOG(msg));
	gtk_widget_destroy(msg);

	free(keyboard);
	return;
    }

    if (filecopy(file, new_file)) {
        // let try for the icon file
        String icon_name = get_icon_name(keyboard);
        filecopy(get_dirname(file)+SCIM_PATH_DELIM_STRING+icon_name, 
                 get_icon_file(icon_name, user_keyboard));
	add_keyboard_to_list(keyboard, path, new_file, user_keyboard);
        restart_scim();
    } else {
	msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
				     GTK_MESSAGE_ERROR,
				     GTK_BUTTONS_CLOSE,
				     _
				     ("Failed to install the keyboard %s!"),
				     file.c_str());
	gtk_dialog_run(GTK_DIALOG(msg));
	gtk_widget_destroy(msg);

	free(keyboard);
	return;
    }
}

static void
on_keyboard_delete_clicked(GtkButton * button, gpointer user_data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    GtkTreeSelection *selection;
    GtkWidget *msg;

    selection =
	gtk_tree_view_get_selection(GTK_TREE_VIEW
				    (__widget_keyboard_list_view));

    if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
	gchar *fn;
	String file;

	gtk_tree_model_get(model, &iter, TABLE_COLUMN_FILE, &fn, -1);
	file = String(fn);
	g_free(fn);

	if (!test_file_unlink(file)) {
	    msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
					 GTK_MESSAGE_ERROR,
					 GTK_BUTTONS_CLOSE,
					 _
					 ("Can not delete the file %s!"),
					 file.c_str());
	    gtk_dialog_run(GTK_DIALOG(msg));
	    gtk_widget_destroy(msg);
	    return;
	}

	msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
				     GTK_MESSAGE_QUESTION,
				     GTK_BUTTONS_OK_CANCEL,
				     _
				     ("Are you sure to delete this keyboard file?"));

	gint result = gtk_dialog_run(GTK_DIALOG(msg));
	gtk_widget_destroy(msg);

	if (result != GTK_RESPONSE_OK) {
	    return;
        }

	if (unlink(file.c_str()) != 0) {
	    msg = gtk_message_dialog_new(0, GTK_DIALOG_MODAL,
					 GTK_MESSAGE_ERROR,
					 GTK_BUTTONS_CLOSE,
					 _
					 ("Failed to delete the keyboard file!"));
	    gtk_dialog_run(GTK_DIALOG(msg));
	    gtk_widget_destroy(msg);
	    return;
	} else {
            // Delete the icon file
            XKEYBOARD * keyboard;
            gchar *type;
            bool user;
            gtk_tree_model_get(model, &iter, 
                               TABLE_COLUMN_KEYBOARD, &keyboard,
                               TABLE_COLUMN_TYPE, &type,             
                               TABLE_COLUMN_IS_USER, &user,-1);
            fprintf(stderr, "DAR got keyboard info\n");
            unlink(get_icon_file(get_icon_name(keyboard), user).c_str());
            restart_scim();
        }

	delete_keyboard_from_list(model, &iter);
    }
}


static gint
run_keyboard_properties_dialog(XKEYBOARD * keyboard,
			       KeyboardPropertiesData & data,
			       bool editable)
{
    GtkWidget *dialog;
    GtkWidget *dialog_vbox;
    GtkWidget *dialog_action_area;
    GtkWidget *scrolledwindow;
    GtkWidget *viewport;
    GtkWidget *table;
    GtkWidget *label;
    GtkWidget *hbox;
    GtkWidget *entry_name;
    GtkWidget *entry_author;
    GtkWidget *entry_icon;
    GtkWidget *entry_locales;
    GtkWidget *entry_copyright;
    GtkWidget *okbutton;

    gint result = GTK_RESPONSE_CANCEL;

    {				// Create dialog.
	dialog = gtk_dialog_new();
	gtk_container_set_border_width(GTK_CONTAINER(dialog), 2);
	gtk_window_set_title(GTK_WINDOW(dialog), _("Table Properties"));
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);
	gtk_dialog_set_has_separator(GTK_DIALOG(dialog), FALSE);

	dialog_vbox = GTK_DIALOG(dialog)->vbox;
	gtk_widget_show(dialog_vbox);

	scrolledwindow = gtk_scrolled_window_new(NULL, NULL);
	gtk_widget_show(scrolledwindow);
	gtk_box_pack_start(GTK_BOX(dialog_vbox), scrolledwindow,
			   TRUE, TRUE, 0);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW
				       (scrolledwindow),
				       GTK_POLICY_NEVER,
				       GTK_POLICY_ALWAYS);

	viewport = gtk_viewport_new(NULL, NULL);
	gtk_widget_show(viewport);
	gtk_container_add(GTK_CONTAINER(scrolledwindow), viewport);

	table = gtk_table_new(18, 2, FALSE);
	gtk_widget_show(table);
	gtk_container_add(GTK_CONTAINER(viewport), table);
	gtk_table_set_row_spacings(GTK_TABLE(table), 2);
	gtk_table_set_col_spacings(GTK_TABLE(table), 2);

	label = gtk_label_new(_("Name:"));
	gtk_widget_show(label);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 0, 1,
			 (GtkAttachOptions) (GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	entry_name = gtk_label_new(data.name.c_str());
	gtk_widget_show(entry_name);
	gtk_table_attach(GTK_TABLE(table), entry_name, 1, 2, 0, 1,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(entry_name), 0, 0.5);

	label = gtk_label_new(_("Author:"));
	gtk_widget_show(label);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 1, 2,
			 (GtkAttachOptions) (GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	entry_author = gtk_label_new(data.author.c_str());
	gtk_widget_show(entry_author);
	gtk_table_attach(GTK_TABLE(table), entry_author, 1, 2, 1, 2,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(entry_author), 0, 0.5);

	label = gtk_label_new(_("Copyright:"));
	gtk_widget_show(label);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 6, 7,
			 (GtkAttachOptions) (GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	entry_copyright = gtk_label_new(data.copyright.c_str());
	gtk_widget_show(entry_copyright);
	gtk_table_attach(GTK_TABLE(table), entry_copyright, 1,
			 2, 6, 7,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(entry_copyright), 0, 0.5);


	label = gtk_label_new(_("Icon File:"));
	gtk_widget_show(label);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 4, 5,
			 (GtkAttachOptions) (GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(hbox);
	gtk_table_attach(GTK_TABLE(table), hbox, 1, 2, 4, 5,
			 (GtkAttachOptions) (GTK_FILL),
			 (GtkAttachOptions) (GTK_FILL), 0, 0);

	entry_icon = gtk_label_new(data.icon.c_str());
	gtk_widget_show(entry_icon);
	gtk_box_pack_start(GTK_BOX(hbox), entry_icon, TRUE, TRUE, 0);
	gtk_misc_set_alignment(GTK_MISC(entry_icon), 0, 0.5);

	label = gtk_label_new(_("Language:"));
	gtk_widget_show(label);
	gtk_table_attach(GTK_TABLE(table), label, 0, 1, 5, 6,
			 (GtkAttachOptions) (GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);

	entry_locales = gtk_label_new(data.locales.c_str());
	gtk_widget_show(entry_locales);
	gtk_table_attach(GTK_TABLE(table), entry_locales, 1, 2, 5,
			 6,
			 (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
			 (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(entry_locales), 0, 0.5);

	dialog_action_area = GTK_DIALOG(dialog)->action_area;
	gtk_widget_show(dialog_action_area);
	gtk_button_box_set_layout(GTK_BUTTON_BOX
				  (dialog_action_area), GTK_BUTTONBOX_END);

	okbutton = gtk_button_new_from_stock("gtk-ok");
	gtk_widget_show(okbutton);
	gtk_dialog_add_action_widget(GTK_DIALOG(dialog), okbutton,
				     GTK_RESPONSE_OK);
	GTK_WIDGET_SET_FLAGS(okbutton, GTK_CAN_DEFAULT);
    }


    {				// Run the dialog and return the result;
	gtk_window_set_default_size(GTK_WINDOW(dialog), 560, 400);

    gtk_dialog_run(GTK_DIALOG(dialog));


	gtk_widget_destroy(dialog);
    }

    return result;
}

static void
on_keyboard_properties_clicked(GtkButton * button, gpointer user_data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    GtkTreeSelection *selection;

    selection =
	gtk_tree_view_get_selection(GTK_TREE_VIEW
				    (__widget_keyboard_list_view));

    if (gtk_tree_selection_get_selected(selection, &model, &iter)) {
	XKEYBOARD *keyboard;
	gchar *file;
        gchar *type;
        bool user;
        
	gtk_tree_model_get(model, &iter, TABLE_COLUMN_KEYBOARD,
			   &keyboard, TABLE_COLUMN_FILE, &file, TABLE_COLUMN_TYPE, &type, TABLE_COLUMN_IS_USER, &user, -1);

	if (!keyboard || !file) {
	    g_free(file);
	    return;
	}

	KeyboardPropertiesData data, olddata;
	gint result;

	data.name = keyboard->name;
	data.author = get_static_store(keyboard, SS_AUTHOR);	
	if (data.author.length() == 0)
	    data.author = String("None specified");
	
	data.locales = get_static_store(keyboard, SS_LANGUAGE);
	if (data.locales.length() == 0)
	    data.locales = String("None specified");
	data.icon = get_icon_file(get_icon_name(keyboard), user);
	data.copyright = get_static_store(keyboard, SS_COPYRIGHT);

	olddata = data;

	result = run_keyboard_properties_dialog(keyboard, data,
						test_file_modify(file));

	g_free(file);

    }
}

/*
 * vi:ts=4:nowrap:expandtab 
 */
