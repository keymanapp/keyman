#ifndef __KMPDETAILS_H__
#define __KMPDETAILS_H__

// kmp details from json

#include <glib.h>
typedef struct
{
    gchar *fileVersion;
    gchar *keymanDeveloperVersion;
} kmp_system;

typedef struct
{
    gchar *version;
    gchar *name;
    gchar *copyright;
    gchar *author_desc;
    gchar *author_url;
    gchar *website_desc;
    gchar *website_url;
} kmp_info;


typedef struct
{
    gchar *readmeFile;
    gchar *graphicFile;
} kmp_options;

typedef struct
{
    gchar *name;
    gchar *id;
    gchar *version;
    gchar *kmx_file;
    gchar *kvk_file;
    GList *languages;
} kmp_keyboard;

typedef struct
{
    gchar *name;
    gchar *id;
} kmp_language;

typedef struct
{
    gchar *name;
    gchar *description;
} kmp_fileinfo;

typedef struct
{
    gchar *id;
    gchar *description;
    gchar *license;
} keyboard_details;

typedef struct
{
    kmp_system system;
    kmp_info info;
    kmp_options options;
    GList *keyboards;
    GList *files;
} kmp_details;

enum kmp_json_status_codes {
    JSON_OK,
    JSON_PARSE_ERROR,
    JSON_FILE_NOT_EXISTS
};

typedef int kmp_json_status;

kmp_json_status get_kmp_details(const gchar *kmp_dir, kmp_details *details);
kmp_json_status free_kmp_details(kmp_details * details);
kmp_json_status get_keyboard_details(const gchar *kmp_dir, const gchar *id, keyboard_details *details);
kmp_json_status free_keyboard_details(keyboard_details * details);
kmp_json_status print_kmp_details(kmp_details * details);

#endif // __KMPDETAILS_H__
