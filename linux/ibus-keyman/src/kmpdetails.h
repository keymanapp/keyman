// kmp experiments

// structs like this

#include <glib.h>
struct kmp_system
{
    gchar *fileVersion;
    gchar *keymanDeveloperVersion;
};

struct kmp_options
{
    gchar *readmeFile;
    gchar *graphicFile;
};

struct kmp_details
{
    struct kmp_system system;
    struct kmp_options options;
};

// or a GObject
// https://developer.gnome.org/gobject/stable/chapter-gobject.html
// ???

// directly same structure as json?
// or is there a structure more useful for ibus-keyman?

// what do we care about knowing?
// system: don't think so
// options: readme?
// info: version, name, copyright, author, website
// files: kvk files - and store ldmlfile as well generated from that
//            want to associate the kvk file with the keyboard
//            just use this entry to verify that the keyboard has an OSK?
//        kmx files? or just use the id from the keyboard?
//             or again use the entry to verify that the keyboard has a kmx
//                 and isn't just a js keyboard?
// keyboards: list of them
//    name, id, version
//    generate kvkfile, kmxfile and ldmlfile and use files to verify them
//    list of languages (name, id)


// just throw away the rest or store in case needed later?
// it's still there is the json so leave it for later

// then there is the keyboard_id.json file from https://api.keyman.com/keyboard/keyboard_id
// id, name, license, version, languages, authorName, authorEmail, description, lastModifiedDate
// sourcePath gives a relative path in the keyboards repo to the source
// id, name and version and languages and authorName and authorEmail should match what is in kmp.json
// license, description and lastModifiedDate and sourcePath might be useful to add to keyboard

// the lastModifiedDate and version in that json file will be useful for checking for updates
// when I work out how to include that in km-config
// it is 1-2kb for checking each keyboard
// how often does windows check and what does it check?

// and while I'm thinking - what will be the "Preferences" app for the ibus-keyman
// keyboards in the ibus-setup input methods tab?
