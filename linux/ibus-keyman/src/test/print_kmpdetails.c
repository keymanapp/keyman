// APIs to get info that ibus-keyman will want from
// the kmp.json from each kmp
//
// get list of keyboards from this
// associated languages

#include <locale.h>
#include <sys/stat.h>
#include "kmpdetails.h"


int
main (gint argc, gchar **argv)
{
    struct stat filestat;
    g_autofree gchar *kmp_json = NULL;
    g_autoptr(kmp_details) details = g_new0(kmp_details, 1);

    if (argc < 2)
    {
      g_print ("Usage: kmpdetails <kmp.json>\n");
      g_print ("ERROR: no file specified\n");
      return EXIT_FAILURE;
    }

    kmp_json = g_strdup_printf("%s/kmp.json", argv[1]);
    stat(kmp_json, &filestat);
    if (!S_ISREG(filestat.st_mode))
    {
      g_print ("Usage: kmpdetails <path to kmp.json>\n");
      g_print ("ERROR: file %s not found\n", kmp_json);
      return EXIT_FAILURE;
    }

    setlocale(LC_ALL, "C.UTF-8");

    get_kmp_details(argv[1], details);
    print_kmp_details(details);

    return EXIT_SUCCESS;
}

