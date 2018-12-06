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
    kmp_details details;

    if (argc < 2)
    {
      g_print ("Usage: kmpdetails <kmp.json>\n");
      g_print ("ERROR: no file specified\n");
      return EXIT_FAILURE;
    }

    stat(argv[1], &filestat);
    if (!S_ISREG(filestat.st_mode))
    {
      g_print ("Usage: kmpdetails <kmp.json>\n");
      g_print ("ERROR: file %s not found\n", argv[1]);
      return EXIT_FAILURE;
    }

    setlocale (LC_ALL, "C.UTF-8");

    get_kmp_details(argv[1], &details);
    print_kmp_details(&details);
    free_kmp_details(&details);

    return EXIT_SUCCESS;
}

