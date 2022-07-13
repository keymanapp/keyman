
#include "kmx_u16.h"

PKMX_WCHAR incxstr(PKMX_WCHAR p);
PKMX_WCHAR decxstr(PKMX_WCHAR p, PKMX_WCHAR pStart);
int xstrlen(PKMX_WCHAR p);
int  xstrlen_ignoreifopt(PKMX_WCHAR p);
int  xstrpos(PKMX_WCHAR p1, PKMX_WCHAR p);
PKMX_WCHAR  xstrchr(PKMX_WCHAR buf, PKMX_WCHAR chr);
int  xchrcmp(PKMX_WCHAR ch1, PKMX_WCHAR ch2);

//****old ******************************************************************************
//PWSTR incxstr(PWSTR p);
//PWSTR decxstr(PWSTR p, PWSTR pStart);
//int xstrlen(PWSTR p);
//int xstrlen_ignoreifopt(PWSTR p);
//int xstrpos(PWSTR p1, PWSTR p);
//PWSTR xstrchr(PWSTR buf, PWSTR chr);
//int xchrcmp(PWSTR ch1, PWSTR ch2);