PWSTR incxstr(PWSTR p);
PWSTR decxstr(PWSTR p, PWSTR pStart);
int xstrlen(PWSTR p);
int xstrlen_ignoreifopt(PWSTR p);
int xstrpos(PWSTR p1, PWSTR p);
PWSTR xstrchr(PWSTR buf, PWSTR chr);
int xchrcmp(PWSTR ch1, PWSTR ch2);
//**********************************************************************************
PKMX_WCHAR incxstr(PKMX_WCHAR p);
PKMX_WCHAR KMX_incxstr(PKMX_WCHAR p);
//PKMX_WCHAR KMX_decxstr(PKMX_WCHAR p, PKMX_WCHAR pStart);
int KMX_xstrlen(PKMX_WCHART p);
int KMX_xstrlen(PKMX_WSTR p);           // just for intermediate use - can be removed later
//int KMX_xstrlen_ignoreifopt(PKMX_WCHAR p);
//int KMX_xstrpos(PKMX_WCHAR p1, PKMX_WCHAR p);
//PWSTR KMX_xstrchr(PKMX_WCHAR buf, PKMX_WCHAR chr);
//int KMX_xchrcmp(PKMX_WCHAR ch1, PKMX_WCHAR ch2);

//const PKMX_WCHAR u16cpy(PKMX_WCHAR dst, const PKMX_WCHAR  src);  // TODO: deprecate all usages  Ursprung
