#ifndef _KL95API_H
#define _KL95API_H

typedef struct tagKEYMANLOCALEINFO {
	BOOL	fIsUserDefLocale;
	DWORD	dwLocale;
	char	szLayoutName[80];
	char	szName[80];
	HBITMAP hBitmap;
	} KEYMANLOCALEINFO, FAR *LPKEYMANLOCALEINFO;

BOOL WINAPI GetKeymanLocaleInfo( DWORD locale, LPKEYMANLOCALEINFO kli );
HKL WINAPI SelectLocaleKeyboard( DWORD locale, DWORD layout );
int WINAPI GetLocaleKeyboardList( DWORD locale, int n, DWORD *list );

#endif	// _KL95API_H
