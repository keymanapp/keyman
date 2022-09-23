/*
  Name:             kmdecomp
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      12 Oct 2012

  Modified Date:    12 Oct 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          12 Oct 2012 - mcdurdin - I3467 - V9.0 - Upgrade KMDECOMP to compile with KM9 source tree
*/

#include "pch.h"

#include <string.h>
#include <stdio.h>
#include "../../../common/windows/cpp/include/keymansentry.h"
#include "../../../common/windows/cpp/include/keymanversion.h"
#include "../../../common/windows/cpp/include/legacy_kmx_memory.h"
#include "../../../common/windows/cpp/include/legacy_kmx_file.h"

BOOL LoadKeyboard(LPSTR fileName, LPKEYBOARD *lpKeyboard, LPBYTE *lpBitmap, DWORD *cbBitmap);

void Err(char *p);
int SaveKeyboardSource(LPKEYBOARD kbd, LPBYTE lpBitmap, DWORD cbBitmap, char *filename, char *bmpfile);
int run(int argc, char *argv[]);

/*
 * Keyman
 *
 * Version 6.0
 *
 * KMDFILE compiled file loading
 *
 */

#define KEYMAN_SENTRY_LOGGER_DEVELOPER_TOOLS_KMDECOMP KEYMAN_SENTRY_LOGGER_DEVELOPER_TOOLS ".kmdecomp"

int main(int argc, char *argv[])
{
  return keyman_sentry_main(TRUE, KEYMAN_SENTRY_LOGGER_DEVELOPER_TOOLS_KMDECOMP, argc, argv, run);
}

int run(int argc, char *argv[])
{
	LPKEYBOARD kbd;
	LPBYTE lpBitmap;
	DWORD cbBitmap;
	char buf[_MAX_PATH], buf2[_MAX_PATH], drive[_MAX_DRIVE], dir[_MAX_DIR], filename[_MAX_FNAME];

	if(argc < 2 || !strcmp(argv[1], "--help"))
	{
    printf(
      "KMDECOMP: Decompile Keyman .kmx keyboard\n"
      "Version %s, %s\n"
      "Usage: KMDECOMP <filename> [output]\n"
      "Will create a.kmn and optionally a.bmp / .ico with the same filename and in the same location as the input",
      KEYMAN_VersionWithTag, KEYMAN_Copyright
    );
		return 1;
	}

	if(!LoadKeyboard(argv[1], &kbd, &lpBitmap, &cbBitmap)) return 2;

  if (argc < 3) {
    _splitpath_s(argv[1], drive, _MAX_DRIVE, dir, _MAX_DIR, filename, _MAX_FNAME, NULL, 0);
  }
  else {
    _splitpath_s(argv[2], drive, _MAX_DRIVE, dir, _MAX_DIR, filename, _MAX_FNAME, NULL, 0);
  }

	_makepath_s(buf, _MAX_PATH, drive, dir, filename, ".kmn");
	_makepath_s(buf2, _MAX_PATH, drive, dir, filename, ".bmp");

	int n = SaveKeyboardSource(kbd, lpBitmap, cbBitmap, buf, buf2);

	if(lpBitmap) delete lpBitmap;

	delete kbd;
	return n;
}


void Err(char *p)
{
	printf("Fatal Error: %s\n", p);
}

BOOL LoadKeyboard(LPSTR fileName, LPKEYBOARD *lpKeyboard, LPBYTE *lpBitmap, DWORD *cbBitmap)
{
	DWORD sz, i, j;
	LPBYTE buf;
	HANDLE hFile;
	PCOMP_KEYBOARD ckbp;
	PCOMP_GROUP cgp;
	PCOMP_STORE csp;
	PCOMP_KEY ckp;
	LPKEYBOARD kbp;
	LPGROUP gp;
	LPSTORE sp;
	LPKEY kp;

	if(!lpKeyboard)
	{
		Err("Internal error 001");
		return FALSE;
	}

	if(!fileName)
	{
		Err("Bad Filename");
		return FALSE;
	}

	hFile = CreateFile(fileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
	if(hFile == INVALID_HANDLE_VALUE)
	{
		Err("Could not open file");
		return FALSE;
	}

	sz = GetFileSize(hFile, NULL);

	buf = new BYTE[sz];

	if(!buf)
	{
		CloseHandle(hFile);
		Err("Could not allocate memory");
		return FALSE;
	}

	ReadFile(hFile, buf, sz, &sz, NULL);

	CloseHandle(hFile);

	kbp = (LPKEYBOARD) buf;
	ckbp = (PCOMP_KEYBOARD) buf;

	if(kbp->dwIdentifier != FILEID_COMPILED) { delete buf; Err("errNotFileID"); return FALSE; }

	/* Check file version */

	if(ckbp->dwFileVersion < VERSION_MIN ||
	   ckbp->dwFileVersion > VERSION_MAX)
	{
		/* Old or new version -- identify the desired program version */
		kbp->dpStoreArray = (LPSTORE) (buf + ckbp->dpStoreArray);
		for(sp = kbp->dpStoreArray, i = 0; i < kbp->cxStoreArray; i++, sp++) {
			if(sp->dwSystemID == TSS_COMPILEDVERSION)
			{
				char buf2[256];
				wsprintf(buf2, "Wrong File Version: file version is %ls", ((PBYTE)kbp) + (INT_PTR)sp->dpString);
				delete buf;
				Err(buf2);
				return FALSE;
			}
		}
		delete buf; Err("Unknown File Version: try using the latest version of KMDECOMP");
		return FALSE;
	}

	kbp->dpStoreArray = (LPSTORE) (buf + ckbp->dpStoreArray);
	kbp->dpGroupArray = (LPGROUP) (buf + ckbp->dpGroupArray);

	//kbp->dpName = (PWSTR) (buf + ckbp->dpName);
	//kbp->dpCopyright = (PWSTR) (buf + ckbp->dpCopyright);
	//kbp->dpMessage = (PWSTR) (buf + ckbp->dpMessage);
	//kbp->dpLanguageName = (PWSTR) (buf + ckbp->dpLanguageName);

	if(ckbp->dwBitmapSize > 0)
	{
	  *lpBitmap = new BYTE[ckbp->dwBitmapSize];
	  *cbBitmap = ckbp->dwBitmapSize;
	  memcpy(*lpBitmap, buf + ckbp->dpBitmapOffset, *cbBitmap);
	}
	else
	{
	  *lpBitmap = NULL;
	  *cbBitmap = 0;
	}

	for(sp = kbp->dpStoreArray, csp = (PCOMP_STORE) sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)
	{
		if(csp->dpName > 0) sp->dpName = (PWSTR) (buf + csp->dpName); else sp->dpName = NULL;
		sp->dpString = (PWSTR) (buf + csp->dpString);
	}

	for(gp = kbp->dpGroupArray, cgp = (PCOMP_GROUP) gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)
	{
		if(cgp->dpName > 0) gp->dpName = (PWSTR) (buf + cgp->dpName); else gp->dpName = NULL;
		gp->dpKeyArray = (LPKEY) (buf + cgp->dpKeyArray);
		if(cgp->dpMatch != NULL) gp->dpMatch = (PWSTR) (buf + cgp->dpMatch);
		if(cgp->dpNoMatch != NULL) gp->dpNoMatch = (PWSTR) (buf + cgp->dpNoMatch);

		for(kp = gp->dpKeyArray, ckp = (PCOMP_KEY) kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++)
		{
			kp->dpOutput = (PWSTR) (buf + ckp->dpOutput);
			kp->dpContext = (PWSTR) (buf + ckp->dpContext);
		}
	}

	*lpKeyboard = kbp;

	return TRUE;
}

