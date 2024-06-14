
#include "pch.h"

// These four errors are copied from kmn_compiler_errors.h, because WriteCompiledKeyboard is
// a clone of the compiler's equivalent function. However, the functions
// diverge, as mc_savekeyboard.cpp's version is copying from an existing
// compiled keyboard. The error codes have been kept consistent with those in
// kmn_compiler_errors.h
#define CERR_None                                          0x00000000
#define CERR_CannotAllocateMemory                          0x00008004
#define CERR_UnableToWriteFully                            0x00008007
#define CERR_SomewhereIGotItWrong                          0x00008009

DWORD WriteCompiledKeyboard(LPKEYBOARD fk, HANDLE hOutfile, BOOL FSaveDebug);

BOOL SaveKeyboard(LPKEYBOARD kbd, PWSTR filename) {
	HANDLE hOutfile = CreateFile(filename, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, 0, NULL);
	if(hOutfile == INVALID_HANDLE_VALUE) {
    LogError(L"Failed to create output file (%d)", GetLastError());
    return FALSE;
  }

  DWORD err = WriteCompiledKeyboard(kbd, hOutfile, FALSE);

  CloseHandle(hOutfile);

  if(err != CERR_None) {
    LogError(L"Failed to write compiled keyboard with error %d", err);
    DeleteFile(filename);
    return FALSE;
  }

  return TRUE;
}

DWORD WriteCompiledKeyboard(LPKEYBOARD fk, HANDLE hOutfile, BOOL FSaveDebug)
{
	LPGROUP fgp;
	LPSTORE fsp;
	LPKEY fkp;

	PCOMP_KEYBOARD ck;
	PCOMP_GROUP gp;
	PCOMP_STORE sp;
	PCOMP_KEY kp;
	PBYTE buf;
	DWORD size, offset;
	DWORD i, j;

	// Calculate how much memory to allocate

	size = sizeof(COMP_KEYBOARD) +
			fk->cxGroupArray * sizeof(COMP_GROUP) +
			fk->cxStoreArray * sizeof(COMP_STORE) +
			/*wcslen(fk->szName)*2 + 2 +
			wcslen(fk->szCopyright)*2 + 2 +
			wcslen(fk->szLanguageName)*2 + 2 +
			wcslen(fk->szMessage)*2 + 2 +*/
			fk->dwBitmapSize;

	for(i = 0, fgp = fk->dpGroupArray; i < fk->cxGroupArray; i++, fgp++) {
    if(fgp->dpName)
	    size += wcslen(fgp->dpName)*2 + 2;
		size += fgp->cxKeyArray * sizeof(COMP_KEY);
		for(j = 0, fkp = fgp->dpKeyArray; j < fgp->cxKeyArray; j++, fkp++) {
			size += wcslen(fkp->dpOutput)*2 + 2;
			size += wcslen(fkp->dpContext)*2 + 2;
		}

		if (fgp->dpMatch ) size += wcslen(fgp->dpMatch)*2 + 2;
		if (fgp->dpNoMatch ) size += wcslen(fgp->dpNoMatch)*2 + 2;
	}

	for(i = 0; i < fk->cxStoreArray; i++)
	{
		size += wcslen(fk->dpStoreArray[i].dpString)*2 + 2;
    if(fk->dpStoreArray[i].dpName)
      size += wcslen(fk->dpStoreArray[i].dpName)*2 + 2;
	}

	buf = new BYTE[size];
	if(!buf) return CERR_CannotAllocateMemory;
	memset(buf, 0, size);

	ck = (PCOMP_KEYBOARD) buf;

	ck->dwIdentifier = FILEID_COMPILED;

  ck->dwFileVersion = fk->dwFileVersion;
	ck->dwCheckSum = 0; // No checksum in 16.0, see #7276
	ck->KeyboardID = fk->xxkbdlayout;
  ck->IsRegistered = fk->IsRegistered;
	ck->cxStoreArray = fk->cxStoreArray;
	ck->cxGroupArray = fk->cxGroupArray;
	ck->StartGroup[0] = fk->StartGroup[0];
	ck->StartGroup[1] = fk->StartGroup[1];
	ck->dwHotKey = fk->dwHotKey;

	ck->dwFlags = fk->dwFlags;

	offset = sizeof(COMP_KEYBOARD);

	/*ck->dpLanguageName = offset;
	wcscpy((PWSTR)(buf + offset), fk->szLanguageName);
	offset += wcslen(fk->szLanguageName)*2 + 2;

	ck->dpName = offset;
	wcscpy((PWSTR)(buf + offset), fk->szName);
	offset += wcslen(fk->szName)*2 + 2;

	ck->dpCopyright = offset;
	wcscpy((PWSTR)(buf + offset), fk->szCopyright);
	offset += wcslen(fk->szCopyright)*2 + 2;

	ck->dpMessage = offset;
	wcscpy((PWSTR)(buf + offset), fk->szMessage);
	offset += wcslen(fk->szMessage)*2 + 2;*/

	ck->dpStoreArray = offset;
	sp = (PCOMP_STORE)(buf+offset);
	fsp = fk->dpStoreArray;
	offset += sizeof(COMP_STORE) * ck->cxStoreArray;
	for(i = 0; i < ck->cxStoreArray; i++, sp++, fsp++) {
		sp->dwSystemID = fsp->dwSystemID;
		sp->dpString = offset;
		wcscpy_s((PWSTR)(buf+offset), (size-offset) / sizeof(WCHAR), fsp->dpString);  // I3481   // I3641
		offset += wcslen(fsp->dpString)*2 + 2;

    if(!fsp->dpName) {
      sp->dpName = 0;
    } else {
  		sp->dpName = offset;
	  	wcscpy_s((PWSTR)(buf+offset), (size-offset) / sizeof(WCHAR), fsp->dpName);  // I3481   // I3641
		  offset += wcslen(fsp->dpName)*2 + 2;
    }
	}

	ck->dpGroupArray = offset;
	gp = (PCOMP_GROUP)(buf+offset);
	fgp = fk->dpGroupArray;

	offset += sizeof(COMP_GROUP) * ck->cxGroupArray;

	for(i = 0; i < ck->cxGroupArray; i++, gp++, fgp++) {
		gp->cxKeyArray = fgp->cxKeyArray;
		gp->fUsingKeys = fgp->fUsingKeys;

		gp->dpMatch = gp->dpNoMatch = 0;

		if(fgp->dpMatch) {
			gp->dpMatch = offset;
			wcscpy_s((PWSTR)(buf+offset), (size-offset) / sizeof(WCHAR), fgp->dpMatch);  // I3481   // I3641
			offset += wcslen(fgp->dpMatch)*2 + 2;
		}
		if(fgp->dpNoMatch) {
			gp->dpNoMatch = offset;
			wcscpy_s((PWSTR)(buf+offset), (size-offset) / sizeof(WCHAR), fgp->dpNoMatch);  // I3481   // I3641
			offset += wcslen(fgp->dpNoMatch)*2 + 2;
		}

    if(fgp->dpName) {
			gp->dpName = offset;
			wcscpy_s((PWSTR)(buf+offset), (size-offset) / sizeof(WCHAR), fgp->dpName);  // I3481   // I3641
			offset += wcslen(fgp->dpName)*2 + 2;
		}	else {
      gp->dpName = 0;
    }

		gp->dpKeyArray = offset;
		kp = (PCOMP_KEY) (buf + offset);
		fkp = fgp->dpKeyArray;
		offset += gp->cxKeyArray * sizeof(COMP_KEY);
		for(j = 0; j < gp->cxKeyArray; j++, kp++, fkp++) {
			kp->Key = fkp->Key;
			kp->Line = fkp->Line;
			kp->ShiftFlags = fkp->ShiftFlags;
			kp->dpOutput = offset;
			wcscpy_s((PWSTR)(buf+offset), (size-offset) / sizeof(WCHAR), fkp->dpOutput);  // I3481   // I3641
			offset += wcslen(fkp->dpOutput)*2 + 2;


  		kp->dpContext = offset;
	  	wcscpy_s((PWSTR)(buf+offset), (size-offset) / sizeof(WCHAR), fkp->dpContext);  // I3481   // I3641
		  offset += wcslen(fkp->dpContext)*2 + 2;
		}
	}

  if(fk->dwBitmapSize > 0) {
  	ck->dwBitmapSize = fk->dwBitmapSize;
	  ck->dpBitmapOffset = offset;
    memcpy(buf + offset, ((PBYTE)fk) + fk->dpBitmapOffset, fk->dwBitmapSize);
	  offset += fk->dwBitmapSize;
  } else {
  	ck->dwBitmapSize = 0;
	  ck->dpBitmapOffset = 0;
  }

	if(offset != size)
  {
    delete[] buf;
    return CERR_SomewhereIGotItWrong;
  }

	WriteFile(hOutfile, buf, size, &offset, NULL);

	if(offset != size)
  {
    delete[] buf;
    return CERR_UnableToWriteFully;
  }

	delete[] buf;

	return CERR_None;
}
