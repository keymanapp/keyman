#include "stdafx.h"


static BOOL LoadKeyboardFile(LPSTR fileName, LPKEYBOARD *lpKeyboard);
BOOL VerifyKeyboard(LPBYTE filebase, DWORD sz);

LPKEYBOARD FixupKeyboard(PBYTE bufp, PBYTE base, DWORD dwFileSize);

void Err(wchar_t *s) {
	LogError(L"LoadKeyboard: %s, last error = %d\n", s, GetLastError());
}

BOOL LoadKeyboard(LPWSTR fileName, LPKEYBOARD *lpKeyboard) {
	DWORD sz;
	LPBYTE buf;
	HANDLE hFile;
	LPKEYBOARD kbp;
  PBYTE filebase;

	if(!fileName || !lpKeyboard) {
		Err(L"Bad Filename");
		return FALSE;
	}

	hFile = CreateFile(fileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
	if(hFile == INVALID_HANDLE_VALUE) {
		Err(L"Could not open file");
		return FALSE;
	}

	sz = GetFileSize(hFile, NULL);

	buf = new BYTE[sz];

	if(!buf) {
		Err(L"Not allocmem");
		CloseHandle(hFile);
		return FALSE;
	}

  filebase = buf;

	ReadFile(hFile, filebase, sz, &sz, NULL);
	CloseHandle(hFile);

	if(!VerifyKeyboard(filebase, sz)) {
    Err(L"errVerifyKeyboard");
    delete buf; 
    return FALSE;
  }

	kbp = FixupKeyboard(buf, filebase, sz);
  if(!kbp) {
    Err(L"errFixupKeyboard");
    delete buf; 
    return FALSE;
  }

	if(kbp->dwIdentifier != FILEID_COMPILED) { 
    Err(L"errNotFileID"); 
    delete buf; 
    return FALSE; 
  }

	*lpKeyboard = kbp;
	return TRUE;
}

PWCHAR StringOffset(PBYTE base, DWORD offset) {
  if(offset == 0) return NULL;
  return (PWCHAR)(base + offset);
}

LPKEYBOARD FixupKeyboard(PBYTE bufp, PBYTE base, DWORD dwFileSize) {
  UNREFERENCED_PARAMETER(dwFileSize);

  DWORD i, j;
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) base;
  PCOMP_GROUP cgp;
  PCOMP_STORE csp;
  PCOMP_KEY ckp;
  LPKEYBOARD kbp = (LPKEYBOARD) bufp;
  LPSTORE sp;
  LPGROUP gp;
  LPKEY kp;

	kbp->dpStoreArray = (LPSTORE) (base + ckbp->dpStoreArray);
	kbp->dpGroupArray = (LPGROUP) (base + ckbp->dpGroupArray);

	for(sp = kbp->dpStoreArray, csp = (PCOMP_STORE) sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)	{
    sp->dpName = StringOffset(base, csp->dpName);
		sp->dpString = StringOffset(base, csp->dpString);
	}

	for(gp = kbp->dpGroupArray, cgp = (PCOMP_GROUP) gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)	{
    gp->dpName = StringOffset(base, cgp->dpName); 
		gp->dpKeyArray = (LPKEY) (base + cgp->dpKeyArray);
		if(cgp->dpMatch != NULL) gp->dpMatch = (PWSTR) (base + cgp->dpMatch);
		if(cgp->dpNoMatch != NULL) gp->dpNoMatch = (PWSTR) (base + cgp->dpNoMatch);

		for(kp = gp->dpKeyArray, ckp = (PCOMP_KEY) kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++) {
			kp->dpOutput = (PWSTR) (base + ckp->dpOutput);
			kp->dpContext = (PWSTR) (base + ckp->dpContext);
		}
	}

  return kbp;
}

BOOL VerifyChecksum(LPBYTE buf, DWORD sz) {
	DWORD tempcs;
  PCOMP_KEYBOARD ckbp;

  ckbp = (PCOMP_KEYBOARD) buf;

	tempcs = ckbp->dwCheckSum;
  ckbp->dwCheckSum = 0;

	return tempcs == CalculateBufferCRC(sz, buf);
}

BOOL VerifyKeyboard(LPBYTE filebase, DWORD sz) {
  DWORD i;
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) filebase;
  PCOMP_STORE csp;

	/* Check file version */ 

	if(ckbp->dwFileVersion < VERSION_MIN || 
	   ckbp->dwFileVersion > VERSION_MAX) { 
		/* Old or new version -- identify the desired program version */ 
		if(VerifyChecksum(filebase, sz)) { 
			for(csp = (PCOMP_STORE)(filebase + ckbp->dpStoreArray), i = 0; i < ckbp->cxStoreArray; i++, csp++) {
				if(csp->dwSystemID == TSS_COMPILEDVERSION) {
					wchar_t buf2[256];
          if(csp->dpString == 0) {
  					wsprintf(buf2, L"errWrongFileVersion:NULL");
          } else {
					  wsprintf(buf2, L"errWrongFileVersion:%10.10ls", StringOffset(filebase, csp->dpString));
          }
					Err(buf2);
					return FALSE;
				}
      }
		}
		Err(L"errWrongFileVersion");
		return FALSE; 
	}
	
	if(!VerifyChecksum(filebase, sz)) { 
    Err(L"errBadChecksum"); 
    return FALSE; 
  }

  return TRUE;
}
