#include "mc_kmxfile.h"
#include <typeinfo>

#define CERR_None                                          0x00000000
#define CERR_CannotAllocateMemory                          0x00008004
#define CERR_UnableToWriteFully                            0x00008007
#define CERR_SomewhereIGotItWrong                          0x00008009

const int CODE__SIZE[] = {
   -1,   // undefined                0x00
    1,   // CODE_ANY                 0x01
    2,   // CODE_INDEX               0x02
    0,   // CODE_CONTEXT             0x03
    0,   // CODE_NUL                 0x04
    1,   // CODE_USE                 0x05
    0,   // CODE_RETURN              0x06
    0,   // CODE_BEEP                0x07
    1,   // CODE_DEADKEY             0x08
   -1,  // unused                   0x09
    2,   // CODE_EXTENDED            0x0A
   -1,  // CODE_EXTENDEDEND         0x0B (unused)
    1,   // CODE_SWITCH              0x0C
   -1,  // CODE_KEY                 0x0D (never used)
    0,   // CODE_CLEARCONTEXT        0x0E
    1,   // CODE_CALL                0x0F
   -1,  // UC_SENTINEL_EXTENDEDEND  0x10 (not valid with UC_SENTINEL)
    1,   // CODE_CONTEXTEX           0x11
    1,   // CODE_NOTANY              0x12
    2,   // CODE_SETOPT              0x13
    3,   // CODE_IFOPT               0x14
    1,   // CODE_SAVEOPT             0x15
    1,   // CODE_RESETOPT            0x16
    3,   // CODE_IFSYSTEMSTORE       0x17
    2    // CODE_SETSYSTEMSTORE      0x18
};

KMX_BOOL KMX_VerifyKeyboard(LPKMX_BYTE filebase, KMX_DWORD sz);

LPKMX_KEYBOARD KMX_FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base, KMX_DWORD dwFileSize);

KMX_BOOL KMX_SaveKeyboard(LPKMX_KEYBOARD kbd, PKMX_WCHAR filename) {

  FILE *fp;
  fp = Open_File(filename, u"wb");

  if(fp == NULL)
  {
    KMX_LogError(L"Failed to create output file (%d)", errno);
    return FALSE;
  }

  KMX_DWORD err = KMX_WriteCompiledKeyboardToFile(kbd, fp, FALSE);
  fclose(fp);

  if(err != CERR_None) {
    KMX_LogError(L"Failed to write compiled keyboard with error %d", err);

    std::u16string u16_filname(filename);
    std::string s = string_from_u16string(u16_filname);

    remove(s.c_str());
    return FALSE;
  }

  return TRUE;
}

KMX_DWORD KMX_WriteCompiledKeyboardToFile(LPKMX_KEYBOARD fk, FILE* hOutfile, KMX_BOOL FSaveDebug) {

	LPKMX_GROUP fgp;
	LPKMX_STORE fsp;
	LPKMX_KEY fkp;

	PCOMP_KEYBOARD ck;
	PCOMP_GROUP gp;
	PCOMP_STORE sp;
	PCOMP_KEY kp;
	PKMX_BYTE buf;
	KMX_DWORD size, offset;
	DWORD i, j;

	// Calculate how much memory to allocate
	size = sizeof(COMP_KEYBOARD) +
			fk->cxGroupArray * sizeof(COMP_GROUP) +
			fk->cxStoreArray * sizeof(COMP_STORE) +
      //wcslen(fk->szName)*2 + 2 +
			//wcslen(fk->szCopyright)*2 + 2 +
			//wcslen(fk->szLanguageName)*2 + 2 +
			//wcslen(fk->szMessage)*2 + 2 +
      fk->dwBitmapSize;

	for(i = 0, fgp = fk->dpGroupArray; i < fk->cxGroupArray; i++, fgp++) {
    if(fgp->dpName)
	    size += u16len(fgp->dpName)*2 + 2;
		size += fgp->cxKeyArray * sizeof(COMP_KEY);
		for(j = 0, fkp = fgp->dpKeyArray; j < fgp->cxKeyArray; j++, fkp++) {
			size += u16len(fkp->dpOutput)*2 + 2;
			size += u16len(fkp->dpContext)*2 + 2;
		}

		if (fgp->dpMatch ) size += u16len(fgp->dpMatch)*2 + 2;
		if (fgp->dpNoMatch ) size += u16len(fgp->dpNoMatch)*2 + 2;
	}

	for(i = 0; i < fk->cxStoreArray; i++)
	{
		size += u16len(fk->dpStoreArray[i].dpString)*2 + 2;
    if(fk->dpStoreArray[i].dpName)
      size += u16len(fk->dpStoreArray[i].dpName)*2 + 2;
	}

	buf = new KMX_BYTE[size];
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

	ck->dpStoreArray = offset;
	sp = (PCOMP_STORE)(buf+offset);
	fsp = fk->dpStoreArray;
	offset += sizeof(COMP_STORE) * ck->cxStoreArray;
	for(i = 0; i < ck->cxStoreArray; i++, sp++, fsp++) {
		sp->dwSystemID = fsp->dwSystemID;
		sp->dpString = offset;
		u16ncpy((PKMX_WCHAR)(buf+offset), fsp->dpString, (size-offset) / sizeof(KMX_WCHAR));  // I3481   // I3641
		offset += u16len(fsp->dpString)*2 + 2;

    if(!fsp->dpName) {
      sp->dpName = 0;
    } else {
      sp->dpName = offset;
      u16ncpy((PKMX_WCHAR)(buf+offset), fsp->dpName, (size-offset) / sizeof(KMX_WCHAR));  // I3481   // I3641
		  offset += u16len(fsp->dpName)*2 + 2;
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
			u16ncpy((PKMX_WCHAR)(buf+offset), fgp->dpMatch, (size-offset) / sizeof(KMX_WCHAR));  // I3481   // I3641
			offset += u16len(fgp->dpMatch)*2 + 2;
		}
		if(fgp->dpNoMatch) {
			gp->dpNoMatch = offset;
			u16ncpy((PKMX_WCHAR)(buf+offset), fgp->dpNoMatch, (size-offset) / sizeof(KMX_WCHAR));  // I3481   // I3641
			offset += u16len(fgp->dpNoMatch)*2 + 2;
		}

    if(fgp->dpName) {
			gp->dpName = offset;
			u16ncpy((PKMX_WCHAR)(buf+offset), fgp->dpName, (size-offset) / sizeof(KMX_WCHAR));  // I3481   // I3641
			offset += u16len(fgp->dpName)*2 + 2;
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

			u16ncpy((PKMX_WCHAR)(buf+offset), fkp->dpOutput, (size-offset) / sizeof(KMX_WCHAR));  // I3481   // I3641
			offset += u16len(fkp->dpOutput)*2 + 2;

      kp->dpContext = offset;
      u16ncpy((PKMX_WCHAR)(buf+offset), fkp->dpContext, (size-offset) / sizeof(KMX_WCHAR));  // I3481   // I3641
		  offset += u16len(fkp->dpContext)*2 + 2;
		}
	}

  if(fk->dwBitmapSize > 0) {
    ck->dwBitmapSize = fk->dwBitmapSize;
	  ck->dpBitmapOffset = offset;
    memcpy(buf + offset, ((PKMX_BYTE)fk) + fk->dpBitmapOffset, fk->dwBitmapSize);
	  offset += fk->dwBitmapSize;
  } else {
    ck->dwBitmapSize = 0;
	  ck->dpBitmapOffset = 0;
  }

  fwrite(buf, size,1,hOutfile);
	if(offset != size)
  {
    delete[] buf;
    return CERR_UnableToWriteFully;
  }

	delete[] buf;

	return CERR_None;
}

PKMX_WCHAR KMX_StringOffset(PKMX_BYTE base, KMX_DWORD offset) {
  if(offset == 0) return NULL;
  return (PKMX_WCHAR)(base + offset);
}

#ifdef KMX_64BIT

/**  CopyKeyboard will copy the data read into bufp from x86-sized structures into
  x64-sized structures starting at `base`
  * After this function finishes, we still need to keep the original data because
    we don't copy the strings
  This method is used on 64-bit architectures.
*/
LPKMX_KEYBOARD CopyKeyboard(PKMX_BYTE bufp, PKMX_BYTE base) {
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) base;

  /* Copy keyboard structure */

  LPKMX_KEYBOARD kbp = (LPKMX_KEYBOARD) bufp;
  bufp += sizeof(KMX_KEYBOARD);

  kbp->dwIdentifier = ckbp->dwIdentifier;
  kbp->dwFileVersion = ckbp->dwFileVersion;
  kbp->dwCheckSum = ckbp->dwCheckSum;
  kbp->xxkbdlayout = ckbp->KeyboardID;
  kbp->IsRegistered = ckbp->IsRegistered;
  kbp->version = ckbp->version;
  kbp->cxStoreArray = ckbp->cxStoreArray;
  kbp->cxGroupArray = ckbp->cxGroupArray;
  kbp->StartGroup[0] = ckbp->StartGroup[0];
  kbp->StartGroup[1] = ckbp->StartGroup[1];
  kbp->dwFlags = ckbp->dwFlags;
  kbp->dwHotKey = ckbp->dwHotKey;

  kbp->dpStoreArray = (LPKMX_STORE) bufp;
  bufp += sizeof(KMX_STORE) * kbp->cxStoreArray;

  kbp->dpGroupArray = (LPKMX_GROUP) bufp;
  bufp += sizeof(KMX_GROUP) * kbp->cxGroupArray;

  PCOMP_STORE csp;
  LPKMX_STORE sp;
  KMX_DWORD i;

  for(
    csp = (PCOMP_STORE)(base + ckbp->dpStoreArray), sp = kbp->dpStoreArray, i = 0;
    i < kbp->cxStoreArray;
    i++, sp++, csp++)
  {
    sp->dwSystemID = csp->dwSystemID;
    sp->dpName = KMX_StringOffset(base, csp->dpName);
    sp->dpString = KMX_StringOffset(base, csp->dpString);
  }

  PCOMP_GROUP cgp;
  LPKMX_GROUP gp;

  for(
    cgp = (PCOMP_GROUP)(base + ckbp->dpGroupArray), gp = kbp->dpGroupArray, i = 0;
    i < kbp->cxGroupArray;
    i++, gp++, cgp++)
  {
    gp->dpName = KMX_StringOffset(base, cgp->dpName);
    gp->dpKeyArray = cgp->cxKeyArray > 0 ? (LPKMX_KEY) bufp : NULL;
    gp->cxKeyArray = cgp->cxKeyArray;
    bufp += sizeof(KMX_KEY) * gp->cxKeyArray;
    gp->dpMatch = KMX_StringOffset(base, cgp->dpMatch);
    gp->dpNoMatch = KMX_StringOffset(base, cgp->dpNoMatch);
    gp->fUsingKeys = cgp->fUsingKeys;

    PCOMP_KEY ckp;
    LPKMX_KEY kp;
    KMX_DWORD j;

    for(
      ckp = (PCOMP_KEY)(base + cgp->dpKeyArray), kp = gp->dpKeyArray, j = 0;
      j < gp->cxKeyArray;
      j++, kp++, ckp++)
    {
      kp->Key = ckp->Key;
      kp->Line = ckp->Line;
      kp->ShiftFlags = ckp->ShiftFlags;
      kp->dpOutput = KMX_StringOffset(base, ckp->dpOutput);
      kp->dpContext = KMX_StringOffset(base, ckp->dpContext);
    }
  }
  return kbp;
}

// else KMX_FixupKeyboard
#else  /**  Fixup the keyboard by expanding pointers. On disk the pointers are stored relative to the
 beginning of the file, but we need real pointers. This method is used on 32-bit architectures.
*/

LPKMX_KEYBOARD KMX_FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base, KMX_DWORD dwFileSize) {

  UNREFERENCED_PARAMETER(dwFileSize);

  KMX_DWORD i, j;
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) base;
  PCOMP_GROUP cgp;
  PCOMP_STORE csp;
  PCOMP_KEY ckp;
  LPKMX_KEYBOARD kbp = (LPKMX_KEYBOARD) bufp;
  LPKMX_STORE sp;
  LPKMX_GROUP gp;
  LPKMX_KEY kp;

	kbp->dpStoreArray = (LPKMX_STORE) (base + ckbp->dpStoreArray);
	kbp->dpGroupArray = (LPKMX_GROUP) (base + ckbp->dpGroupArray);

	for(sp = kbp->dpStoreArray, csp = (PCOMP_STORE) sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)	{
    sp->dpName = KMX_StringOffset(base, csp->dpName);
		sp->dpString = KMX_StringOffset(base, csp->dpString);
	}

	for(gp = kbp->dpGroupArray, cgp = (PCOMP_GROUP) gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)	{
    gp->dpName = KMX_StringOffset(base, cgp->dpName);
		gp->dpKeyArray = (LPKMX_KEY) (base + cgp->dpKeyArray);
		if(cgp->dpMatch != NULL) gp->dpMatch = (PKMX_WCHAR) (base + cgp->dpMatch);
		if(cgp->dpNoMatch != NULL) gp->dpNoMatch = (PKMX_WCHAR) (base + cgp->dpNoMatch);

    for(kp = gp->dpKeyArray, ckp = (PCOMP_KEY) kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++) {
			kp->dpOutput = (PKMX_WCHAR) (base + ckp->dpOutput);
			kp->dpContext = (PKMX_WCHAR) (base + ckp->dpContext);
		}
	}

  return kbp;
}

#endif

KMX_BOOL KMX_LoadKeyboard(char16_t* fileName, LPKMX_KEYBOARD* lpKeyboard) {

  PKMX_BYTE buf;
  FILE* fp;
  LPKMX_KEYBOARD kbp;
  PKMX_BYTE filebase;

  if(!fileName || !lpKeyboard) {
    KMX_LogError(L"Bad Filename\n" );
    return FALSE;
  }

  fp = Open_File((const KMX_WCHAR*)fileName, u"rb");

  if(fp == NULL) {
    KMX_LogError(L"Could not open file\n" );
    return FALSE;
  }

  if (fseek(fp, 0, SEEK_END) != 0) {
    fclose(fp);
    KMX_LogError(L"Could not fseek file\n" );
    return FALSE;
  }

  auto sz = ftell(fp);
  if (sz < 0) {
    fclose(fp);
    return FALSE;
  }

  if (fseek(fp, 0, SEEK_SET) != 0) {
    fclose(fp);
    KMX_LogError(L"Could not fseek(set) file\n" );
    return FALSE;
  }

  #ifdef KMX_64BIT
  //  allocate enough memory for expanded data structure + original data.
  //  Expanded data structure is double the size of data on disk (8-byte
  //  pointers) - on disk the "pointers" are relative to the beginning of
  //  the file.
  //  We save the original data at the end of buf; we don't copy strings, so
  //  those will remain in the location at the end of the buffer.
    buf = new KMX_BYTE[sz * 3];
  #else
    buf = new KMX_BYTE[sz];
  #endif

  if (!buf) {
    fclose(fp);
    KMX_LogError(L"Not allocmem\n" );
    return FALSE;
  }

  #ifdef KMX_64BIT
    filebase = buf + sz*2;
  #else
    filebase = buf;
  #endif

  if (fread(filebase, 1, sz, fp) < (size_t)sz) {
    KMX_LogError(L"Could not read file\n" );
    fclose(fp);
    return FALSE;
  }

  fclose(fp);

  if(*PKMX_DWORD(filebase) != KMX_DWORD(FILEID_COMPILED))
  {
    delete [] buf;
    KMX_LogError(L"Invalid file - signature is invalid\n");
    return FALSE;
  }

  if (!KMX_VerifyKeyboard(filebase, sz)) {
    KMX_LogError(L"errVerifyKeyboard\n" );
    return FALSE;
  }

#ifdef KMX_64BIT
  kbp = CopyKeyboard(buf, filebase);
#else
  kbp = KMX_FixupKeyboard(buf, filebase, sz);
#endif


  if (!kbp) {
    KMX_LogError(L"errFixupKeyboard\n" );
    return FALSE;
  }

  if (kbp->dwIdentifier != FILEID_COMPILED) {
    delete[] buf;
    KMX_LogError(L"errNotFileID\n" );
    return FALSE;
  }
  *lpKeyboard = kbp;
  return TRUE;
}

KMX_BOOL KMX_VerifyKeyboard(LPKMX_BYTE filebase, KMX_DWORD sz){
  KMX_DWORD i;
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD)filebase;
  PCOMP_STORE csp;

  // Check file version //

  if (ckbp->dwFileVersion < VERSION_MIN || ckbp->dwFileVersion > VERSION_MAX) {
    // Old or new version -- identify the desired program version //
    for (csp = (PCOMP_STORE)(filebase + ckbp->dpStoreArray), i = 0; i < ckbp->cxStoreArray; i++, csp++) {
      if (csp->dwSystemID == TSS_COMPILEDVERSION) {
        if (csp->dpString == 0) {
          KMX_LogError(L"errWrongFileVersion:NULL");
        } else {
          KMX_LogError(L"errWrongFileVersion:%10.10ls",(const PKMX_WCHAR) KMX_StringOffset((PKMX_BYTE)filebase, csp->dpString));
        }
        return FALSE;
      }
    }
    KMX_LogError(L"errWrongFileVersion");
    return FALSE;
  }
  return TRUE;
}

PKMX_WCHAR KMX_incxstr(PKMX_WCHAR p) {

  if (*p == 0)
    return p;
  if (*p != UC_SENTINEL) {
    if (*p >= 0xD800 && *p <= 0xDBFF && *(p + 1) >= 0xDC00 && *(p + 1) <= 0xDFFF)
      return p + 2;
    return p + 1;
  }
  // UC_SENTINEL(FFFF) with UC_SENTINEL_EXTENDEDEND(0x10) == variable length
  if (*(p + 1) == CODE_EXTENDED) {
    p += 2;
    while (*p && *p != UC_SENTINEL_EXTENDEDEND)
      p++;

    if (*p == 0)        return p;
    return p + 1;
  }

  if (*(p + 1) > CODE_LASTCODE || CODE__SIZE[*(p + 1)] == -1) {
    return p + 1;
  }

  int deltaptr = 2 + CODE__SIZE[*(p + 1)];

  // check for \0 between UC_SENTINEL(FFFF) and next printable character
  for (int i = 0; i < deltaptr; i++) {
    if (*p == 0)
      return p;
    p++;
  }
  return p;
}

