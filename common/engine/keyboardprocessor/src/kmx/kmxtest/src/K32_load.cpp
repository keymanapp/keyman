#include "pch.h"

BOOL VerifyKeyboard(LPBYTE filebase, DWORD sz);
BOOL LoadKeyboard(LPSTR fileName, LPKEYBOARD *lpKeyboard);

#ifdef _WIN64
LPKEYBOARD CopyKeyboard(PBYTE bufp, PBYTE base, DWORD dwFileSize);
#else
LPKEYBOARD FixupKeyboard(PBYTE bufp, PBYTE base, DWORD dwFileSize);
#endif

BOOL LoadlpKeyboard(PSTR KeyboardName)
{
	if(!LoadKeyboard(KeyboardName, &g_keyboard.Keyboard)) return FALSE;   // I5136

  LoadKeyboardOptions(&g_keyboard);
	
	return TRUE;
}

/*
 * Instead of performing a straightforward calculation of the 32 bit
 * CRC using a series of logical operations, this program uses the
 * faster table lookup method.  This routine is called once when the
 * program starts up to build the table which will be used later
 * when calculating the CRC values.
 */

#define CRC32_POLYNOMIAL     0xEDB88320L

unsigned long CRCTable[256];

void BuildCRCTable(void)
{
	static BOOL TableBuilt = FALSE;
    int i;
    int j;
    unsigned long crc;

	if(!TableBuilt)
	{
		for(i = 0; i <= 255; i++)
		{
			crc = i;
			
			for(j = 8; j > 0; j--)
				if(crc & 1) crc = (crc >> 1) ^ CRC32_POLYNOMIAL; else crc >>= 1;
        
			CRCTable[i] = crc;
		}
	}
}


/*
 * This routine calculates the CRC for a block of data using the
 * table lookup method. It accepts an original value for the crc,
 * and returns the updated value.
 */

unsigned long CalculateBufferCRC(unsigned long count, BYTE *p)
{
    unsigned long temp1;
    unsigned long temp2;
	unsigned long crc = 0xFFFFFFFFL;

	BuildCRCTable();

	while (count-- != 0)
	{
        temp1 = ( crc >> 8 ) & 0x00FFFFFFL;
        temp2 = CRCTable[((int) crc ^ *p++) & 0xff];
        crc = temp1 ^ temp2;
    }
    
	return crc;
}

BOOL LoadKeyboard(LPSTR fileName, LPKEYBOARD *lpKeyboard)
{
	DWORD sz;
	LPBYTE buf;
	HANDLE hFile;
	LPKEYBOARD kbp;
  PBYTE filebase;

	if(!fileName || !lpKeyboard)
	{
		DebugLog("Bad Filename");
		return FALSE;
	}

	hFile = CreateFile(fileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
	if(hFile == INVALID_HANDLE_VALUE)
	{
		DebugLog("Could not open file");
		return FALSE;
	}

	sz = GetFileSize(hFile, NULL);

#ifdef _WIN64
	buf = new BYTE[sz*3];
#else
	buf = new BYTE[sz];
#endif

	if(!buf)
	{
		CloseHandle(hFile);
		DebugLog("Not allocmem");
		return FALSE;
	}
#ifdef _WIN64
	filebase = buf + sz*2;
#else
	filebase = buf;
#endif

	ReadFile(hFile, filebase, sz, &sz, NULL);
	CloseHandle(hFile);

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

	if(*LPDWORD(filebase) != FILEID_COMPILED)
	{
		delete buf; 
    DebugLog("Invalid file");
    return FALSE;
	}

	if(!VerifyKeyboard(filebase, sz)) return FALSE;

#ifdef _WIN64
	kbp = CopyKeyboard(buf, filebase, sz);
#else
	kbp = FixupKeyboard(buf, filebase, sz);
#endif

  if(!kbp) return FALSE;

	if(kbp->dwIdentifier != FILEID_COMPILED) { delete buf; DebugLog("errNotFileID"); return FALSE; }

	*lpKeyboard = kbp;

	return TRUE;
}

PWCHAR StringOffset(PBYTE base, DWORD offset)
{
  if(offset == 0) return NULL;
  return (PWCHAR)(base + offset);
}

#ifdef _WIN64

/**
  CopyKeyboard will copy the data read into bufp from x86-sized structures into x64-sized structures starting at base
  * We know the base is dwFileSize * 3
  * After this function finishes, we still need to keep the original data
*/
LPKEYBOARD CopyKeyboard(PBYTE bufp, PBYTE base, DWORD dwFileSize)
{
  UNREFERENCED_PARAMETER(dwFileSize);

  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) base;

  /* Copy keyboard structure */

  LPKEYBOARD kbp = (LPKEYBOARD) bufp;
  bufp += sizeof(KEYBOARD);

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
  kbp->hBitmap = 0; // will be built later

  kbp->dpStoreArray = (LPSTORE) bufp;
  bufp += sizeof(STORE) * kbp->cxStoreArray;

  kbp->dpGroupArray = (LPGROUP) bufp;
  bufp += sizeof(GROUP) * kbp->cxGroupArray;
  
  PCOMP_STORE csp;
  LPSTORE sp;
  DWORD i;

  for(
    csp = (PCOMP_STORE)(base + ckbp->dpStoreArray), sp = kbp->dpStoreArray, i = 0; 
    i < kbp->cxStoreArray; 
    i++, sp++, csp++)
  {
    sp->dwSystemID = csp->dwSystemID;
    sp->dpName = StringOffset(base, csp->dpName);
    sp->dpString = StringOffset(base, csp->dpString);
  }

  PCOMP_GROUP cgp;
  LPGROUP gp;

  for(
    cgp = (PCOMP_GROUP)(base + ckbp->dpGroupArray), gp = kbp->dpGroupArray, i = 0;
    i < kbp->cxGroupArray;
    i++, gp++, cgp++)
  {
    gp->dpName = (PWCHAR)(base + cgp->dpName);
    gp->dpKeyArray = (LPKEY) bufp;
    gp->cxKeyArray = cgp->cxKeyArray;
    bufp += sizeof(KEY) * gp->cxKeyArray;
    gp->dpMatch = StringOffset(base, cgp->dpMatch);
    gp->dpNoMatch = StringOffset(base, cgp->dpNoMatch);
    gp->fUsingKeys = cgp->fUsingKeys;

    PCOMP_KEY ckp;
    LPKEY kp;
    DWORD j;

    for(
      ckp = (PCOMP_KEY)(base + cgp->dpKeyArray), kp = gp->dpKeyArray, j = 0;
      j < gp->cxKeyArray;
      j++, kp++, ckp++)
    {
      kp->Key = ckp->Key;
      kp->Line = ckp->Line;
      kp->ShiftFlags = ckp->ShiftFlags;
      kp->dpOutput = StringOffset(base, ckp->dpOutput);
      kp->dpContext = StringOffset(base, ckp->dpContext);
    }
  }

  return kbp;
}

#else

LPKEYBOARD FixupKeyboard(PBYTE bufp, PBYTE base, DWORD dwFileSize)
{
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

	kbp->hBitmap = NULL;

	for(sp = kbp->dpStoreArray, csp = (PCOMP_STORE) sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)
	{
    sp->dpName = StringOffset(base, csp->dpName);
		sp->dpString = StringOffset(base, csp->dpString);
	}

	for(gp = kbp->dpGroupArray, cgp = (PCOMP_GROUP) gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)
	{
    gp->dpName = StringOffset(base, cgp->dpName); 
		gp->dpKeyArray = (LPKEY) (base + cgp->dpKeyArray);
		if(cgp->dpMatch != NULL) gp->dpMatch = (PWSTR) (base + cgp->dpMatch);
		if(cgp->dpNoMatch != NULL) gp->dpNoMatch = (PWSTR) (base + cgp->dpNoMatch);

		for(kp = gp->dpKeyArray, ckp = (PCOMP_KEY) kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++)
		{
			kp->dpOutput = (PWSTR) (base + ckp->dpOutput);
			kp->dpContext = (PWSTR) (base + ckp->dpContext);
		}
	}

  return kbp;
}

#endif

BOOL VerifyChecksum(LPBYTE buf, DWORD sz)
{
	DWORD tempcs;
  PCOMP_KEYBOARD ckbp;

  ckbp = (PCOMP_KEYBOARD) buf;

	tempcs = ckbp->dwCheckSum;
  ckbp->dwCheckSum = 0;

	return tempcs == CalculateBufferCRC(sz, buf);
}

BOOL VerifyKeyboard(LPBYTE filebase, DWORD sz)
{
  DWORD i;
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) filebase;
  PCOMP_STORE csp;

	/* Check file version */ 

	if(ckbp->dwFileVersion < VERSION_MIN || 
	   ckbp->dwFileVersion > VERSION_MAX) 
	{ 
		/* Old or new version -- identify the desired program version */ 
		if(VerifyChecksum(filebase, sz)) 
		{ 
			for(csp = (PCOMP_STORE)(filebase + ckbp->dpStoreArray), i = 0; i < ckbp->cxStoreArray; i++, csp++)
				if(csp->dwSystemID == TSS_COMPILEDVERSION)
				{
          if(csp->dpString == 0)
            DebugLog("errWrongFileVersion:NULL");
          else
					  DebugLog("errWrongFileVersion:%10.10ls", StringOffset(filebase, csp->dpString));
					return FALSE;
				}
		}
		DebugLog("errWrongFileVersion");
		return FALSE; 
	}
	
	if(!VerifyChecksum(filebase, sz)) { DebugLog("errBadChecksum"); return FALSE; }

  return TRUE;
}
