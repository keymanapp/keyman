
#include "mc_kmxfile.h"
#include "helpers.h"
#include "u16.h"
#include "filesystem.h"     // _S2 needed?
#include <typeinfo>

int dummytest_mc_kmx_file(){
  std::cout<< " dummytest_mc_kmx_file    is available\t";
  return 0;
  }

//KMX_BOOL VerifyKeyboard(LPKMX_BYTE filebase, KMX_DWORD sz);
KMX_BOOL KMX_VerifyKeyboard(LPKMX_BYTE filebase, KMX_DWORD sz);
//BOOL VerifyKeyboard(LPBYTE filebase, DWORD sz);

//LPKEYBOARD FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base, KMX_DWORD dwFileSize);

LPKMX_KEYBOARD KMX_FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base, KMX_DWORD dwFileSize);

/*void Err(wchar_t *s) {
	LogError(L"LoadKeyboard: %s, last error = %d\n", s, GetLastError());
}*/

/*PWSTR StringOffset(PBYTE base, DWORD offset) {
  if(offset == 0) return NULL;
  return (PWSTR)(base + offset);
}*/

/*PWCHAR StringOffset(PBYTE base, DWORD offset) {
  if(offset == 0) return NULL;
  return (PWSTR)(base + offset);
}
*/

/*PKMX_WCHART KMX_StringOffset(PKMX_BYTE base, KMX_DWORD offset) {
  if(offset == 0) return NULL;
  return (PKMX_WCHART)(base + offset);
}*/

PKMX_WCHAR KMX_StringOffset(PKMX_BYTE base, KMX_DWORD offset) {
  if(offset == 0) return NULL;
  return (PKMX_WCHAR)(base + offset);
}

/*LPKEYBOARD FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base, KMX_DWORD dwFileSize) {

  MyCoutW(L"   ##### FixupKeyboard started", 1);
  UNREFERENCED_PARAMETER(dwFileSize);

  KMX_DWORD i, j;
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

  MyCoutW(L"   ##### first assignment finished", 1);
	for(sp = kbp->dpStoreArray, csp = (PCOMP_STORE) sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)	{
    sp->dpName = StringOffset(base, csp->dpName);
		sp->dpString = StringOffset(base, csp->dpString);
	}

  MyCoutW(L"   ##### sp filled", 1);
	for(gp = kbp->dpGroupArray, cgp = (PCOMP_GROUP) gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)	{
    gp->dpName = StringOffset(base, cgp->dpName);
		gp->dpKeyArray = (LPKEY) (base + cgp->dpKeyArray);
		if(cgp->dpMatch != NULL) gp->dpMatch = (PKMX_WCHART) (base + cgp->dpMatch);       // _S2 Warning about NULL !!
		if(cgp->dpNoMatch != NULL) gp->dpNoMatch = (PKMX_WCHART) (base + cgp->dpNoMatch); // _S2 Warning about NULL !!

  MyCoutW(L"   ##### gp, cgp filled", 1);
		for(kp = gp->dpKeyArray, ckp = (PCOMP_KEY) kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++) {
			kp->dpOutput = (PKMX_WCHART) (base + ckp->dpOutput);
			kp->dpContext = (PKMX_WCHART) (base + ckp->dpContext);
		}
	}

  MyCoutW(L"   ##### kp filled", 1);
  MyCoutW(L"   ##### FixupKeyboard ended", 1);
  return kbp;
}*/


#ifdef KMX_64BIT
/**
  CopyKeyboard will copy the data read into bufp from x86-sized structures into
  x64-sized structures starting at `base`
  * After this function finishes, we still need to keep the original data because
    we don't copy the strings
  This method is used on 64-bit architectures.
*/
LPKMX_KEYBOARD KMX_CopyKeyboard(PKMX_BYTE bufp, PKMX_BYTE base)
{
  PKMX_COMP_KEYBOARD ckbp = (PKMX_COMP_KEYBOARD) base;

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

  PKMX_COMP_STORE csp;
  LPKMX_STORE sp;
  KMX_DWORD i;

  for(
    csp = (PKMX_COMP_STORE)(base + ckbp->dpStoreArray), sp = kbp->dpStoreArray, i = 0;
    i < kbp->cxStoreArray;
    i++, sp++, csp++)
  {
    sp->dwSystemID = csp->dwSystemID;
    sp->dpName = KMX_StringOffset(base, csp->dpName);
    sp->dpString = KMX_StringOffset(base, csp->dpString);
  }

  PKMX_COMP_GROUP cgp;
  LPKMX_GROUP gp;

  for(
    cgp = (PKMX_COMP_GROUP)(base + ckbp->dpGroupArray), gp = kbp->dpGroupArray, i = 0;
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

    PKMX_COMP_KEY ckp;
    LPKMX_KEY kp;
    KMX_DWORD j;

    for(
      ckp = (PKMX_COMP_KEY)(base + cgp->dpKeyArray), kp = gp->dpKeyArray, j = 0;
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
#else
/**
 Fixup the keyboard by expanding pointers. On disk the pointers are stored relative to the
 beginning of the file, but we need real pointers. This method is used on 32-bit architectures.
*/

LPKMX_KEYBOARD KMX_FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base, KMX_DWORD dwFileSize) {
  MyCoutW(L"  ##### KMX_FixupKeyboard of mcompile started",1);
  UNREFERENCED_PARAMETER(dwFileSize);

  KMX_DWORD i, j;
  PKMX_COMP_KEYBOARD ckbp = (PKMX_COMP_KEYBOARD) base;
  PKMX_COMP_GROUP cgp;
  PKMX_COMP_STORE csp;
  PKMX_COMP_KEY ckp;
  LPKMX_KEYBOARD kbp = (LPKMX_KEYBOARD) bufp;
  LPKMX_STORE sp;
  LPKMX_GROUP gp;
  LPKMX_KEY kp;

	kbp->dpStoreArray = (LPKMX_STORE) (base + ckbp->dpStoreArray);
	kbp->dpGroupArray = (LPKMX_GROUP) (base + ckbp->dpGroupArray);

	for(sp = kbp->dpStoreArray, csp = (PKMX_COMP_STORE) sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)	{
    sp->dpName = KMX_StringOffset(base, csp->dpName);
		sp->dpString = KMX_StringOffset(base, csp->dpString);
	}

	for(gp = kbp->dpGroupArray, cgp = (PKMX_COMP_GROUP) gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)	{
    gp->dpName = KMX_StringOffset(base, cgp->dpName);
		gp->dpKeyArray = (LPKMX_KEY) (base + cgp->dpKeyArray);
		if(cgp->dpMatch != NULL) gp->dpMatch = (PKMX_WCHAR) (base + cgp->dpMatch);
		if(cgp->dpNoMatch != NULL) gp->dpNoMatch = (PKMX_WCHAR) (base + cgp->dpNoMatch);

    // _S2  Version of kmx_file v
    /*gp->dpName = StringOffset(base, cgp->dpName);
    gp->dpKeyArray = cgp->cxKeyArray > 0 ? (LPKEY) (base + cgp->dpKeyArray) : NULL;
    gp->dpMatch = StringOffset(base, cgp->dpMatch);
    gp->dpNoMatch = StringOffset(base, cgp->dpNoMatch);*/
    // _S2  Version of kmx_file ^

		for(kp = gp->dpKeyArray, ckp = (PKMX_COMP_KEY) kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++) {
			kp->dpOutput = (PKMX_WCHAR) (base + ckp->dpOutput);
			kp->dpContext = (PKMX_WCHAR) (base + ckp->dpContext);
		}
	}

  MyCoutW(L"  ##### KMX_FixupKeyboard of mcompile ended",1);
  return kbp;
}
#endif


/*LPKMX_KEYBOARD KMX_FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base, KMX_DWORD dwFileSize) {
  MyCoutW(L"  ##### KMX_FixupKeyboard of mc_kmxfile started",1);
  UNREFERENCED_PARAMETER(dwFileSize);

  KMX_DWORD i, j;
  PKMX_COMP_KEYBOARD ckbp = (PKMX_COMP_KEYBOARD) base;
  PKMX_COMP_GROUP cgp;
  PKMX_COMP_STORE csp;
  PKMX_COMP_KEY ckp;
  LPKMX_KEYBOARD kbp = (LPKMX_KEYBOARD) bufp;
  LPKMX_STORE sp;
  LPKMX_GROUP gp;
  LPKMX_KEY kp;

	kbp->dpStoreArray = (LPKMX_STORE) (base + ckbp->dpStoreArray);
	kbp->dpGroupArray = (LPKMX_GROUP) (base + ckbp->dpGroupArray);

	for(sp = kbp->dpStoreArray, csp = (PKMX_COMP_STORE) sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)	{
    sp->dpName = KMX_StringOffset(base, csp->dpName);
		sp->dpString = KMX_StringOffset(base, csp->dpString);
	}

	for(gp = kbp->dpGroupArray, cgp = (PKMX_COMP_GROUP) gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)	{
    gp->dpName = KMX_StringOffset(base, cgp->dpName);
		gp->dpKeyArray = (LPKMX_KEY) (base + cgp->dpKeyArray);
		if(cgp->dpMatch != NULL) gp->dpMatch = (PKMX_WCHART) (base + cgp->dpMatch);
		if(cgp->dpNoMatch != NULL) gp->dpNoMatch = (PKMX_WCHART) (base + cgp->dpNoMatch);

		for(kp = gp->dpKeyArray, ckp = (PKMX_COMP_KEY) kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++) {
			kp->dpOutput = (PKMX_WCHART) (base + ckp->dpOutput);
			kp->dpContext = (PKMX_WCHART) (base + ckp->dpContext);
		}
	}
  #
  MyCoutW(L"  ##### KMX_FixupKeyboard of mc_kmxfile ended",1);
  return kbp;
}*/

/*KMX_BOOL LoadKeyboard(char* fileName, LPKEYBOARD *lpKeyboard) {
  std::cout << "##### LoadKeyboard of mcompile started #####\n";
  std::cout << "fileName: " <<fileName<< "\n";

  LPKMX_BYTE buf;
  FILE * fp;
  LPKEYBOARD kbp;
  PKMX_BYTE filebase;

  //DebugLog("Loading file '%s'",fileName); 				  //_S2 ToDo find replacement:   DebugLog

  if(!fileName || !lpKeyboard) {
	std::cout << "TODO: Replace Err(LBad Filename)\n" ;	//_S2 ToDo find replacement:   Err(L"Bad Filename");
	return FALSE;
  }

  fp = Open_File(fileName, "rb");

  if(fp == NULL)
  {
    std::cout << "Could not open file\n"; 						//_S2 ToDo find replacement: DebugLog("Could not open file");
    return FALSE;
  }
  else															                  // _S2 remove
  std::cout << "Could OPEN file"<<fp<<"\n";           // _S2 remove

	MyCout("##### Line 93",1);									        // _S2 remove

  if (fseek(fp, 0, SEEK_END) != 0) {
    fclose(fp);
    MyCout("Could not fseek file",1);							    //_S2 ToDo find replacement:	DebugLog("Could not fseek file");
    return FALSE;
  }
  else
  MyCout("CCould  fseek file",1);								      // _S2 remove

MyCout("##### Line 103",1);
  auto sz = ftell(fp);
  if (sz < 0) {
    fclose(fp);
    return FALSE;
  }

MyCout("##### Line 110",1);								            // _S2 remove
  if (fseek(fp, 0, SEEK_SET) != 0) {
    fclose(fp);
    MyCout("Could not fseek(set) file",1);					  //_S2 ToDo find replacement:	DebugLog("Could not fseek(set) file");
    return FALSE;
  }

#ifdef KMX_64BIT
  // allocate enough memory for expanded data structure + original data.
  // Expanded data structure is double the size of data on disk (8-byte
  // pointers) - on disk the "pointers" are relative to the beginning of
  // the file.
  // We save the original data at the end of buf; we don't copy strings, so
  // those will remain in the location at the end of the buffer.
  buf = new KMX_BYTE[sz * 3];
#else
  buf = new KMX_BYTE[sz];
#endif

MyCout("#### Line 129 ",1);
  if(!buf)
  {
    fclose(fp);
    MyCout("Not allocmem",1);								//_S2 ToDo find replacement: DebugLog()"Not allocmem");		
															              // _S2 delete [] buf; ????
    return FALSE;
  }

#ifdef KMX_64BIT
  filebase = buf + sz*2;
#else
  filebase = buf;
#endif

  if (fread(filebase, 1, sz, fp) < (size_t) sz) {
    MyCout("Could not read file",1);        //_S2 ToDo find replacement: DebugLog("Could not read file");
    fclose(fp);
															              // _S2 delete [] buf; ????
    return FALSE;
  }

  fclose(fp);

MyCout("##### Line 153",1);

if(!VerifyKeyboard(filebase, sz)) {
	// Err(L"errVerifyKeyboard");             //_S2 ToDo find replacement: Err
	                                          // _S2 delete [] buf; ????
  return FALSE;
  }


  MyCout("##### Line 157",1);
  kbp = FixupKeyboard(buf, filebase,sz);
  MyCout("##### Line 159",1);


std::cout << "kbp: "<<kbp<< "\n";
  if(!kbp) {
	//Err(L"errFixupKeyboard");								//_S2 ToDo find replacement: Err
															              // _S2 delete [] buf; ????

  MyCout("##### errFixupKeyboard ",1);
	return FALSE;}


  MyCout("##### Line 171 ",1);
                                            //_S2 can go:
                                             // if(kbp->dwIdentifier != FILEID_COMPILED) {
                                             // Err(L"errNotFileID");
                                             // delete[] buf;
                                             // return FALSE;
                                            //}

//std::cout << "kbp->dwIdentifier: "<<kbp->dwIdentifier<< "\n";

 if(kbp->dwIdentifier != FILEID_COMPILED) {
    delete [] buf;
    //MyCout("errNotFileID",1);								//_S2 ToDo find replacement: DebugLog("errNotFileID");
															                // _S2 delete [] buf; ????
    return FALSE;
  }
MyCout("##### Line 187",1);
	*lpKeyboard = kbp;
															                // _S2 delete [] buf; ????
	MyCout("##### LoadKeyboard of mcompile ended #####",1);
	return TRUE;
}*/

/*KMX_BOOL LoadKeyboard(char16_t* fileName, LPKEYBOARD* lpKeyboard) {
  std::wcout << "##### LoadKeyboard of mcompile started #####\n";

  LPKMX_BYTE buf;
  FILE* fp;
  LPKEYBOARD kbp;
  PKMX_BYTE filebase;

  wprintf(L"Loading file '%ls'\n", u16fmt((const char16_t*) fileName).c_str());

  if(!fileName || !lpKeyboard) {
    KMX_LogError(L"LogError1: Bad Filename\n" );
    return FALSE;
  }

  fp = Open_File((const KMX_WCHAR*)fileName, u"rb");

  if(fp == NULL) {
    KMX_LogError(L"LogError1: Could not open file\n" );
    return FALSE;
  }

  if (fseek(fp, 0, SEEK_END) != 0) {
    fclose(fp);
    KMX_LogError(L"LogError1: Could not fseek file\n" );
    return FALSE;
  }

  auto sz = ftell(fp);
  if (sz < 0) {
    fclose(fp);
    return FALSE;
  }

  if (fseek(fp, 0, SEEK_SET) != 0) {
    fclose(fp);
    KMX_LogError(L"LogErr1: Could not fseek(set) file\n" );
    return FALSE;
  }

  // #ifdef KMX_64BIT
  //  allocate enough memory for expanded data structure + original data.
  //  Expanded data structure is double the size of data on disk (8-byte
  //  pointers) - on disk the "pointers" are relative to the beginning of
  //  the file.
  //  We save the original data at the end of buf; we don't copy strings, so
  //  those will remain in the location at the end of the buffer.
  //  buf = new KMX_BYTE[sz * 3];
  // #else
  buf = new KMX_BYTE[sz];
  // #endif

    MyCoutW(L"#### Line 246 ", 1);
  if (!buf) {
    fclose(fp);
    KMX_LogError(L"LogErr1: Not allocmem\n" );
                                // _S2 delete [] buf; ????
    return FALSE;
  }

  // #ifdef KMX_64BIT
  // ilebase = buf + sz*2;
  // #else
  filebase = buf;
  // #endif

  if (fread(filebase, 1, sz, fp) < (size_t)sz) {
    KMX_LogError(L"LogError1: Could not read file\n" );
    fclose(fp);
    // _S2 delete [] buf; ????
    return FALSE;
  }

  fclose(fp);

  MyCoutW(L"##### Line 328", 1);
  ;
  KMX_DWORD sz_dw = (KMX_DWORD)sz;  //_S2
  size_t sz_t = (size_t)sz;  //_S2
  // if(!VerifyKeyboard(filebase, sz_t)) {
  if (!VerifyKeyboard(filebase, sz_t)) {
    KMX_LogError(L"LogError1: errVerifyKeyboard\n" );
    // _S2 delete [] buf; ????
    return FALSE;
  }

  MyCoutW(L"##### Line 339", 1);
  kbp = FixupKeyboard(buf, filebase, sz_dw);    // _S" changed from sz->sz_dw
  MyCoutW(L"##### Line 341", 1);

  if (!kbp) {
    KMX_LogError(L"LogError1: errFixupKeyboard\n" );
    //  _S2 delete [] buf; ????

    MyCoutW(L"##### errFixupKeyboard ", 1);
    return FALSE;
  }

  MyCoutW(L"##### Line 351 ", 1);

  std::wcout << "##### kbp->dwIdentifier: " << kbp->dwIdentifier << " FILEID_COMPILED: " << FILEID_COMPILED << "\n";

  if (kbp->dwIdentifier != FILEID_COMPILED) {
    delete[] buf;
    KMX_LogError(L"LogError1: errNotFileID\n" );
    return FALSE;
  }
  MyCoutW(L"##### Line 360", 1);
  *lpKeyboard = kbp;
  // _S2 delete [] buf; ????
  MyCoutW(L"##### LoadKeyboard of mcompile ended #####", 1);
  return TRUE;
}
*/

KMX_BOOL KMX_LoadKeyboard(char16_t* fileName, LPKMX_KEYBOARD* lpKeyboard) {
  std::wcout << "##### KMX_LoadKeyboard of mcompile started #####\n";

  PKMX_BYTE buf;
  FILE* fp;
  LPKMX_KEYBOARD kbp;
  PKMX_BYTE filebase;

  wprintf(L"Loading file '%ls'\n", u16fmt((const char16_t*) fileName).c_str());

  if(!fileName || !lpKeyboard) {
    KMX_LogError(L"LogError1: Bad Filename\n" );
    return FALSE;
  }

  fp = Open_File((const KMX_WCHAR*)fileName, u"rb");

  if(fp == NULL) {
    KMX_LogError(L"LogError1: Could not open file\n" );
    return FALSE;
  }

  if (fseek(fp, 0, SEEK_END) != 0) {
    fclose(fp);
    KMX_LogError(L"LogError1: Could not fseek file\n" );
    return FALSE;
  }

  auto sz = ftell(fp);
  if (sz < 0) {
    fclose(fp);
    return FALSE;
  }

  if (fseek(fp, 0, SEEK_SET) != 0) {
    fclose(fp);
    KMX_LogError(L"LogErr1: Could not fseek(set) file\n" );
    return FALSE;
  }

  #ifdef KMX_64BIT              // _S2 opened for copyKeyboard-Version
  //  allocate enough memory for expanded data structure + original data.
  //  Expanded data structure is double the size of data on disk (8-byte
  //  pointers) - on disk the "pointers" are relative to the beginning of
  //  the file.
  //  We save the original data at the end of buf; we don't copy strings, so
  //  those will remain in the location at the end of the buffer.
    buf = new KMX_BYTE[sz * 3]; // _S2 opened for copyKeyboard-Version
  #else // _S2 opened for copyKeyboard-Version
    buf = new KMX_BYTE[sz];
  #endif                        // _S2 opened for copyKeyboard-Version

    MyCoutW(L"#### Line 435 ", 1);
  if (!buf) {
    fclose(fp);
    KMX_LogError(L"LogErr1: Not allocmem\n" );
                                // _S2 delete [] buf; ????
    return FALSE;
  }

  #ifdef KMX_64BIT            // _S2 opened for copyKeyboard-Version
    filebase = buf + sz*2;    // _S2 opened for copyKeyboard-Version
  #else                       // _S2 opened for copyKeyboard-Version
    filebase = buf;
  #endif                      // _S2 opened for copyKeyboard-Version


  if (fread(filebase, 1, sz, fp) < (size_t)sz) {
    KMX_LogError(L"LogError1: Could not read file\n" );
    fclose(fp);
    // _S2 delete [] buf; ????
    return FALSE;
  }

  fclose(fp);

  // _S2 opened for copyKeyboard-Version v
  if(*PKMX_DWORD(filebase) != KMX_DWORD(FILEID_COMPILED))
  {
    delete [] buf;
    KMX_LogError(L"Invalid file - signature is invalid\n");
    return FALSE;
  }
  // _S2 opened for copyKeyboard-Version ^

  MyCoutW(L"##### Line 458", 1);

  if (!KMX_VerifyKeyboard(filebase, sz)) {
    KMX_LogError(L"LogError1: errVerifyKeyboard\n" );
    // _S2 delete [] buf; ????
    return FALSE;
  }

#ifdef KMX_64BIT                                          // _S2 opened for copyKeyboard-Version
  kbp = KMX_CopyKeyboard(buf, filebase);                  // _S2 opened for copyKeyboard-Version
#else                                                     // _S2 opened for copyKeyboard-Version
  MyCoutW(L"##### Line 469", 1);
  kbp = KMX_FixupKeyboard(buf, filebase, sz);       // _S2 changed from sz->sz_dw
  MyCoutW(L"##### Line 471", 1);
#endif                                                    // _S2 opened for copyKeyboard-Version


  if (!kbp) {
    KMX_LogError(L"LogError1: errFixupKeyboard\n" );
    //  _S2 delete [] buf; ????

    MyCoutW(L"##### errFixupKeyboard ", 1);
    return FALSE;
  }

  MyCoutW(L"##### Line 468 ", 1);

  std::wcout << "kbp->dwIdentifier: " << kbp->dwIdentifier << " FILEID_COMPILED: " << FILEID_COMPILED << "\n";

  if (kbp->dwIdentifier != FILEID_COMPILED) {
    delete[] buf;
    KMX_LogError(L"LogError1: errNotFileID\n" );
    return FALSE;
  }
  MyCoutW(L"##### Line 477", 1);
  *lpKeyboard = kbp;
  // _S2 delete [] buf; ????
  MyCoutW(L"##### LoadKeyboard of mcompile ended #####", 1);
  return TRUE;
}


// _S2 Version for char16_t filename
/*KMX_BOOL LoadKeyboard(char16_t* fileName, LPKEYBOARD* lpKeyboard) {
  std::cout << "##### LoadKeyboard of mcompile started #####\n";
  std::cout << "fileName: " <<fileName << "\n";

  LPKMX_BYTE buf;
  FILE* fp;
  LPKEYBOARD kbp;
  PKMX_BYTE filebase;

  //DebugLog("Loading file '%s'",fileName); 				  //_S2 ToDo find replacement:   DebugLog

  if(!fileName || !lpKeyboard) {
       std::cout << "TODO: Replace Err(LBad Filename)\n";//_S2 ToDo find replacement:   Err(L"Bad Filename");
       return FALSE;
  }

  fp = Open_File((const KMX_WCHAR*)fileName, u"rb");

  if(fp == NULL) {
    std::cout << "Could not open file\n";          //_S2 ToDo find replacement: DebugLog("Could not open file");
    return FALSE;
  } else                                           // _S2 remove
    std::cout << "Could OPEN file" << fp << "\n";  // _S2 remove

   MyCout("##### Line 224", 1);                     // _S2 remove

  if (fseek(fp, 0, SEEK_END) != 0) {
    fclose(fp);
    MyCout("Could not fseek file", 1);  //_S2 ToDo find replacement:	DebugLog("Could not fseek file");
    return FALSE;
  } else
    MyCout("CCould  fseek file", 1);  // _S2 remove

    MyCout("##### Line 234", 1);
  auto sz = ftell(fp);
  if (sz < 0) {
    fclose(fp);
    return FALSE;
  }

    MyCout("##### Line 241", 1);  // _S2 remove
  if (fseek(fp, 0, SEEK_SET) != 0) {
    fclose(fp);
    MyCout("Could not fseek(set) file", 1);  //_S2 ToDo find replacement:	DebugLog("Could not fseek(set) file");
    return FALSE;
  }

  // #ifdef KMX_64BIT
  //  allocate enough memory for expanded data structure + original data.
  //  Expanded data structure is double the size of data on disk (8-byte
  //  pointers) - on disk the "pointers" are relative to the beginning of
  //  the file.
  //  We save the original data at the end of buf; we don't copy strings, so
  //  those will remain in the location at the end of the buffer.
  //  buf = new KMX_BYTE[sz * 3];
  // #else
  buf = new KMX_BYTE[sz];
  // #endif

    MyCout("#### Line 260 ", 1);
  if (!buf) {
    fclose(fp);
    MyCout("Not allocmem", 1);  //_S2 ToDo find replacement: DebugLog()"Not allocmem");
                                // _S2 delete [] buf; ????
    return FALSE;
  }

  // #ifdef KMX_64BIT
  // ilebase = buf + sz*2;
  // #else
  filebase = buf;
  // #endif

  if (fread(filebase, 1, sz, fp) < (size_t)sz) {
    MyCout("Could not read file", 1);  //_S2 ToDo find replacement: DebugLog("Could not read file");
    fclose(fp);
    // _S2 delete [] buf; ????
    return FALSE;
  }

  fclose(fp);

  MyCout("##### Line 285", 1);
  ;
  KMX_DWORD sz_dw = (KMX_DWORD)sz;  //_S2
  size_t sz_t = (size_t)sz;  //_S2
  // shold call VerifyKeyboard_M of class
  if (!VerifyKeyboard(filebase, sz_t)) {
    MyCout("##### errVerifyKeyboard", 1);
    // Err(L"errVerifyKeyboard");             //_S2 ToDo find replacement: Err
    // _S2 delete [] buf; ????
    return FALSE;
  }

  MyCout("##### Line 297", 1);
  kbp = FixupKeyboard(buf, filebase, sz_dw);    // _S" changed from sz->sz_dw
  MyCout("##### Line 299", 1);

  std::cout << "kbp: " << kbp << "\n";
  if (!kbp) {
    // Err(L"errFixupKeyboard");								//_S2 ToDo find replacement: Err
    //  _S2 delete [] buf; ????

    MyCout("##### errFixupKeyboard ", 1);
    return FALSE;
  }

  MyCout("##### Line 311 ", 1);
  //_S2 can go:
  //if(kbp->dwIdentifier != FILEID_COMPILED) {
  //Err(L"errNotFileID");
  //delete[] buf;
  //return FALSE;
//}


  std::cout << "kbp->dwIdentifier: " << kbp->dwIdentifier << " FILEID_COMPILED: " << FILEID_COMPILED << "\n";
  std::cout << "..xxxxx.\n";
  if (kbp->dwIdentifier != FILEID_COMPILED) {
    delete[] buf;
    MyCout("errNotFileID", 1);  //_S2 ToDo find replacement: DebugLog("errNotFileID");
    return FALSE;
  }
  MyCout("##### Line 327", 1);
  *lpKeyboard = kbp;
  // _S2 delete [] buf; ????
  MyCout("##### LoadKeyboard of mcompile ended #####", 1);
  return TRUE;
}
*/

/*KMX_BOOL VerifyKeyboard(LPBYTE filebase, KMX_DWORD sz) {
  KMX_DWORD i;
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) filebase;
  PCOMP_STORE csp;

	// Check file version //

	if(ckbp->dwFileVersion < VERSION_MIN ||
	   ckbp->dwFileVersion > VERSION_MAX) {
		// Old or new version -- identify the desired program version //
			for(csp = (PCOMP_STORE)(filebase + ckbp->dpStoreArray), i = 0; i < ckbp->cxStoreArray; i++, csp++) {
				if(csp->dwSystemID == TSS_COMPILEDVERSION) {
					//wchar_t buf2[256];
          if(csp->dpString == 0) {
					// _S2 wsprintf(buf2, L"errWrongFileVersion:NULL");
          MyCout("errWrongFileVersion",1);
          } else {
					  // _S2 wsprintf(buf2, L"errWrongFileVersion:%10.10ls", StringOffset(filebase, csp->dpString));

          MyCout("errWrongFileVersion-offset",1);
          }

          MyCout("err buf",1);
					// _S2 Err(buf2);
					return FALSE;
				}
		}

          MyCout("errWrongFileVersion",1);
		// _S2 Err(L"errWrongFileVersion");
		return FALSE;
	}

MyCout("will return true",1);
  return TRUE;
}
*/

KMX_BOOL KMX_VerifyKeyboard(LPKMX_BYTE filebase, KMX_DWORD sz){

  MyCoutW(L"  #### KMX_VerifyKeyboard of mc_kmxfile started", 1);
  KMX_DWORD i;
  PKMX_COMP_KEYBOARD ckbp = (PKMX_COMP_KEYBOARD)filebase;
  PKMX_COMP_STORE csp;

  // Check file version //

  if (ckbp->dwFileVersion < VERSION_MIN || ckbp->dwFileVersion > VERSION_MAX) {
    // Old or new version -- identify the desired program version //
    for (csp = (PKMX_COMP_STORE)(filebase + ckbp->dpStoreArray), i = 0; i < ckbp->cxStoreArray; i++, csp++) {
      if (csp->dwSystemID == TSS_COMPILEDVERSION) {
        //wchar_t buf2[256];
        if (csp->dpString == 0) {
          // _S2 wsprintf(buf2, L"errWrongFileVersion:NULL");
          MyCout("errWrongFileVersion", 1);
        } else {
          // _S2 wsprintf(buf2, L"errWrongFileVersion:%10.10ls", StringOffset(filebase, csp->dpString));

          MyCout("errWrongFileVersion-offset", 1);
        }

        MyCout("err buf", 1);
        // _S2 Err(buf2);
        return FALSE;
      }
    }

    MyCout("errWrongFileVersion", 1);
    // _S2 Err(L"errWrongFileVersion");
    return FALSE;
  }
  /**/
  MyCout("will return true", 1);

    MyCoutW(L"  #### KMX_VerifyKeyboard of mc_kmxfile ended", 1);
  return TRUE;
}



//---------------------old----------------------------------------
/*
#include "pch.h"


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

	if(!ReadFile(hFile, filebase, sz, &sz, NULL)) {
    Err(L"errReadFile");
    CloseHandle(hFile);
    delete[] buf;
    return FALSE;
  }
	CloseHandle(hFile);

	if(!VerifyKeyboard(filebase, sz)) {
    Err(L"errVerifyKeyboard");
    delete[] buf;
    return FALSE;
  }

	kbp = FixupKeyboard(buf, filebase, sz);
  if(!kbp) {
    Err(L"errFixupKeyboard");
    delete[] buf;
    return FALSE;
  }

	if(kbp->dwIdentifier != FILEID_COMPILED) {
    Err(L"errNotFileID");
    delete[] buf;
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

BOOL VerifyKeyboard(LPBYTE filebase, DWORD sz) {
  DWORD i;
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) filebase;
  PCOMP_STORE csp;

	// Check file version //

	if(ckbp->dwFileVersion < VERSION_MIN ||
	   ckbp->dwFileVersion > VERSION_MAX) {
		// Old or new version -- identify the desired program version //
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
		Err(L"errWrongFileVersion");
		return FALSE;
	}


  return TRUE;
}
*/