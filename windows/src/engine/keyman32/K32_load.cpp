/*
  Name:             K32_load
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Support unencrypted keyboards for any internal Keyman product
                    19 Jun 2007 - mcdurdin - Load ICO files and convert to BMP (for TIP)
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    02 Dec 2011 - mcdurdin - I3160 - Hotkeys can fail to activate keyboards when multiple OEM products are running
                    04 Nov 2012 - mcdurdin - I3522 - V9.0 - Merge of I3160 - Hotkeys can fail to activate keyboards when multiple OEM products are running
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    06 Feb 2015 - mcdurdin - I4552 - V9.0 - Add mnemonic recompile option to ignore deadkeys
                    25 Oct 2016 - mcdurdin - I5136 - Remove additional product references from Keyman Engine
*/

#include "pch.h"

HBITMAP LoadBitmapFile(LPBYTE data, DWORD sz);
BOOL VerifyKeyboard(LPBYTE filebase);

#ifdef _WIN64
LPKEYBOARD CopyKeyboard(PBYTE bufp, PBYTE base, DWORD dwFileSize);
#else
LPKEYBOARD FixupKeyboard(PBYTE bufp, PBYTE base, DWORD dwFileSize);
#endif

HBITMAP LoadBitmapFileEx(PBYTE filebase);

BOOL GetKeyboardFileName(LPSTR kbname, LPSTR buf, int nbuf)
{
  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if(!_td) return FALSE;

  int n = 0;
  RegistryReadOnly *reg = Reg_GetKeymanInstalledKeyboard(kbname);
  if(!reg) return FALSE;

  __try
  {
    // We need to test if the user is in deadkey conversion mode    // I4552
    if(Globals::get_MnemonicDeadkeyConversionMode() && reg->ValueExists(REGSZ_KeymanFile_MnemonicOverride_Deadkey)) {
      n = reg->ReadString(REGSZ_KeymanFile_MnemonicOverride_Deadkey, buf, nbuf);
    }
    else if(reg->ValueExists(REGSZ_KeymanFile_MnemonicOverride)) {   // I4169
      n = reg->ReadString(REGSZ_KeymanFile_MnemonicOverride, buf, nbuf);
    } else {
      n = reg->ReadString(REGSZ_KeymanFile, buf, nbuf);
    }
  }
  __finally
  {
    delete reg;
  }
  return n;
}

BOOL LoadlpKeyboard(int i)
{
  SendDebugMessageFormat(0, sdmLoad, 0, "%s: Enter ---", __FUNCTION__);

  PKEYMAN64THREADDATA _td = ThreadGlobals();
  if (!_td) return FALSE;
  if (_td->lpKeyboards[i].lpCoreKeyboard) return TRUE;
  if (_td->lpActiveKeyboard == &_td->lpKeyboards[i]) _td->lpActiveKeyboard = NULL;  // I822 TSF not working

  if (_td->lpKeyboards[i].lpCoreKeyboardState) {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadlpKeyboard: a keyboard km_kbp_state exits without matching keyboard - disposing of state");
    km_kbp_state_dispose(_td->lpKeyboards[i].lpCoreKeyboardState);
    _td->lpKeyboards[i].lpCoreKeyboardState = NULL;
  }

  char buf[256];
  if (!GetKeyboardFileName(_td->lpKeyboards[i].Name, buf, 255)) return FALSE;
  PWCHAR keyboardPath = strtowstr(buf);
  km_kbp_status err_status = km_kbp_keyboard_load(keyboardPath, &_td->lpKeyboards[i].lpCoreKeyboard);
  if (err_status != KM_KBP_STATUS_OK) {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadlpKeyboard: km_kbp_keyboard_load failed for %ls with error status [%d]", keyboardPath, err_status);
    delete keyboardPath;
    return FALSE;
  }
  delete keyboardPath;

  km_kbp_option_item *core_environment = nullptr;

  if(!SetupCoreEnvironment(&core_environment)) {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadlpKeyboard: Unable to set environment options for keyboard %ls", keyboardPath);
    return FALSE;
  }

  err_status = km_kbp_state_create(_td->lpKeyboards[i].lpCoreKeyboard, core_environment, &_td->lpKeyboards[i].lpCoreKeyboardState);

  DeleteCoreEnvironment(core_environment);

  if (err_status != KM_KBP_STATUS_OK) {
    SendDebugMessageFormat(
        0, sdmLoad, 0, "LoadlpKeyboard: km_kbp_state_create failed with error status [%d]", err_status);
    // Dispose of the keyboard to leave us in a consistent state
    ReleaseKeyboardMemoryCore(&_td->lpKeyboards[i].lpCoreKeyboard);
    return FALSE;
  }
  // Register callback?
  err_status = km_kbp_keyboard_get_imx_list(_td->lpKeyboards[i].lpCoreKeyboard, &_td->lpKeyboards[i].lpIMXList);
  if (err_status != KM_KBP_STATUS_OK) {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadlpKeyboard: km_kbp_keyboard_get_imx_list failed with error status [%d]", err_status);
    // Dispose of the keyboard to leave us in a consistent state
    ReleaseKeyboardMemoryCore(&_td->lpKeyboards[i].lpCoreKeyboard);
    return FALSE;
  }

  LoadDLLs(&_td->lpKeyboards[i]);

  LoadKeyboardOptionsRegistrytoCore(&_td->lpKeyboards[i], _td->lpKeyboards[i].lpCoreKeyboardState);

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

//#define Err(s)

void Err(char *s)
{
  SendDebugMessageFormat(0, sdmLoad, 0, "LoadKeyboard: %s", s);
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

#ifdef _WIN64
  // allocate enough memory for expanded data structure + original data.
  // Expanded data structure is double the size of data on disk (8-byte
  // pointers) - on disk the "pointers" are relative to the beginning of
  // the file.
  // We save the original data at the end of buf; we don't copy strings, so
  // those will remain in the location at the end of the buffer.
  buf = new BYTE[sz * 3];
#else
  buf = new BYTE[sz];
#endif

  if(!buf)
  {
    CloseHandle(hFile);
    Err("Not allocmem");
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
  if (!_td) {
    delete[] buf;
    return FALSE;
  }

  if(*LPDWORD(filebase) != FILEID_COMPILED)
  {
    delete[] buf;
    Err("Invalid file");
    return FALSE;
  }

  if(!VerifyKeyboard(filebase)) return FALSE;

#ifdef _WIN64
  kbp = CopyKeyboard(buf, filebase, sz);
#else
  kbp = FixupKeyboard(buf, filebase, sz);
#endif

  if(!kbp) return FALSE;

  if(kbp->dwIdentifier != FILEID_COMPILED) { delete buf; Err("errNotFileID"); return FALSE; }

  kbp->hBitmap = LoadBitmapFileEx(filebase);

  *lpKeyboard = kbp;

  return TRUE;
}



// These next two structs represent how the icon information is stored
// in an ICO file.
typedef struct
{
  BYTE	bWidth;               // Width of the image
  BYTE	bHeight;              // Height of the image (times 2)
  BYTE	bColorCount;          // Number of colors in image (0 if >=8bpp)
  BYTE	bReserved;            // Reserved
  WORD	wPlanes;              // Color Planes
  WORD	wBitCount;            // Bits per pixel
  DWORD	dwBytesInRes;         // how many bytes in this resource?
  DWORD	dwImageOffset;        // where in the file is this image
} ICONDIRENTRY, *LPICONDIRENTRY;
typedef struct
{
  WORD			idReserved;   // Reserved
  WORD			idType;       // resource type (1 for icons)
  WORD			idCount;      // how many images?
  ICONDIRENTRY	idEntries[1]; // the entries for each image
} ICONDIR, *LPICONDIR;

// The following two structs are for the use of this program in
// manipulating icons. They are more closely tied to the operation
// of this program than the structures listed above. One of the
// main differences is that they provide a pointer to the DIB
// information of the masks.
typedef struct
{
  UINT			Width, Height, Colors; // Width, Height and bpp
  LPBYTE			lpBits;                // ptr to DIB bits
  DWORD			dwNumBytes;            // how many bytes?
  LPBITMAPINFO	lpbi;                  // ptr to header
  LPBYTE			lpXOR;                 // ptr to XOR image bits
  LPBYTE			lpAND;                 // ptr to AND image bits
} ICONIMAGE, *LPICONIMAGE;
typedef struct
{
  UINT		nNumImages;                      // How many images?
  ICONIMAGE	IconImages[1];                   // Image entries
} ICONRESOURCE, *LPICONRESOURCE;
/****************************************************************************/
LPICONRESOURCE ReadIconFromICOFile( PBYTE buf, int sz );
void FreeIconResource(LPICONRESOURCE lpIR);
HICON MakeIconFromResource( LPICONIMAGE lpIcon );

HBITMAP LoadBitmapFile(LPBYTE data, DWORD sz)
{
  BITMAPFILEHEADER *bmfh;
  BITMAPINFO *bmi;
  HBITMAP hBitmap, hBitmap2, hOldBmp1, hOldBmp2;
  HDC hDC, hSrcDC, hSrcDC2;
  LPICONRESOURCE lpir = NULL;

  bmfh = (BITMAPFILEHEADER *) data;

  if(bmfh->bfType == 0x4D42)
  {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadKeyboard: Bitmap found");
    bmi = (BITMAPINFO *) (data + sizeof(BITMAPFILEHEADER));

    hDC = GetDC(GetDesktopWindow());
    hSrcDC = CreateCompatibleDC(hDC);
    hSrcDC2 = CreateCompatibleDC(hDC);
    hBitmap = CreateDIBitmap(hDC, &bmi->bmiHeader, CBM_INIT, data + bmfh->bfOffBits, bmi, DIB_RGB_COLORS);
    hBitmap2 = CreateCompatibleBitmap(hDC, 16, 16);
    ReleaseDC(GetDesktopWindow(), hDC);

    hOldBmp1 = (HBITMAP) SelectObject(hSrcDC, hBitmap);
    hOldBmp2 = (HBITMAP) SelectObject(hSrcDC2, hBitmap2);
    BitBlt(hSrcDC2, 0, 0, 16, 16, hSrcDC, 0, 0, SRCCOPY);
    SelectObject(hSrcDC, hOldBmp1);
    SelectObject(hSrcDC2, hOldBmp2);
    DeleteDC(hSrcDC);
    DeleteDC(hSrcDC2);
    DeleteObject(hBitmap);
  }
  else
  {
    SendDebugMessageFormat(0, sdmLoad, 0, "LoadKeyboard: Icon found");

    lpir = ReadIconFromICOFile(data, sz);
    if(!lpir)
    {
      SendDebugMessageFormat(0, sdmLoad, 0, "LoadKeyboard: icon not loaded");
      return 0;
    }

    if(lpir->nNumImages == 0)
    {
      FreeIconResource(lpir);
      return 0;
    }

    HICON hIcon = MakeIconFromResource(&lpir->IconImages[0]);
    //HICON hIcon = CreateIcon(GetModuleHandle(LIBRARY_NAME), lpir->IconImages[0].Width, lpir->IconImages[0].Height,
    //  1, lpir->IconImages[0].Colors, lpir->IconImages[0].lpAND, lpir->IconImages[0].lpXOR);
    FreeIconResource(lpir);


    if(hIcon == 0)
    {
      DebugLastError("MakeIconFromResource");
      return 0;
    }
    hDC = GetDC(GetDesktopWindow());

    hBitmap2 = CreateCompatibleBitmap(hDC, 16, 16);
    hSrcDC = CreateCompatibleDC(hDC);
    ReleaseDC(GetDesktopWindow(), hDC);

    hOldBmp2 = (HBITMAP) SelectObject(hSrcDC, hBitmap2);
    DrawIconEx(hSrcDC, 0, 0, hIcon, 16, 16, 0, NULL, DI_NORMAL);
    SelectObject(hSrcDC, hOldBmp2);
    DeleteDC(hSrcDC);

    DestroyIcon(hIcon);

  }
  return hBitmap2;
}



/****************************************************************************
*
*     FUNCTION: ReadICOHeader
*
*     PURPOSE:  Reads the header from an ICO file
*
*     PARAMS:   HANDLE hFile - handle to the file
*
*     RETURNS:  UINT - Number of images in file, -1 for failure
*
* History:
*                July '95 - Created
*
\****************************************************************************/
UINT ReadICOHeader( PBYTE buf )
{
    if(*(PWORD)(buf) != 0) return (UINT)-1;
    if(*(PWORD)(buf+2) != 1) return (UINT)-1;
    return *(PWORD)(buf+4);
}
/* End ReadICOHeader() ****************************************************/




/****************************************************************************
*
*     FUNCTION: DIBNumColors
*
*     PURPOSE:  Calculates the number of entries in the color table.
*
*     PARAMS:   LPSTR lpbi - pointer to the CF_DIB memory block
*
*     RETURNS:  WORD - Number of entries in the color table.
*
* History:
*                July '95 - Copied <g>
*
\****************************************************************************/
WORD DIBNumColors( LPSTR lpbi )
{
    WORD wBitCount;
    DWORD dwClrUsed;

    dwClrUsed = ((LPBITMAPINFOHEADER) lpbi)->biClrUsed;

    if (dwClrUsed)
        return (WORD) dwClrUsed;

    wBitCount = ((LPBITMAPINFOHEADER) lpbi)->biBitCount;

    switch (wBitCount)
    {
        case 1: return 2;
        case 4: return 16;
        case 8:	return 256;
    }
    return 0;
}
/* End DIBNumColors() ******************************************************/



/****************************************************************************
*
*     FUNCTION: PaletteSize
*
*     PURPOSE:  Calculates the number of bytes in the color table.
*
*     PARAMS:   LPSTR lpbi - pointer to the CF_DIB memory block
*
*     RETURNS:  WORD - number of bytes in the color table
*
*
* History:
*                July '95 - Copied <g>
*
\****************************************************************************/
WORD PaletteSize( LPSTR lpbi )
{
    return ( DIBNumColors( lpbi ) * sizeof( RGBQUAD ) );
}
/* End PaletteSize() ********************************************************/

/****************************************************************************
*
*     FUNCTION: FindDIBits
*
*     PURPOSE:  Locate the image bits in a CF_DIB format DIB.
*
*     PARAMS:   LPSTR lpbi - pointer to the CF_DIB memory block
*
*     RETURNS:  LPSTR - pointer to the image bits
*
* History:
*                July '95 - Copied <g>
*
\****************************************************************************/
LPSTR FindDIBBits( LPSTR lpbi )
{
   return ( lpbi + *(LPDWORD)lpbi + PaletteSize( lpbi ) );
}
/* End FindDIBits() *********************************************************/



// How wide, in bytes, would this many bits be, DWORD aligned?
#define WIDTHBYTES(bits)      ((((bits) + 31)>>5)<<2)

/****************************************************************************
*
*     FUNCTION: BytesPerLine
*
*     PURPOSE:  Calculates the number of bytes in one scan line.
*
*     PARAMS:   LPBITMAPINFOHEADER lpBMIH - pointer to the BITMAPINFOHEADER
*                                           that begins the CF_DIB block
*
*     RETURNS:  DWORD - number of bytes in one scan line (DWORD aligned)
*
* History:
*                July '95 - Created
*
\****************************************************************************/
DWORD BytesPerLine( LPBITMAPINFOHEADER lpBMIH )
{
    return WIDTHBYTES(lpBMIH->biWidth * lpBMIH->biPlanes * lpBMIH->biBitCount);
}
/* End BytesPerLine() ********************************************************/


/****************************************************************************
*
*     FUNCTION: AdjustIconImagePointers
*
*     PURPOSE:  Adjusts internal pointers in icon resource struct
*
*     PARAMS:   LPICONIMAGE lpImage - the resource to handle
*
*     RETURNS:  BOOL - TRUE for success, FALSE for failure
*
* History:
*                July '95 - Created
*
\****************************************************************************/
BOOL AdjustIconImagePointers( LPICONIMAGE lpImage )
{
    // Sanity check
    if( lpImage==NULL )
        return FALSE;
    // BITMAPINFO is at beginning of bits
    lpImage->lpbi = (LPBITMAPINFO)lpImage->lpBits;
    // Width - simple enough
    lpImage->Width = lpImage->lpbi->bmiHeader.biWidth;
    // Icons are stored in funky format where height is doubled - account for it
    lpImage->Height = (lpImage->lpbi->bmiHeader.biHeight)/2;
    // How many colors?
    lpImage->Colors = lpImage->lpbi->bmiHeader.biPlanes * lpImage->lpbi->bmiHeader.biBitCount;
    // XOR bits follow the header and color table
    lpImage->lpXOR = (LPBYTE) FindDIBBits((LPSTR)lpImage->lpbi);
    // AND bits follow the XOR bits
    lpImage->lpAND = lpImage->lpXOR + (lpImage->Height*BytesPerLine((LPBITMAPINFOHEADER)(lpImage->lpbi)));
    return TRUE;
}
/* End AdjustIconImagePointers() *******************************************/

/****************************************************************************
*
*     FUNCTION: ReadIconFromICOFile
*
*     PURPOSE:  Reads an Icon Resource from an ICO file
*
*     PARAMS:   LPCTSTR szFileName - Name of the ICO file
*
*     RETURNS:  LPICONRESOURCE - pointer to the resource, NULL for failure
*
* History:
*                July '95 - Created
*
\****************************************************************************/
LPICONRESOURCE ReadIconFromICOFile( PBYTE buf, int sz )
{
  UNREFERENCED_PARAMETER(sz);
    LPICONRESOURCE    	lpIR = NULL, lpNew = NULL;
    HANDLE            	hFile = NULL;
    //LPRESOURCEPOSINFO	lpRPI = NULL;
    UINT                i;
    LPICONDIRENTRY    	lpIDE = NULL;

    // Allocate memory for the resource structure
    if( (lpIR = (LPICONRESOURCE) malloc( sizeof(ICONRESOURCE) )) == NULL )
    {
        SendDebugMessageFormat(0, sdmLoad, 0, "Error Allocating Memory");
        return NULL;
    }

    // Read in the header
    if( (lpIR->nNumImages = ReadICOHeader(buf)) == (UINT)-1 )
    {
        SendDebugMessageFormat(0, sdmLoad, 0, "Error Reading File Header");
        free( lpIR );
        return NULL;
    }
    // Adjust the size of the struct to account for the images
    if( (lpNew = (LPICONRESOURCE) realloc( lpIR, sizeof(ICONRESOURCE) + ((lpIR->nNumImages-1) * sizeof(ICONIMAGE)) )) == NULL )
    {
        SendDebugMessageFormat(0, sdmLoad, 0, "Error Allocating Memory");
        CloseHandle( hFile );
        free( lpIR );
        return NULL;
    }
    lpIR = lpNew;
    // Store the original name

    // Allocate enough memory for the icon directory entries
    if( (lpIDE = (LPICONDIRENTRY) malloc( lpIR->nNumImages * sizeof( ICONDIRENTRY ) ) ) == NULL )
    {
        SendDebugMessageFormat(0, sdmLoad, 0, "Error Allocating Memory");
        free( lpIR );
        return NULL;
    }
    memcpy(lpIDE, buf + 6, lpIR->nNumImages * sizeof( ICONDIRENTRY ));

    // Loop through and read in each image
    for( i = 0; i < lpIR->nNumImages; i++ )
    {
        // Allocate memory for the resource
        if( (lpIR->IconImages[i].lpBits = (LPBYTE) malloc(lpIDE[i].dwBytesInRes)) == NULL )
        {
            SendDebugMessageFormat(0, sdmLoad, 0, "Error Allocating Memory");
            free( lpIR );
            free( lpIDE );
            return NULL;
        }
        lpIR->IconImages[i].dwNumBytes = lpIDE[i].dwBytesInRes;
        // Seek to beginning of this image
        memcpy(lpIR->IconImages[i].lpBits, buf + lpIDE[i].dwImageOffset, lpIDE[i].dwBytesInRes);

        // Set the internal pointers appropriately
        if( ! AdjustIconImagePointers( &(lpIR->IconImages[i]) ) )
        {
            SendDebugMessageFormat(0, sdmLoad, 0, "Error Converting to Internal Format");
            free( lpIDE );
            free( lpIR );
            return NULL;
        }
    }
    // Clean up
    free( lpIDE );
    //free( lpRPI );
    return lpIR;
}
/* End ReadIconFromICOFile() **********************************************/

void FreeIconResource(LPICONRESOURCE lpIR)
{
  for( UINT i = 0; i < lpIR->nNumImages; i++ )
    // Allocate memory for the resource
    free(lpIR->IconImages[i].lpBits);
  free( lpIR );
}

/****************************************************************************
*
*     FUNCTION: MakeIconFromResource
*
*     PURPOSE:  Makes an HICON from an icon resource
*
*     PARAMS:   LPICONIMAGE	lpIcon - pointer to the icon resource
*
*     RETURNS:  HICON - handle to the new icon, NULL for failure
*
* History:
*                July '95 - Created
*
\****************************************************************************/
HICON MakeIconFromResource( LPICONIMAGE lpIcon )
{
    HICON        	hIcon = NULL;

    // Sanity Check
    if( lpIcon == NULL )
        return NULL;
    if( lpIcon->lpBits == NULL )
        return NULL;
    // Let the OS do the real work :)
    hIcon = CreateIconFromResourceEx( lpIcon->lpBits, lpIcon->dwNumBytes, TRUE, 0x00030000,
            (*(LPBITMAPINFOHEADER)(lpIcon->lpBits)).biWidth, (*(LPBITMAPINFOHEADER)(lpIcon->lpBits)).biHeight/2, 0 );

    // It failed, odds are good we're on NT so try the non-Ex way
    if( hIcon == NULL )
    {
        // We would break on NT if we try with a 16bpp image
        if(lpIcon->lpbi->bmiHeader.biBitCount != 16)
        {
            hIcon = CreateIconFromResource( lpIcon->lpBits, lpIcon->dwNumBytes, TRUE, 0x00030000 );
        }
    }
    return hIcon;
}
/* End MakeIconFromResource() **********************************************/

PWCHAR StringOffset(PBYTE base, DWORD offset)
{
  if(offset == 0) return NULL;
  return (PWCHAR)(base + offset);
}

#ifdef _WIN64

/**
  CopyKeyboard will copy the data read into bufp from x86-sized structures into
  x64-sized structures starting at `base`
  * After this function finishes, we still need to keep the original data because
    we don't copy the strings
  This method is used on 64-bit architectures.
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
    gp->dpName = StringOffset(base, cgp->dpName);
    gp->dpKeyArray = cgp->cxKeyArray > 0 ? (LPKEY) bufp : NULL;
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

/**
 Fixup the keyboard by expanding pointers. On disk the pointers are stored relative to the
 beginning of the file, but we need real pointers. This method is used on 32-bit architectures.
*/
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

/*if( ckbp->dwBitmapSize > 0 )
    kbp->hBitmap = LoadBitmapFile((buf + ckbp->dpBitmapOffset), ckbp->dwBitmapSize);
  else
    kbp->hBitmap = NULL;
*/

  for(sp = kbp->dpStoreArray, csp = (PCOMP_STORE) sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)
  {
    sp->dpName = StringOffset(base, csp->dpName);
    sp->dpString = StringOffset(base, csp->dpString);
  }

  for(gp = kbp->dpGroupArray, cgp = (PCOMP_GROUP) gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)
  {
    gp->dpName = StringOffset(base, cgp->dpName);
    gp->dpKeyArray = cgp->cxKeyArray > 0 ? (LPKEY) (base + cgp->dpKeyArray) : NULL;
    gp->dpMatch = StringOffset(base, cgp->dpMatch);
    gp->dpNoMatch = StringOffset(base, cgp->dpNoMatch);

    for(kp = gp->dpKeyArray, ckp = (PCOMP_KEY) kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++)
    {
      kp->dpOutput = StringOffset(base, ckp->dpOutput);
      kp->dpContext = StringOffset(base, ckp->dpContext);
    }
  }

  return kbp;
}

#endif

HBITMAP LoadBitmapFileEx(PBYTE filebase)
{
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) filebase;

  if( ckbp->dwBitmapSize > 0 )
    return LoadBitmapFile(filebase + ckbp->dpBitmapOffset, ckbp->dwBitmapSize);
  else
    return NULL;
}


BOOL VerifyKeyboard(LPBYTE filebase)
{
  DWORD i;
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) filebase;
  PCOMP_STORE csp;

  /* Check file version */

  if(ckbp->dwFileVersion < VERSION_MIN ||
     ckbp->dwFileVersion > VERSION_MAX)
  {
    for(csp = (PCOMP_STORE)(filebase + ckbp->dpStoreArray), i = 0; i < ckbp->cxStoreArray; i++, csp++) {
      if(csp->dwSystemID == TSS_COMPILEDVERSION)
      {
        char buf2[256];
        if(csp->dpString == 0)
          wsprintf(buf2, "errWrongFileVersion:NULL");
        else
          wsprintf(buf2, "errWrongFileVersion:%10.10ls", StringOffset(filebase, csp->dpString));
        Err(buf2);
        return FALSE;
      }
    }
    Err("errWrongFileVersion");
    return FALSE;
  }

  return TRUE;
}
