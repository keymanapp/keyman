/*
  Name:             register
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Nov 2007

  Modified Date:    1 Jan 2013
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          06 Nov 2007 - mcdurdin - I1139 - Fix TSF 'language overload' issue on Vista
                    18 Mar 2011 - mcdurdin - I2794 - Handle leaks
                    24 Oct 2012 - mcdurdin - I3489 - V9.0 - Add categories for TIP to enable Win8 immersive mode
                    20 Nov 2012 - mcdurdin - I3581 - V9.0 - KMTip needs to pass activated profile guid through to Keyman32 to switch keyboards
                    01 Jan 2013 - mcdurdin - I3715 - V9.0 - Prepare to have kmtip working in secure screens such as boot
*/
//
// register.cpp
//
// Server registration code.
//

#include "stdafx.h"

#define CLSID_STRLEN 38  // strlen("{xxxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxx}")

// for Windows 8
const GUID GUID_TFCAT_TIPCAP_IMMERSIVESUPPORT =
{ 0x13A016DF, 0x560B, 0x46CD, { 0x94, 0x7A, 0x4C, 0x3A, 0xF1, 0xE0, 0xE3, 0x5D } };  // I3489
const GUID GUID_TFCAT_TIPCAP_SYSTRAYSUPPORT =
{ 0x25504FB4, 0x7BAB, 0x4BC1, { 0x9C, 0x69, 0xCF, 0x81, 0x89, 0x0F, 0x0E, 0xF5 } };  // I3489

const struct
{
    const GUID *pguidCategory;
    const GUID *pguid;
    const BOOL Win8;  // I3489
}
c_rgCategories[] =
{
  { &GUID_TFCAT_TIP_KEYBOARD,            &c_clsidI3619TIP, FALSE },
	{ &GUID_TFCAT_TIPCAP_IMMERSIVESUPPORT, &c_clsidI3619TIP, TRUE },  // I3489
  { &GUID_TFCAT_TIPCAP_SYSTRAYSUPPORT,   &c_clsidI3619TIP, TRUE },  // I3489
  //{ &GUID_TFCAT_TIPCAP_COMLESS,          &c_clsidI3619TIP, FALSE },
  { &GUID_TFCAT_TIPCAP_SECUREMODE, &c_clsidI3619TIP, FALSE }   // I3715
};

static const TCHAR c_szInfoKeyPrefix[] = TEXT("CLSID\\");
static const TCHAR c_szInProcSvr32[] = TEXT("InProcServer32");
static const TCHAR c_szModelName[] = TEXT("ThreadingModel");

//+---------------------------------------------------------------------------
//
//  RegisterProfiles
//
//----------------------------------------------------------------------------

BOOL CKMTipTextService::RegisterProfiles()
{
    ITfInputProcessorProfiles *pInputProcessProfiles = NULL;
    HRESULT hr;

    hr = CoCreateInstance(CLSID_TF_InputProcessorProfiles, NULL, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, (void**)&pInputProcessProfiles);
    if (hr != S_OK)
        return FALSE;

    hr = pInputProcessProfiles->Register(c_clsidI3619TIP);
    if (hr != S_OK)
        goto Exit;

Exit:
    if(pInputProcessProfiles != NULL)  // I2794
      pInputProcessProfiles->Release();

    return (hr == S_OK);
}

//+---------------------------------------------------------------------------
//
//  UnregisterProfiles
//
//----------------------------------------------------------------------------

void CKMTipTextService::UnregisterProfiles()
{
    ITfInputProcessorProfiles *pInputProcessProfiles;
    HRESULT hr;

    hr = CoCreateInstance(CLSID_TF_InputProcessorProfiles, NULL, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, (void**)&pInputProcessProfiles);

    if (hr != S_OK)
        return;

    pInputProcessProfiles->Unregister(c_clsidI3619TIP);
    pInputProcessProfiles->Release();
}

//+---------------------------------------------------------------------------
//
//  RegisterCategories
//
//----------------------------------------------------------------------------

OSVERSIONINFO osvi = {0};

BOOL IsWin8OrLater()
{
  if(osvi.dwOSVersionInfoSize == 0)
  {
    osvi.dwOSVersionInfoSize = sizeof(osvi);
    GetVersionEx(&osvi);
  }
  return (osvi.dwMajorVersion == 6 && osvi.dwMinorVersion >= 2) || osvi.dwMajorVersion > 6;
}

BOOL CKMTipTextService::RegisterCategories(BOOL fRegister)
{
    ITfCategoryMgr *pCategoryMgr;
    int i;
    HRESULT hr;

    hr = CoCreateInstance(CLSID_TF_CategoryMgr, NULL, CLSCTX_INPROC_SERVER, 
                          IID_ITfCategoryMgr, (void**)&pCategoryMgr);

    if (hr != S_OK)
        return E_FAIL;

    for (i=0; i<ARRAYSIZE(c_rgCategories); i++)
    {
      if(IsWin8OrLater() || !c_rgCategories[i].Win8)
      {
        if (fRegister)  // I3489
          {
              hr = pCategoryMgr->RegisterCategory(c_clsidI3619TIP,
                     *c_rgCategories[i].pguidCategory, *c_rgCategories[i].pguid);
          }
          else
          {
              hr = pCategoryMgr->UnregisterCategory(c_clsidI3619TIP,
                     *c_rgCategories[i].pguidCategory, *c_rgCategories[i].pguid);
          }

          if (hr != S_OK)
              break;
      }
    }

    pCategoryMgr->Release();
    return (hr == S_OK);
}

//+---------------------------------------------------------------------------
//
// CLSIDToStringA
//
//----------------------------------------------------------------------------

BOOL CLSIDToString(REFGUID refGUID, TCHAR *pchA)
{
    static const BYTE GuidMap[] = {3, 2, 1, 0, '-', 5, 4, '-', 7, 6, '-',
                                   8, 9, '-', 10, 11, 12, 13, 14, 15};

    static const TCHAR szDigits[] = L"0123456789ABCDEF";

    int i;
    TCHAR *p = pchA;

    const BYTE * pBytes = (const BYTE *) &refGUID;

    *p++ = '{';
    for (i = 0; i < sizeof(GuidMap); i++)
    {
        if (GuidMap[i] == '-')
        {
            *p++ = '-';
        }
        else
        {
            *p++ = szDigits[ (pBytes[GuidMap[i]] & 0xF0) >> 4 ];
            *p++ = szDigits[ (pBytes[GuidMap[i]] & 0x0F) ];
        }
    }

    *p++ = '}';
    *p   = '\0';

    return TRUE;
}

//+---------------------------------------------------------------------------
//
// RecurseDeleteKey
//
// RecurseDeleteKey is necessary because on NT RegDeleteKey doesn't work if the
// specified key has subkeys
//----------------------------------------------------------------------------
LONG RecurseDeleteKey(HKEY hParentKey, LPCTSTR lpszKey)
{
    HKEY hKey;
    LONG lRes;
    FILETIME time;
    TCHAR szBuffer[256];
    DWORD dwSize = sizeof(szBuffer);

    if (RegOpenKey(hParentKey, lpszKey, &hKey) != ERROR_SUCCESS)
        return ERROR_SUCCESS; // let's assume we couldn't open it because it's not there

    lRes = ERROR_SUCCESS;
    while (RegEnumKeyEx(hKey, 0, szBuffer, &dwSize, NULL, NULL, NULL, &time)==ERROR_SUCCESS)
    {
        lRes = RecurseDeleteKey(hKey, szBuffer);
        if (lRes != ERROR_SUCCESS)
            break;
        dwSize = sizeof(szBuffer);
    }
    RegCloseKey(hKey);

    return lRes == ERROR_SUCCESS ? RegDeleteKey(hParentKey, lpszKey) : lRes;
}

//+---------------------------------------------------------------------------
//
//  RegisterServer
//
//----------------------------------------------------------------------------

BOOL CKMTipTextService::RegisterServer()
{
    DWORD dw;
    HKEY hKey;
    HKEY hSubKey;
    BOOL fRet;
    TCHAR achIMEKey[ARRAYSIZE(c_szInfoKeyPrefix) + CLSID_STRLEN];
    TCHAR achFileName[MAX_PATH];

    if (!CLSIDToString(c_clsidI3619TIP, achIMEKey + ARRAYSIZE(c_szInfoKeyPrefix) - 1))
        return FALSE;
    memcpy(achIMEKey, c_szInfoKeyPrefix, sizeof(c_szInfoKeyPrefix)-sizeof(TCHAR));

    if (fRet = RegCreateKeyEx(HKEY_CLASSES_ROOT, achIMEKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &hKey, &dw)
            == ERROR_SUCCESS)
    {
        fRet &= RegSetValueEx(hKey, NULL, 0, REG_SZ, (BYTE *)KMTIP_DESC, (lstrlen(KMTIP_DESC)+1)*sizeof(TCHAR))
            == ERROR_SUCCESS;

        if (fRet &= RegCreateKeyEx(hKey, c_szInProcSvr32, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_WRITE, NULL, &hSubKey, &dw)
            == ERROR_SUCCESS)
        {
            dw = GetModuleFileName(g_hInst, achFileName, ARRAYSIZE(achFileName));

            fRet &= RegSetValueEx(hSubKey, NULL, 0, REG_SZ, (BYTE *)achFileName, (lstrlen(achFileName)+1)*sizeof(TCHAR)) == ERROR_SUCCESS;
            fRet &= RegSetValueEx(hSubKey, c_szModelName, 0, REG_SZ, (BYTE *)KMTIP_MODEL, (lstrlen(KMTIP_MODEL)+1)*sizeof(TCHAR)) == ERROR_SUCCESS;
            RegCloseKey(hSubKey);
        }
        RegCloseKey(hKey);
    }

    return fRet;
}

//+---------------------------------------------------------------------------
//
//  UnregisterServer
//
//----------------------------------------------------------------------------

void CKMTipTextService::UnregisterServer()
{
    TCHAR achIMEKey[ARRAYSIZE(c_szInfoKeyPrefix) + CLSID_STRLEN];

    if (!CLSIDToString(c_clsidI3619TIP, achIMEKey + ARRAYSIZE(c_szInfoKeyPrefix) - 1))
        return;
    memcpy(achIMEKey, c_szInfoKeyPrefix, sizeof(c_szInfoKeyPrefix)-sizeof(TCHAR));

    RecurseDeleteKey(HKEY_CLASSES_ROOT, achIMEKey);
}
