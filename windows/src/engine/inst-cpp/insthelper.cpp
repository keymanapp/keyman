#include "insthelper.h"
#include "pch.h"
#include <AccCtrl.h>
#include <ShlObj_core.h>
#include <iostream>
#include <knownfolders.h>
#include <limits.h>
#include <msiquery.h>
//#include <security.h>
#include <utility>
#include <tchar.h>
#include <string>
#include <AclAPI.h>
#include <Windows.h>
#include <Msi.h>
#include <initguid.h>
#include <comdef.h>
#include <msctf.h>
#include <wrl/client.h>
// #include <atlbase.h>
#include<sstream>
#include<iomanip>
#include "kmtip_guids.h"

const CLSID c_clsidKMTipTextService = {0xFE0420F1, 0x38D1, 0x4B4C, {0x96, 0xBF, 0xE7, 0xE2, 0x0A, 0x74, 0xCF, 0xB7}};
const LPCTSTR SFolderKeymanRoot = TEXT("\\Keyman");

extern "C" unsigned int
HandleError(const MSIHANDLE& hInstall, const std::wstring& messagePrefix) {
  DWORD errorCode    = GetLastError();
  std::wstring error = messagePrefix + std::to_wstring(errorCode);
  MsiSetProperty(hInstall, TEXT("EnginePostInstall_Error"), error.c_str());
  return errorCode;
}

extern "C" __declspec(dllexport) UINT WINAPI EnginePostInstall(MSIHANDLE hInstall)
{
  HANDLE hFile;

  // Find %appdata% path
  wchar_t path[MAX_PATH]={0};
  PWSTR ppath = nullptr;

  if (!SUCCEEDED(SHGetKnownFolderPath(FOLDERID_ProgramData, 0, NULL, &ppath))) {
    HandleError(hInstall, L"Keyman Engine failed to get known folder path");
  } else {
    wcscat_s(path, MAX_PATH, ppath);
    wcscat_s(path, MAX_PATH, SFolderKeymanRoot);

    // Create directory if it does not exist
    if (GetFileAttributes(path) == INVALID_FILE_ATTRIBUTES) {
      if (!CreateDirectory(path, NULL)) {
        return HandleError(hInstall, L"Keyman Engine failed to set permissions on shared data in CreateDir: ");
      }
    }

    // Create file handle
    hFile = CreateFile(path, READ_CONTROL | WRITE_DAC, 0, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (hFile == INVALID_HANDLE_VALUE) {
      return HandleError(hInstall, L"Keyman Engine failed to set permissions on shared data in CreateFile: ");
    }
    // Create an SID for the WinBuiltinAnyPackageSid group on the local computer.
    BYTE sidBuffer[SECURITY_MAX_SID_SIZE] ;
    PSID pSid = sidBuffer ;
    DWORD sidSize = sizeof( sidBuffer ) ;
    if ( !CreateWellKnownSid( WinBuiltinAnyPackageSid, nullptr, sidBuffer, &sidSize ) ) {
      CloseHandle(hFile);
      return HandleError(hInstall, L"Keyman Engine failed to create SID for the WinBuiltinAnyPackageSid group on the local computer: ");
    }
    // Set permission on shared data
    EXPLICIT_ACCESS ea      = {0};
    ea.grfAccessPermissions = GENERIC_READ | GENERIC_EXECUTE;
    ea.grfAccessMode        = SET_ACCESS;
    ea.grfInheritance       = SUB_CONTAINERS_AND_OBJECTS_INHERIT;
    ea.Trustee.TrusteeForm  = TRUSTEE_IS_SID;
    ea.Trustee.TrusteeType  = TRUSTEE_IS_WELL_KNOWN_GROUP;
    ea.Trustee.ptstrName    = (LPWSTR)pSid ;

    // Get a pointer to the existing DACL
    PACL pOldDACL = nullptr;
    PACL pNewDACL = nullptr;
    SE_OBJECT_TYPE objectType = SE_FILE_OBJECT;

    DWORD dwRes = GetSecurityInfo(hFile, objectType, DACL_SECURITY_INFORMATION, nullptr, nullptr, &pOldDACL, nullptr, nullptr);
    if (dwRes != ERROR_SUCCESS) {
      if (!CreateDirectory(path, NULL)) {
        return HandleError(hInstall, L"Keyman Engine failed to point to existing DACL");
      }
    }

    // Set entries in ACL
    dwRes = SetEntriesInAcl(1, &ea, pOldDACL, &pNewDACL);
    if (dwRes != ERROR_SUCCESS) {
      if (!CreateDirectory(path, NULL)) {
        return HandleError(hInstall, L"Keyman Engine failed to set new DACL");
      }
    }

    DWORD result = SetEntriesInAcl(1, &ea, NULL, &pNewDACL);
    if (result != ERROR_SUCCESS) {
      LocalFree(pNewDACL);
      CloseHandle(hFile);
      return HandleError(hInstall, L"Keyman Engine failed to set permissions on shared data in GrantPermission: ");
    }

    result = SetNamedSecurityInfo(path, SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, NULL, NULL, pNewDACL, NULL);
    if (result != ERROR_SUCCESS) {
      return HandleError(hInstall, L"Keyman Engine failed to apply DACL to shared data folder: ");
    }

    LocalFree(pNewDACL);
    CloseHandle(hFile);
  }

  CoTaskMemFree( ppath );
  return ERROR_SUCCESS;
}

const DWORD ILOT_UNINSTALL = 1;

std::wstring
GuidToWString(const GUID& guid) {
  wchar_t guidCStr[40] = {0};
  int length           = StringFromGUID2(guid, guidCStr, 40);

  // Successfully converted GUID to wstring
  return std::wstring(guidCStr);
}

std::wstring
GetLayoutInstallString(int LangID, const GUID& guidProfile) {
  std::wostringstream result;

  result << std::hex << std::setw(4) << std::setfill(L'0') << LangID << L":" << GuidToWString(c_clsidKMTipTextService)
         << GuidToWString(guidProfile);

  return result.str();
}

bool
InstallLayoutOrTip(const wchar_t* FLayoutInstallString, DWORD Flags) {
  HMODULE hInputDll;
  BOOL result = FALSE;

  // Function pointer type for InstallLayoutOrTip
  typedef BOOL(WINAPI * TInstallLayoutOrTipFunc)(const wchar_t*, DWORD);

  // Load the DLL
  hInputDll = LoadLibrary(L"input.dll");
  if (!hInputDll) {
    return false;
  }

  // Get the function pointer
  TInstallLayoutOrTipFunc PInstallLayoutOrTip;
  PInstallLayoutOrTip = (TInstallLayoutOrTipFunc)GetProcAddress(hInputDll, "InstallLayoutOrTip");
  if (!PInstallLayoutOrTip) {
    FreeLibrary(hInputDll);
    return false;
  }

  // Call the function
  result = PInstallLayoutOrTip(FLayoutInstallString, Flags);

  // Free the DLL
  FreeLibrary(hInputDll);

  return result ? true : false;
}
// Unregister the TIP
using Microsoft::WRL::ComPtr;

void UnregisterTIPAndItsProfiles(const CLSID& AClsid)
{
    HRESULT hr;
    ComPtr<ITfInputProcessorProfiles> pInputProcessorProfiles;
    ComPtr<ITfInputProcessorProfileMgr> pInputProcessorProfileMgr;
    ComPtr<IEnumTfInputProcessorProfiles> ippEnum;

    ULONG pcFetch;
    TF_INPUTPROCESSORPROFILE profile;
    std::wstring FLayoutInstallString;

    hr = CoCreateInstance(
        CLSID_TF_InputProcessorProfiles,
        nullptr,
        CLSCTX_INPROC_SERVER,
        IID_PPV_ARGS(pInputProcessorProfiles.GetAddressOf()));
    if (FAILED(hr))
        throw _com_error(hr);

    hr = pInputProcessorProfiles->QueryInterface(
        IID_PPV_ARGS(pInputProcessorProfileMgr.GetAddressOf()));
    if (FAILED(hr))
        throw _com_error(hr);

    hr = pInputProcessorProfileMgr->EnumProfiles(
        0,
        ippEnum.GetAddressOf());
    if (FAILED(hr))
        throw _com_error(hr);

    while (ippEnum->Next(1, &profile, &pcFetch) == S_OK)
    {
        if (profile.dwProfileType == TF_PROFILETYPE_INPUTPROCESSOR &&
            IsEqualGUID(profile.clsid, AClsid))
        {
            FLayoutInstallString = GetLayoutInstallString(profile.langid, profile.guidProfile);
            InstallLayoutOrTip(FLayoutInstallString.c_str(), ILOT_UNINSTALL);
            pInputProcessorProfileMgr->UnregisterProfile(
                AClsid, profile.langid, profile.guidProfile, 0);
        }
    }

    pInputProcessorProfiles->Unregister(AClsid);
}

extern "C" __declspec(dllexport) UINT WINAPI PreUninstall( MSIHANDLE hInstall ) {
  // Initialize COM
  HRESULT hr = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
  if (SUCCEEDED(hr)) {
    try {
      UnregisterTIPAndItsProfiles(c_clsidKMTipTextService);
    } catch (...) {
      // Swallow exceptions so we don't break the uninstall
    }

    // Clean up COM
    CoUninitialize();
  }
  else
    throw _com_error(hr);

  return ERROR_SUCCESS;
}
