#include "insthelper.h"
#include "pch.h"
#include <AccCtrl.h>
#include <ShlObj_core.h>
#include <iostream>
#include <knownfolders.h>
#include <limits.h>
#include <msiquery.h>
#include <security.h>
#include <utility>
#include <tchar.h>
#include <string>
#include <AclAPI.h>
#include <Windows.h>
#include <Msi.h>
#include <initguid.h>
#include <comdef.h>
#include <msctf.h>
#include <atlbase.h>
#include<sstream>
#include<iomanip>

const LPCTSTR SFolderKeymanRoot = TEXT("\\Keyman");

extern "C" __declspec(dllexport) unsigned int EnginePostInstall(MSIHANDLE hInstall) {
  HANDLE hFile;

  // Find %appdata% path
  PWSTR path = new wchar_t[MAX_PATH];

  if (SUCCEEDED(SHGetKnownFolderPath(FOLDERID_ProgramData, 0, NULL, &path))) {
    wcscat_s(path, MAX_PATH, SFolderKeymanRoot);

    // Create directory if it does not exist
    if (GetFileAttributes(path) == INVALID_FILE_ATTRIBUTES) {
      if (!CreateDirectory(path, NULL)) {
        DWORD errorCode = GetLastError();
        std::wstring error =
            L"Keyman Engine failed to set permissions on shared data in CreateDir: " + std::to_wstring(errorCode);
        MsiSetProperty(hInstall, TEXT("EnginePostInstall_Error"), error.c_str());
        return errorCode;
      }
    }

    // Create file handle
    hFile = CreateFile(path, READ_CONTROL | WRITE_DAC, 0, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (hFile == INVALID_HANDLE_VALUE) {
      DWORD errorCode    = GetLastError();
      std::wstring error = L"Keyman Engine failed to set permissions on shared data in CreateFile: " + std::to_wstring(errorCode);
      MsiSetProperty(hInstall, TEXT("EnginePostInstall_Error"), error.c_str());
      return errorCode;
    }

    // Set permission on shared data
    EXPLICIT_ACCESS ea      = {0};
    ea.grfAccessPermissions = GENERIC_READ | GENERIC_EXECUTE;
    ea.grfAccessMode        = SET_ACCESS;
    ea.grfInheritance       = SUB_CONTAINERS_AND_OBJECTS_INHERIT;
    ea.Trustee.TrusteeForm  = TRUSTEE_IS_SID;
    ea.Trustee.ptstrName    = (LPWCH)L"ALL APPLICATION PACKAGES";

    // Get a pointer to the existing DACL
    PACL pOldDACL = NULL;
    PACL pNewDACL = NULL;
    SE_OBJECT_TYPE objectType = SE_FILE_OBJECT;

    DWORD dwRes = GetSecurityInfo(hFile, objectType, DACL_SECURITY_INFORMATION, nullptr, nullptr, &pOldDACL, nullptr, nullptr);
    if (dwRes != ERROR_SUCCESS) {
      if (!CreateDirectory(path, NULL)) {
        DWORD errorCode = GetLastError();
        std::wstring error =
            L"Keyman Engine failed to point to existing DACL";
        MsiSetProperty(hInstall, TEXT("EnginePostInstall_Error"), error.c_str());
        return errorCode;
      }
    }

    // Set entries in ACL
    dwRes = SetEntriesInAcl(1, &ea, pOldDACL, &pNewDACL);
    if (dwRes != ERROR_SUCCESS) {
      if (!CreateDirectory(path, NULL)) {
        DWORD errorCode    = GetLastError();
        std::wstring error = L"Keyman Engine failed to set new DACL";
        MsiSetProperty(hInstall, TEXT("EnginePostInstall_Error"), error.c_str());
        return errorCode;
      }
    }

    DWORD result = SetEntriesInAcl(1, &ea, NULL, &pNewDACL);
    if (result != ERROR_SUCCESS) {
      CloseHandle(hFile);
      std::wstring error =
          L"Keyman Engine failed to set permissions on shared data in GrantPermission: " + std::to_wstring(result);
      MsiSetProperty(hInstall, TEXT("EnginePostInstall_Error"), error.c_str());
      if (pNewDACL) {
        LocalFree(pNewDACL);
      }
      return result;
    }

    result = SetNamedSecurityInfo(path, SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, NULL, NULL, pNewDACL, NULL);
    if (result != ERROR_SUCCESS) {
      std::wstring error = L"Keyman Engine failed to apply DACL to shared data folder: " + std::to_wstring(result);
      MsiSetProperty(hInstall, TEXT("EnginePostInstall_Error"), error.c_str());
    }

    if (pNewDACL) {
      LocalFree(pNewDACL);
    }

    CloseHandle(hFile);
  }

  CoTaskMemFree(path);
  return ERROR_SUCCESS;
}

// Define the CLS ID for KMTip Text Service
DEFINE_GUID(c_clsidKMTipTextService, 0xFE0420F1, 0x38D1, 0x4B4C, 0x96, 0xBF, 0xE7, 0xE2, 0x0A, 0x74, 0xCF, 0xB7);

// Define the CLASS_TF_InputProcessorProfiles constant
DEFINE_GUID(CLASS_TF_InputProcessorProfiles, 0x33C53A50, 0xF456, 0x4884, 0xB0, 0x49, 0x85, 0xFD, 0x64, 0x3E, 0xCF, 0xED);

DEFINE_GUID(IID_ITfInputProcessorProfiles, 0x1F02B6C5, 0x7842, 0x4EE6, 0x8A, 0x0B, 0x9A, 0x24, 0x18, 0x3A, 0x95, 0xCA);

const DWORD ILOT_UNINSTALL = 1;

std::wstring
GuidToWString(const GUID& guid) {
  wchar_t guidCStr[40] = {0};
  int length           = StringFromGUID2(guid, guidCStr, 40);

  if (length > 0) {
    // Successfully converted GUID to wstring
    return std::wstring(guidCStr);
  } else {
    // Handle the error, for example, return an empty wstring
    return std::wstring();
  }
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
void UnregisterTIPAndItsProfiles(const CLSID& AClsid) {
  HRESULT hr;
  CComPtr<ITfInputProcessorProfiles> pInputProcessorProfiles;
  CComPtr<ITfInputProcessorProfileMgr> pInputProcessorProfileMgr;
  CComPtr<IEnumTfInputProcessorProfiles> ippEnum;
  ULONG pcFetch;
  TF_INPUTPROCESSORPROFILE profile;
  std::wstring FLayoutInstallString;

  // Create an instance of ITF Input Processor Profiles
  hr = CoCreateInstance(
      CLASS_TF_InputProcessorProfiles, NULL, CLSCTX_INPROC_SERVER, IID_ITfInputProcessorProfiles,
      (void**)&pInputProcessorProfiles);
  if (FAILED(hr))
    throw _com_error(hr);

  // Query for ITfInputProcessorProfileMgr interface
  hr = pInputProcessorProfiles->QueryInterface(IID_ITfInputProcessorProfileMgr, (void**)&pInputProcessorProfileMgr);
  if (FAILED(hr))
    throw _com_error(hr);

  // Enumerate the profiles
  hr = pInputProcessorProfileMgr->EnumProfiles(0, &ippEnum);
  if (FAILED(hr))
    throw _com_error(hr);

  // Unregister the input profiles installed through Keyman
  while (ippEnum->Next(1, &profile, &pcFetch) == S_OK) {
    if (profile.dwProfileType == TF_PROFILETYPE_INPUTPROCESSOR && IsEqualGUID(profile.clsid, AClsid)) {
      FLayoutInstallString = GetLayoutInstallString(profile.langid, profile.guidProfile);
      InstallLayoutOrTip(FLayoutInstallString.c_str(), ILOT_UNINSTALL);
      pInputProcessorProfileMgr->UnregisterProfile(AClsid, profile.langid, profile.guidProfile, 0);
    }
  }

  // Unregister the entire input processor
  pInputProcessorProfiles->Unregister(AClsid);
}

extern "C" __declspec(dllexport) unsigned int PreUninstall() {
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
