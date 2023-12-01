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
    ea.Trustee.TrusteeForm  = TRUSTEE_IS_NAME;
    ea.Trustee.ptstrName    = (LPWCH)L"ALL APPLICATION PACKAGES";

    PACL pOldDACL = NULL;

    DWORD result = SetEntriesInAcl(1, &ea, NULL, &pOldDACL);
    if (result != ERROR_SUCCESS) {
      CloseHandle(hFile);
      std::wstring error =
          L"Keyman Engine failed to set permissions on shared data in GrantPermission: " + std::to_wstring(result);
      MsiSetProperty(hInstall, TEXT("EnginePostInstall_Error"), error.c_str());
      if (pOldDACL) {
        LocalFree(pOldDACL);
      }
      return result;
    }

    result = SetNamedSecurityInfo(path, SE_FILE_OBJECT, DACL_SECURITY_INFORMATION, NULL, NULL, pOldDACL, NULL);
    if (result != ERROR_SUCCESS) {
      std::wstring error = L"Keyman Engine failed to apply DACL to shared data folder: " + std::to_wstring(result);
      MsiSetProperty(hInstall, TEXT("EnginePostInstall_Error"), error.c_str());
    }

    if (pOldDACL) {
      LocalFree(pOldDACL);
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

// Unregister the TIP
void UnregisterTIPAndItsProfiles(const CLSID& AClsid) {
  HRESULT hr;
  CComPtr<ITfInputProcessorProfiles> pInputProcessorProfiles;
  CComPtr<ITfInputProcessorProfileMgr> pInputProcessorProfileMgr;
  CComPtr<IEnumTfInputProcessorProfiles> ippEnum;

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
