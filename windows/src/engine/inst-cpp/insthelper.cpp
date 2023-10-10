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

const LPCTSTR SFolderKeymanRoot = TEXT("\\Keyman");

unsigned int
EnginePostInstall(MSIHANDLE hInstall) {
  HANDLE hFile;

  // Find %appdata% path
  TCHAR path[MAX_PATH];
  if (SUCCEEDED(SHGetFolderPathW(NULL, CSIDL_COMMON_APPDATA, NULL, 0, path))) {
    _tcscat_s(path, SFolderKeymanRoot);

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

    // Create file
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

  return ERROR_SUCCESS;
}

unsigned int
PreUninstall() {
  return 1;
}
