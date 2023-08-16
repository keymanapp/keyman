#include "insthelper.h"
#include "pch.h"  // use stdafx.h in Visual Studio 2017 and earlier
#include <limits.h>
#include <utility>
#include <iostream>
#include <msiquery.h>
#include <knownfolders.h>
#include <ShlObj_core.h>
#include <security.h>
#include <AccCtrl.h>

std::wstring const SFolderKeymanRoot = L"Keyman";

void convertWStringToCharPtr(_In_ std::wstring input, _Out_ char *outputString) {
  size_t outputSize     = input.length() + 1;  // +1 for null terminator
  outputString          = new char[outputSize];
  size_t charsConverted = 0;
  const wchar_t *inputW = input.c_str();
  wcstombs_s(&charsConverted, outputString, outputSize, inputW, input.length());
}

std::string SysErrorMessage(DWORD errorCode) {
  // Declare a buffer to store the error message.
  char messageBuffer[1024];

  // Get the error message from the system.
  DWORD messageLength =
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nullptr, errorCode, 0, messageBuffer, sizeof(messageBuffer), nullptr);

  // If the error message is not empty, return it.
  if (messageLength > 0) {
    return std::string(messageBuffer);
  } else {
    return std::string("Unknown error");
  }
}

unsigned int ReportFailure(MSIHANDLE hInstall, std::string func, unsigned int code) {
  MsiSetProperty(
      hInstall, "EnginePostInstall_Error",
      (LPCWSTR)("Keyman Engine failed to set permissions on shared data in " + func + ": " + SysErrorMessage(code)));
}

void
GrantPermissionToAllApplicationPackages(LPCTSTR lpFolderPath) {
  SECURITY_ATTRIBUTES sa;
  sa.nLength              = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle       = FALSE;

  TRUSTEE_W trustee;
  trustee.TrusteeForm = TRUSTEE_IS_NAME;
  trustee.TrusteeType = TRUSTEE_IS_WELL_KNOWN_GROUP;
  trustee.ptstrName   = L"ALL APPLICATION PACKAGES";

  InitializeSecurityDescriptor(&sa.lpSecurityDescriptor, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorGroup(&sa.lpSecurityDescriptor, &trustee, 0);

  SetFileSecurity(lpFolderPath, DACL_SECURITY_INFORMATION, sa.lpSecurityDescriptor);
}

unsigned int EnginePostInstall(MSIHANDLE hInstall) {
  HANDLE hFile;
  std::wstring Path;

  try {
    // Find %appdata% path
    wchar_t appDataPath[MAX_PATH];
    if (SHGetFolderPathW(NULL, CSIDL_APPDATA, NULL, 0, appDataPath) != S_OK) {
      // Handle error
      return 1;
    }
    std::wstring AppDataPath(appDataPath);

    // Directory path
    Path = AppDataPath + SFolderKeymanRoot;

    // Create directory if it does not exist
    struct stat dirMeta;
    char *path;
    convertWStringToCharPtr(Path, path);
    if (stat(path, &dirMeta) != 0) {
      if (!CreateDirectory(&Path[0], NULL)) {
        return ReportFailure(hInstall, "CreateDir", GetLastError());
      }
    }

    // Create file
    hFile = CreateFile((LPCWSTR)Path.c_str(), READ_CONTROL | WRITE_DAC, 0, nullptr, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
    if (hFile == INVALID_HANDLE_VALUE) {
      return ReportFailure(hInstall, "CreateFile", GetLastError());
    }

    try {
      try {
        GrantPermissionToAllApplicationPackages(hFile, GENERIC_READ | GENERIC_EXECUTE, SE_FILE_OBJECT);
      } catch (EOSError &E) {
        return ReportFailure(hInstall, "GrantPermission", E.ErrorCode);
      }

      return ERROR_SUCCESS;

    } finally {
      if (!CloseHandle(hFile)) {
        return ReportFailure(hInstall, "CloseHandle", GetLastError());
      }
    }
  } catch (Exception &E) {
    MsiSetProperty(
        hInstall, "EnginePostInstall_Error", (LPCWSTR)("Keyman Engine failed to set permissions on shared data, " + E.what()));
    // We don't have an error value so we'll just return !success
    return ERROR_INVALID_FUNCTION;
  }
}

unsigned int PreUninstall() {
  return 1;
}
