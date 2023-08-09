#include "insthelper.h"
#include "pch.h"  // use stdafx.h in Visual Studio 2017 and earlier
#include <limits.h>
#include <utility>
#include <iostream>
#include <msiquery.h>

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

std::wstring GetFolderPath(REFGUID folderId) {
  LPWSTR folderPath;
  if (SUCCEEDED(SHGetKnownFolderPath(folderId, 0, NULL, &folderPath))) {
    std::wstring path(folderPath);
    CoTaskMemFree(folderPath);
    return path;
  }
  return L"";
}

unsigned int ReportFailure(MSIHANDLE hInstall, std::string func, unsigned int code) {
  MsiSetProperty(
      hInstall, "EnginePostInstall_Error",
      (LPCWSTR)("Keyman Engine failed to set permissions on shared data in " + func + ": " + SysErrorMessage(code)));
}

unsigned int EnginePostInstall(MSIHANDLE hInstall) {
  HANDLE hFile;
  std::string Path;

  try {
    Path = GetFolderPath(CSIDL_COMMON_APPDATA) + SFolderKeymanRoot;
    if (!DirectoryExists(Path)) {
      if (!CreateDir(Path)) {
        return ReportFailure(hInstall, "CreateDir", GetLastError());
      }
    }

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
