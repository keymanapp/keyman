//
// Keyman.Winapi.VersionHelpers: Translation of Windows API header
// versionhelpers.h. Replaces GetOsVersion.pas. This module defines
// helper functions to promote version check with proper comparisons.
//
// (C) 2018 SIL International
//
// License: MIT
//
unit Keyman.Winapi.VersionHelpers;

interface

uses
  Winapi.Windows;

function IsWindowsVersionOrGreater(wMajorVersion, wMinorVersion, wServicePackMajor: WORD): BOOL; inline;
function IsWindowsXPOrGreater: BOOL; inline;
function IsWindowsXPSP1OrGreater: BOOL; inline;
function IsWindowsXPSP2OrGreater: BOOL; inline;
function IsWindowsXPSP3OrGreater: BOOL; inline;
function IsWindowsVistaOrGreater: BOOL; inline;
function IsWindowsVistaSP1OrGreater: BOOL; inline;
function IsWindowsVistaSP2OrGreater: BOOL; inline;
function IsWindows7OrGreater: BOOL; inline;
function IsWindows7SP1OrGreater: BOOL; inline;
function IsWindows8OrGreater: BOOL; inline;
function IsWindows8Point1OrGreater: BOOL; inline;
function IsWindowsThresholdOrGreater: BOOL; inline;
function IsWindows10OrGreater: BOOL; inline;
function IsWindowsServer: BOOL; inline;

const
  _WIN32_WINNT_WINTHRESHOLD =         $0A00; // ABRACADABRA_THRESHOLD
  {$EXTERNALSYM _WIN32_WINNT_WINTHRESHOLD}
  _WIN32_WINNT_WIN10 =                $0A00; // ABRACADABRA_THRESHOLD
  {$EXTERNALSYM _WIN32_WINNT_WIN10}

implementation

function IsWindowsVersionOrGreater(wMajorVersion, wMinorVersion, wServicePackMajor: WORD): BOOL; inline;
var
  osvi: OSVERSIONINFOEXW;
  dwlConditionMask: DWORDLONG;
begin
  FillChar(osvi, SizeOf(osvi), 0);
  osvi.dwOSVersionInfoSize := SizeOf(osvi);
  dwlConditionMask := VerSetConditionMask(
    VerSetConditionMask(
    VerSetConditionMask(
      0, VER_MAJORVERSION, VER_GREATER_EQUAL),
         VER_MINORVERSION, VER_GREATER_EQUAL),
         VER_SERVICEPACKMAJOR, VER_GREATER_EQUAL);

  osvi.dwMajorVersion := wMajorVersion;
  osvi.dwMinorVersion := wMinorVersion;
  osvi.wServicePackMajor := wServicePackMajor;

  Result := VerifyVersionInfoW(osvi, VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR, dwlConditionMask) <> False;
end;

function IsWindowsXPOrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINXP), LOBYTE(_WIN32_WINNT_WINXP), 0);
end;

function IsWindowsXPSP1OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINXP), LOBYTE(_WIN32_WINNT_WINXP), 1);
end;

function IsWindowsXPSP2OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINXP), LOBYTE(_WIN32_WINNT_WINXP), 2);
end;

function IsWindowsXPSP3OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINXP), LOBYTE(_WIN32_WINNT_WINXP), 3);
end;

function IsWindowsVistaOrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_VISTA), LOBYTE(_WIN32_WINNT_VISTA), 0);
end;

function IsWindowsVistaSP1OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_VISTA), LOBYTE(_WIN32_WINNT_VISTA), 1);
end;

function IsWindowsVistaSP2OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_VISTA), LOBYTE(_WIN32_WINNT_VISTA), 2);
end;

function IsWindows7OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WIN7), LOBYTE(_WIN32_WINNT_WIN7), 0);
end;

function IsWindows7SP1OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WIN7), LOBYTE(_WIN32_WINNT_WIN7), 1);
end;

function IsWindows8OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WIN8), LOBYTE(_WIN32_WINNT_WIN8), 0);
end;

function IsWindows8Point1OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINBLUE), LOBYTE(_WIN32_WINNT_WINBLUE), 0);
end;

function IsWindowsThresholdOrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINTHRESHOLD), LOBYTE(_WIN32_WINNT_WINTHRESHOLD), 0);
end;

function IsWindows10OrGreater: BOOL; inline;
begin
  Result := IsWindowsVersionOrGreater(HIBYTE(_WIN32_WINNT_WINTHRESHOLD), LOBYTE(_WIN32_WINNT_WINTHRESHOLD), 0);
end;

function IsWindowsServer: BOOL; inline;
var
  osvi: OSVERSIONINFOEXW;
  dwlConditionMask: DWORDLONG;
begin
  FillChar(osvi, SizeOf(osvi), 0);
  osvi.dwOSVersionInfoSize := SizeOf(osvi);
  osvi.wProductType := VER_NT_WORKSTATION;
  dwlConditionMask := VerSetConditionMask(0, VER_PRODUCT_TYPE, VER_EQUAL);

  Result := not VerifyVersionInfoW(&osvi, VER_PRODUCT_TYPE, dwlConditionMask);
end;

end.
