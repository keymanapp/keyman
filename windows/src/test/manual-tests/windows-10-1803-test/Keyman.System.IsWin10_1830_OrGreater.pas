unit Keyman.System.IsWin10_1830_OrGreater;

interface

uses
  Winapi.Windows;

function IsWindows10_1803_OrGreater: BOOL; inline;
procedure WriteOSVersionInfo;

implementation

uses
  System.SysUtils;

const
  WINDOWS_10_MAJORVERSION = 10;
  WINDOWS_10_MINORVERSION = 0;
  WINDOWS_10_BUILDNUMBER = 17134;

function IsWindows10_1803_OrGreater: BOOL; inline;
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
         VER_BUILDNUMBER, VER_GREATER_EQUAL);

  osvi.dwMajorVersion := WINDOWS_10_MAJORVERSION;
  osvi.dwMinorVersion := WINDOWS_10_MINORVERSION;
  osvi.dwBuildNumber := WINDOWS_10_BUILDNUMBER;

  Result := VerifyVersionInfoW(osvi, VER_MAJORVERSION or VER_MINORVERSION or VER_BUILDNUMBER, dwlConditionMask) <> False;
end;

procedure WriteOSVersionInfo;
var
  osvi: OSVERSIONINFOEXW;
begin
  FillChar(osvi, SizeOf(osvi), 0);
  osvi.dwOSVersionInfoSize := SizeOf(osvi);
  GetVersionEx(osvi);
  with osvi do writeln(Format(
    'dwOSVersionInfoSize: %d'#13#10+
    'dwMajorVersion: %d'#13#10+
    'dwMinorVersion: %d'#13#10+
    'dwBuildNumber: %d'#13#10+
    'dwPlatformId: %d'#13#10+
    'szCSDVersion: %s'#13#10+
    'wServicePackMajor: %d'#13#10+
    'wServicePackMinor: %d'#13#10+
    'wSuiteMask: %d'#13#10+
    'wProductType: %d'#13#10+
    'wReserved: %d', [
    dwOSVersionInfoSize,
    dwMajorVersion,
    dwMinorVersion,
    dwBuildNumber,
    dwPlatformId,
    szCSDVersion,
    wServicePackMajor,
    wServicePackMinor,
    wSuiteMask,
    wProductType,
    wReserved]));
end;

end.
