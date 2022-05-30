program CreateProcessAsUser;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  SysUtils;

var lpStartupInfo: STARTUPINFOW;
    lpProcessInformation: PROCESS_INFORMATION;
begin
  { TODO -oUser -cConsole Main : Hier Code einfügen }
  if not CreateProcessWithLogonW(
    'TestBenutzer',//lpUsername,
    '',//lpDomain,
    'testbenutzer',//lpPassword: LPCWSTR;
    LOGON_WITH_PROFILE,//dwLogonFlags: DWORD;
    'C:\Windows\System32\cmd.exe',//lpApplicationName: LPCWSTR;
    nil,//lpCommandLine: LPWSTR;
    CREATE_NEW_CONSOLE,//dwCreationFlags: DWORD;
    nil,//lpEnvironment: LPVOID;
    nil,//lpCurrentDirectory: LPCWSTR;
    lpStartupInfo,//const lpStartupInfo: STARTUPINFOW;
    lpProcessInformation//var lpProcessInformation: PROCESS_INFORMATION
  ) then
    RaiseLastOSError;

end.
