(*
  Name:             UCreateProcessAsShellUser
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      10 Dec 2010

  Modified Date:    3 Aug 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          10 Dec 2010 - mcdurdin - I2361 - Keyman starts in elevated context incorrectly after setup
                    22 Feb 2011 - mcdurdin - I2757 - Installer does not wait for kmshell actions to complete before continuing
                    18 May 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    03 Aug 2014 - mcdurdin - I4318 - Alt+LeftShift hotkey not set on clean install [med]
*)
unit UCreateProcessAsShellUser;

interface

function CreateProcessAsShellUser(const process, cmdline: WideString; Wait: Boolean): Boolean;  // I2757

implementation

uses
  Windows;

function GetShellWindow: HWND; stdcall; external user32;

type
  TCreateProcessWithTokenW = function(hToken: THANDLE; dwLogonFlags: DWORD; lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR;
  dwCreationFlags: DWORD; lpEnvironment: Pointer; lpCurrentDirectory: LPCWSTR; lpStartupInfo: PSTARTUPINFOW;
  lpProcessInformation: PPROCESSINFORMATION): BOOL; stdcall;

var
  CreateProcessWithTokenW: TCreateProcessWithTokenW = nil;

function CreateProcessAsShellUser(const process, cmdline: WideString; Wait: Boolean): Boolean;  // I2757
var
  dwProcessId: Cardinal;
  hProcess, hShellProcessToken: THandle;
  hDesktopWindow: THandle;
  si: TStartupInfoW;
  pi: TProcessInformation;
  hPrimaryToken: NativeUInt;  // I3309
  hCurrentThreadToken: THandle;
  tkp: TTokenPrivileges;
  retlen: Cardinal;

  procedure DoWait;
  var
    Msg: TMsg;
    Waiting: Boolean;
  begin
    Waiting := True;
    while Waiting do
    begin
      case MsgWaitForMultipleObjects(1, pi.hProcess, FALSE, INFINITE, QS_ALLINPUT) of
        WAIT_OBJECT_0 + 1:
          while PeekMessageW(Msg, 0, 0, 0, PM_REMOVE) do
          begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
        WAIT_OBJECT_0, WAIT_ABANDONED_0:
          Waiting := False;
        else
          Waiting := False;
      end;
    end;
  end;
const
  SE_INCREASE_QUOTA_NAME = 'SeIncreaseQuotaPrivilege';
  TOKEN_ADJUST_SESSIONID = $100;
begin
  Result := False;

  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);

  FillChar(pi, SizeOf(pi), 0);

  if not Assigned(CreateProcessWithTokenW) then Exit;

  hDesktopWindow := GetShellWindow;
  if hDesktopWindow = 0 then Exit;
  if GetWindowThreadProcessId(hDesktopWindow, dwProcessId) = 0 then Exit;

  if not ImpersonateSelf(SecurityImpersonation) then Exit;
  try
    if not LookupPrivilegeValue(nil, SE_INCREASE_QUOTA_NAME, tkp.Privileges[0].Luid) then Exit;

    tkp.PrivilegeCount := 1;  // one privilege to set
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

    if not OpenThreadToken(GetCurrentThread, TOKEN_ADJUST_PRIVILEGES, False, hCurrentThreadToken) then Exit;
    try
      if not AdjustTokenPrivileges(hCurrentThreadToken, False, tkp, 0, nil, retlen) then Exit;

      hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, dwProcessId);
      if hProcess = 0 then Exit;
      try
        if not OpenProcessToken(hProcess, TOKEN_DUPLICATE, hShellProcessToken) then Exit;
        try
          if not DuplicateTokenEx(hShellProcessToken, TOKEN_QUERY or TOKEN_ASSIGN_PRIMARY or TOKEN_DUPLICATE or TOKEN_ADJUST_DEFAULT or TOKEN_ADJUST_SESSIONID,
            nil, SecurityImpersonation, TokenPrimary, hPrimaryToken) then Exit;
          try
            if not CreateProcessWithTokenW(hPrimaryToken, 0, PWideChar(process), PWideChar(cmdline), NORMAL_PRIORITY_CLASS, nil, nil, @si, @pi) then Exit;
          finally
            CloseHandle(hPrimaryToken);
          end;
        finally
          CloseHandle(hShellProcessToken);
        end;
      finally
        CloseHandle(hProcess);
      end;
    finally
      CloseHandle(hCurrentThreadToken);
    end;
  finally
    RevertToSelf;
  end;

  Result := True;

  if Wait then  // I2757   // I4318
    DoWait;

  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);

end;

var
  hAdvApi32: THandle = 0;
initialization
  hAdvApi32 := LoadLibrary(advapi32);
  if hAdvApi32 <> 0 then
    CreateProcessWithTokenW := GetProcAddress(hAdvApi32, 'CreateProcessWithTokenW');
finalization
  if hAdvApi32 <> 0 then
    FreeLibrary(hAdvApi32);
end.
