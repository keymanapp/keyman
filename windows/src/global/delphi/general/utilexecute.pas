(*
  Name:             utilexecute
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Jan 2013

  Modified Date:    23 Feb 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Jan 2013 - mcdurdin - I3631 - V9.0 - if svn commit fails, then cvscommit app needs to be aware of this.
                    16 Apr 2014 - mcdurdin - I4170 - V9.0 - Console execute in utilexecute.pas needs a temp copy of buffer to avoid write access violations
                    23 Feb 2016 - mcdurdin - I4983 - Starting a subprocess can fail due to constant buffer for CreateProcessW
*)
unit utilexecute;

interface

uses
  WinApi.ShellApi,
  WinApi.Windows;

type
  TUtilExecuteCallbackEvent = procedure(var Cancelled: Boolean) of object;
  TUtilExecuteCallbackWaitEvent = procedure(hProcess: THandle; var Waiting, Cancelled: Boolean) of object;

  TUtilExecute = class sealed
    class function Console(const cmdline, curdir: string; var FLogText: string; var ExitCode: Integer; FOnCallback: TUtilExecuteCallbackEvent = nil): Boolean; overload; static;   // I3631
    class function Console(const cmdline, curdir: string; var FLogText: string; FOnCallback: TUtilExecuteCallbackEvent = nil): Boolean; overload; static;   // I3631
    class function WaitForProcess(const cmdline, curdir: string; ShowWindow: Integer = SW_SHOWNORMAL; FOnCallback: TUtilExecuteCallbackWaitEvent = nil): Boolean; overload; static;
    class function WaitForProcess(const cmdline, curdir: string; var EC: Cardinal; ShowWindow: Integer = SW_SHOWNORMAL; FOnCallback: TUtilExecuteCallbackWaitEvent = nil): Boolean; overload; static;
    class function Shell(Handle: HWND; const process, curdir: string; const parameters: string = ''; ShowWindow: Integer = SW_SHOWNORMAL; const Verb: string = 'open'): Boolean; static;
    class function ShellCurrentUser(Handle: HWND; const process, curdir: string; const parameters: string = ''; ShowWindow: Integer = SW_SHOWNORMAL; const Verb: string = 'open'): Boolean; static;
    class function URL(const url: string): Boolean;

    class function CreateProcessAsShellUser(const process, cmdline: WideString; Wait: Boolean): Boolean; overload;
    class function CreateProcessAsShellUser(const process, cmdline: WideString; Wait: Boolean; var AExitCode: Cardinal): Boolean; overload;
  end;

implementation

uses
  System.SysUtils,

  Unicode;

class function TUtilExecute.Console(const cmdline, curdir: string; var FLogText: string; FOnCallback: TUtilExecuteCallbackEvent = nil): Boolean;
var
  ec: Integer;
begin
  Result := TUtilExecute.Console(cmdline, curdir, FLogText, ec, FOnCallback);   // I3631
end;

class function TUtilExecute.Console(const cmdline, curdir: string; var FLogText: string; var ExitCode: Integer; FOnCallback: TUtilExecuteCallbackEvent = nil): Boolean;   // I3631
var
  si: TStartupInfo;
  b, ec: DWord;
  cmdlinebuf: string;
  buf: array[0..512] of ansichar;  // I3310
  SecAttrs: TSecurityAttributes;
  hsoutread, hsoutwrite: THandle;
  hsinread, hsinwrite: THandle;
  pi: TProcessInformation;
  n: Integer;
  FCancelled: Boolean;
begin
  Result := False;
  FillChar(SecAttrs, SizeOf(SecAttrs), #0);
  SecAttrs.nLength              := SizeOf(SecAttrs);
  SecAttrs.lpSecurityDescriptor := nil;
  SecAttrs.bInheritHandle       := TRUE;
  if not CreatePipe(hsoutread, hsoutwrite, @SecAttrs, 0) then Exit;

  FillChar(SecAttrs, SizeOf(SecAttrs), #0);
  SecAttrs.nLength              := SizeOf(SecAttrs);
  SecAttrs.lpSecurityDescriptor := nil;
  SecAttrs.bInheritHandle       := TRUE;
  if not CreatePipe(hsinread, hsinwrite, @SecAttrs, 0) then
  begin
    CloseHandle(hsoutread);
    CloseHandle(hsoutwrite);
    Exit;
  end;

  { See support.microsoft.com kb 190351 }

  n := 0;
  FLogText := '';

  SetLength(cmdlinebuf, Length(cmdline));   // I4170
  StrCopy(PChar(cmdlinebuf), PChar(cmdline));   // I4170

  //Sreen.Cursor := crHourglass;
  try
    si.cb := SizeOf(TStartupInfo);
    si.lpReserved := nil;
    si.lpDesktop := nil;
    si.lpTitle := nil;
    si.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    si.wShowWindow := SW_HIDE;
    si.cbReserved2 := 0;
    si.lpReserved2 := nil;
    si.hStdInput := hsinread;
    si.hStdOutput := hsoutwrite;
    si.hStdError := hsoutwrite;

    if CreateProcess(nil, PChar(cmdlinebuf),   // I4170
      nil, nil, True, NORMAL_PRIORITY_CLASS, nil, PChar(curdir), si, pi) then
    try
      if GetExitCodeProcess(pi.hProcess, ec) then
      begin
        while ec = STILL_ACTIVE do
        begin
          Sleep(20);
          Inc(n);
          if n = 30 then
          begin
            if Assigned(FOnCallback) then
            begin
              FCancelled := False;
              FOnCallback(FCancelled);
              if FCancelled then
              begin
                SetLastError(ERROR_CANCELLED);
                Exit;
              end;
            end;
            n := 0;
          end;
          PeekNamedPipe(hsoutread, nil, 0, nil, @b, nil);
          if b > 0 then
          begin
            ReadFile(hsoutread, buf, High(buf), b, nil);
            FLogText := FLogText + String_AtoU(Copy(buf, 1, b));
          end;
          if not GetExitCodeProcess(pi.hProcess, ec) then ec := 0;
        end;
      end;

      repeat
        PeekNamedPipe(hsoutread, nil, 0, nil, @b, nil);
        if b > 0 then
        begin
          ReadFile(hsoutread, buf, High(buf), b, nil);
          FLogText := FLogText + String_AtoU(Copy(buf, 1, b));
        end;
      until b = 0;

      ExitCode := ec;   // I3631

      Result := True;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  finally
    CloseHandle(hsoutread);
    CloseHandle(hsoutwrite);
    CloseHandle(hsinread);
    CloseHandle(hsinwrite);
    //Screen.Cursor := crDefault;
  end;
end;

class function TUtilExecute.Shell(Handle: HWND; const process, curdir, parameters: string;
  ShowWindow: Integer; const Verb: string): Boolean;
begin
  Result := ShellExecute(Handle, PChar(Verb), Pchar(process), PChar(parameters), Pchar(curdir), ShowWindow) > 32;
end;

class function TUtilExecute.ShellCurrentUser(Handle: HWND; const process,
  curdir, parameters: string; ShowWindow: Integer; const Verb: string): Boolean;
begin
  // Use mysterious ways to get the shell process to launch. There are at least
  // three different ways to do this. The way we are using has been used by the
  // bootstrap for years so pretty sure it's robust!
  // Alternative: https://devblogs.microsoft.com/oldnewthing/20131118-00/?p=2643
  // Alternative: https://devblogs.microsoft.com/oldnewthing/20190425-00/?p=102443
  Result := CreateProcessAsShellUser(process, '"'+process+'" '+parameters, False);
end;

class function TUtilExecute.URL(const url: string): Boolean;
begin
  Result := ShellExecute(GetActiveWindow, nil, PChar(url), nil, nil, SW_SHOW) >= 32;
end;

class function TUtilExecute.WaitForProcess(const cmdline, curdir: string; ShowWindow: Integer; FOnCallback: TUtilExecuteCallbackWaitEvent): Boolean;
var
  ec: Cardinal;
begin
  Result := WaitForProcess(cmdline, curdir, ec, ShowWindow, FOnCallback);
end;

class function TUtilExecute.WaitForProcess(const cmdline, curdir: string; var EC: Cardinal; ShowWindow: Integer; FOnCallback: TUtilExecuteCallbackWaitEvent): Boolean;
var
  Waiting: Boolean;
  si: TStartupInfoW;
  pi: TProcessInformation;
  Cancelled: Boolean;
  buf: PChar;
begin
  Result := False;

  Waiting := True;
  si.cb := SizeOf(TStartupInfo);
  si.lpReserved := nil;
  si.lpDesktop := nil;
  si.lpTitle := nil;
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := ShowWindow;
  si.cbReserved2 := 0;
  si.lpReserved2 := nil;

  EC := $FFFFFFFF;

  buf := AllocMem((Length(cmdline)+1)*sizeof(Char));   // I4983
  try
    StrPCopy(buf, cmdline);   // I4983
    if CreateProcess(nil, buf,   // I4983
            nil, nil, True, NORMAL_PRIORITY_CLASS, nil, PWideChar(curdir), si, pi) then
    try
      while Waiting do
      begin
        if Assigned(FOnCallback) then
        begin
          Cancelled := False;
          FOnCallback(pi.hProcess, Waiting, Cancelled);
          if Cancelled then
          begin
            SetLastError(ERROR_CANCELLED);
            Exit;
          end;
        end
        else
        begin
          WaitForSingleObject(pi.hProcess, INFINITE);
          Waiting := False;
        end;
      end;

      if not GetExitCodeProcess(pi.hProcess, EC) then
        EC := $FFFFFFFF;

      Result := True;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  finally
    FreeMem(buf);   // I4983
  end;
end;

// Refactored from UCreateProcessAsShellUser

function GetShellWindow: HWND; stdcall; external user32;

type
  TCreateProcessWithTokenW = function(hToken: THANDLE; dwLogonFlags: DWORD; lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR;
    dwCreationFlags: DWORD; lpEnvironment: Pointer; lpCurrentDirectory: LPCWSTR; lpStartupInfo: PSTARTUPINFOW;
    lpProcessInformation: PPROCESSINFORMATION): BOOL; stdcall;

var
  CreateProcessWithTokenW: TCreateProcessWithTokenW = nil;

class function TUtilExecute.CreateProcessAsShellUser(const process, cmdline: WideString; Wait: Boolean): Boolean;  // I2757
var
  ec: Cardinal;
begin
  Result := CreateProcessAsShellUser(process, cmdline, Wait, ec);
end;

class function TUtilExecute.CreateProcessAsShellUser(const process, cmdline: WideString; Wait: Boolean; var AExitCode: Cardinal): Boolean;  // I2757
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
  begin
    DoWait;
    GetExitCodeProcess(pi.hProcess, AExitCode);
  end;

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
