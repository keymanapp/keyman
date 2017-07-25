(*
  Name:             ShellUserRegistry
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Feb 2011

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Feb 2011 - mcdurdin - I2748 - Install backs up user registry for Admin user, not shell user
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit ShellUserRegistry;

interface

uses
  Windows,
  ErrorControlledRegistry;

type
  _PROFILEINFO = record
    dwSize: DWORD;
    dwFlags: DWORD;
    lpUserName: LPTSTR;
    lpProfilePath: LPTSTR;
    lpDefaultPath: LPTSTR;
    lpServerName: LPTSTR;
    lpPolicyPath: LPTSTR;
    hProfile: THandle;
  end;

type
  TShellUserRegistry = class(TRegistryErrorControlled)  // I2890
  private
    FProfile: _PROFILEINFO;
    hProcessToken: THandle;
    procedure OpenProfile;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysConst,
  SysUtils;

function GetShellWindow: HWND; stdcall; external user32;

type
  PROFILEINFO = _PROFILEINFO;
  LPPROFILEINFO = ^_PROFILEINFO;

const
  PI_NOUI = $00000001;

function LoadUserProfile(hToken: THandle; var lpProfileInfo: PROFILEINFO): BOOL; stdcall; external 'userenv.dll' name 'LoadUserProfileA';
function UnloadUserProfile(hToken, hProfile: THandle): BOOL; stdcall; external 'userenv.dll';

procedure RaiseLastError(func: string; LastError: Integer); overload;
var
  Error: EOSError;
begin
  Error := EOSError.CreateResFmt(@SOSError, [LastError, func + ': ' + SysErrorMessage(LastError)]);
  Error.ErrorCode := LastError;
  raise Error;
end;

procedure RaiseLastError(func: string); overload;
begin
  RaiseLastError(func, GetLastError);
end;


procedure CheckResult(func: string; v: Integer);
begin
  if v <> ERROR_SUCCESS then
    RaiseLastError(func, v);
end;

{ TShellUserRegistry }

constructor TShellUserRegistry.Create;
begin
  inherited Create;
  OpenProfile;
  RootKey := FProfile.hProfile;
end;

destructor TShellUserRegistry.Destroy;
begin
  inherited Destroy;
  if FProfile.hProfile <> 0 then
    if not UnloadUserProfile(hProcessToken, FProfile.hProfile) then RaiseLastError('UnloadUserProfile');
  if hProcessToken <> 0 then
    if not CloseHandle(hProcessToken) then RaiseLastError('CloseHandle(hProcessToken)');
end;

procedure TShellUserRegistry.OpenProfile;
var
  hDesktopWindow: THandle;
  dwProcessId: DWord;
  hProcess: Cardinal;
begin
  FillChar(FProfile, sizeof(FProfile), 0);

  hDesktopWindow := GetShellWindow;
  if hDesktopWindow = 0 then
    RaiseLastError('GetShellWindow');

  if GetWindowThreadProcessId(hDesktopWindow, dwProcessId) = 0 then
    RaiseLastError('GetWindowThreadProcessId');

  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, dwProcessId);
  if hProcess = 0 then
    RaiseLastError('OpenProcess');

  try
    if not OpenProcessToken(hProcess, TOKEN_QUERY or TOKEN_DUPLICATE or TOKEN_IMPERSONATE, hProcessToken) then
      RaiseLastError('OpenProcessToken');

    FProfile.dwSize := sizeof(FProfile);
    FProfile.lpUserName := '__temp__keyman__logonuser';
    FProfile.dwFlags := PI_NOUI;
    if not LoadUserProfile(hProcessToken, FProfile) then
      RaiseLastError('LoadUserProfile');
  finally
    if not CloseHandle(hProcess) then RaiseLastError('CloseHandle(hProcess)');
  end;
end;

end.
