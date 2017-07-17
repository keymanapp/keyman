(*
  Name:             isadmin
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
*)
unit isadmin;

interface

uses baseerror;

type
  EIsAdminError = class(EKeymanBaseError);

const
  E_IsAdmin_NotInstalledCorrectly = $01;

function IsAdministrator: Boolean;

implementation

uses Winapi.Shlobj, Windows, ErrorControlledRegistry, RegistryKeys, KeymanPaths, SysUtils, utilsystem; //, utilkeyman;

{-------------------------------------------------------------------------------
 - IsAdministrator                                                             -
 ------------------------------------------------------------------------------}

{$R-}

var
  FIsAdministrator: Boolean = False;
  FIsAdministrator_Init: Boolean = False;

function IsAdministrator_Groups: Boolean; forward;
function IsAdministrator_SecurityPriv: Boolean; forward;
function IsAdministrator_AccessPriv: Boolean; forward;

function IsAdministrator: Boolean;
begin
  if FIsAdministrator_Init then
  begin
    Result := FIsAdministrator;
    Exit;
  end;

  if GetVersion and $80000000 = $80000000
    then Result := True
    else Result := IsAdministrator_AccessPriv;

  FIsAdministrator := Result;
  FIsAdministrator_Init := True;
end;

function IsAdministrator_AccessPriv: Boolean;
var
  s: string;
  hFile: THandle;
begin
  Result := False;

  { Check registry access permissions - both system\ccs\... and Keyman keys }

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKeyReadOnly(SRegKey_KeymanEngine) then
      raise EIsAdminError.Create(E_IsAdmin_NotInstalledCorrectly, 'Keyman does not appear to be installed correctly: '+
        'the Keyman registry settings are missing.  Please reinstall Keyman.');
  finally
    Free;
  end;

  s := TKeymanPaths.KeyboardsInstallPath;

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKey(SRegKey_KeyboardLayouts, False) then Exit;
    CloseKey;
    if not OpenKey(SRegKey_KeymanEngine, False) then Exit;  //!!!
  finally
    Free;
  end;

  { Get the Keyman keyboard install path }
  s := TKeymanPaths.KeyboardsInstallDir;
  if not DirectoryExists(s) then
    { Don't want to create any directories, so check for write access to CSIDL_COMMON_APPDATA instead }
    s := ExtractFileDir(IncludeTrailingPathDelimiter(GetFolderPath(CSIDL_COMMON_APPDATA)));

  { Check for write access to this path }

  if FileExists(PChar(s+'\kmcheck.tmp')) then DeleteFile(PChar(s+'\kmcheck.tmp'));

  hFile := CreateFile(PChar(s+'\kmcheck.tmp'), GENERIC_READ or GENERIC_WRITE, 0,
    nil, CREATE_ALWAYS, FILE_FLAG_DELETE_ON_CLOSE, 0);
  if hFile = INVALID_HANDLE_VALUE then Exit;
  CloseHandle(hFile);

  Result := True;
end;


function IsAdministrator_SecurityPriv: Boolean;
var
  priv: TTokenPrivileges;
  luid: TLargeInteger;
  hAccessToken: THandle;
  dw: DWord;
const
  SE_SECURITY_NAME = 'SeSecurityPrivilege';
begin
  Result := True;
  if (GetVersion and $80000000) = $80000000 then Exit;    // Win95/98, not NT

  Result := False;
  if not OpenThreadToken(GetCurrentThread(), TOKEN_ADJUST_PRIVILEGES, TRUE, hAccessToken) then
  begin
    if GetLastError <> ERROR_NO_TOKEN then Exit;
    if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES, hAccessToken) then Exit;
  end;
  try
    if LookupPrivilegeValue(nil, SE_SECURITY_NAME, luid) then
    begin
      priv.PrivilegeCount := 1;
      priv.Privileges[0].luid := luid;
      priv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      if AdjustTokenPrivileges(hAccessToken, FALSE, priv, 0, nil, dw) then
      begin
        if GetLastError = ERROR_NOT_ALL_ASSIGNED then Exit;
        Result := True;
      end;
    end;
  finally
    CloseHandle(hAccessToken);
  end;
end;

function IsAdministrator_Groups: Boolean;
const SECURITY_NT_AUTHORITY: SID_IDENTIFIER_AUTHORITY = (Value:(0,0,0,0,0,5));
const SECURITY_BUILTIN_DOMAIN_RID: DWord = $00000020;
const DOMAIN_ALIAS_RID_ADMINS: DWord = $00000220;
var
  hAccessToken: THandle;
  InfoBuffer: PChar;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWord;
  psidAdministrators: PSID;
  siaNtAuthority: SID_IDENTIFIER_AUTHORITY;
  x: UINT;
  bSuccess: Boolean;
begin
  Result := True;
  if (GetVersion and $80000000) = $80000000 then Exit;    // Win95/98, not NT

  Result := False;

  InfoBuffer := AllocMem(1024);
  try
    ptgGroups := PTokenGroups(InfoBuffer);
    siaNtAuthority := SECURITY_NT_AUTHORITY;

    if not OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, TRUE, hAccessToken) then
    begin
      if GetLastError <> ERROR_NO_TOKEN then Exit;
      //
      // retry against process token if no thread token exists
      //
      if not OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, hAccessToken) then Exit;
    end;

    bSuccess := GetTokenInformation(hAccessToken, TokenGroups, InfoBuffer, 1024, dwInfoBufferSize);
    if not bSuccess and (dwInfoBufferSize > 1024) then
    begin
      FreeMem(InfoBuffer);
      InfoBuffer := AllocMem(dwInfoBufferSize);
      ptgGroups := PTokenGroups(InfoBuffer);
      bSuccess := GetTokenInformation(hAccessToken, TokenGroups, InfoBuffer, dwInfoBufferSize, dwInfoBufferSize);
    end;

    CloseHandle(hAccessToken);
      
    if not bSuccess then
    begin
      Exit;
    end;

    if not AllocateAndInitializeSid(siaNtAuthority, 2, SECURITY_BUILTIN_DOMAIN_RID,
       DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators) then Exit;

    // assume that we don't find the admin SID.
    bSuccess := FALSE;

    for x := 0 to ptgGroups.GroupCount - 1 do
      if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
      begin
        bSuccess := True;
        Break;
      end;

    FreeSid(psidAdministrators);
    Result := bSuccess;
  finally
    FreeMem(InfoBuffer);
  end;
end;

end.
 
