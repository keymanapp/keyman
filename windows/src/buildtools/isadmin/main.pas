(*
  Name:             main
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
unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, ComObj, shlobj;

procedure Run;
function IsAdministrator_Groups: string;
function IsAdministrator_SecurityPriv: string;
function IsAdministrator_AccessPriv: string;
function AppendSlash(s: string): string;
function DirectoryExists(const Name: string): Boolean;

implementation

uses
  Winapi.ActiveX,

  ErrorControlledRegistry,
  KeymanPaths,
  RegistryKeys;

{-------------------------------------------------------------------------------
 - IsAdministrator                                                             -
 ------------------------------------------------------------------------------}

{$R-}

function GetFolderPath(csidl: Integer): string;
var
  buf: array[0..260] of Char;
  idl: PItemIDList;
  mm: IMalloc;
begin
  Result := '';
  if SHGetMalloc(mm) = NOERROR then
  begin
    if SHGetSpecialFolderLocation(0, csidl, idl) = NOERROR then
    begin
      if SHGetPathFromIDList(idl, buf) then
      begin
        Result := Buf;
      end;
      mm.Free(idl);
    end;
    mm._Release;
  end;

  if (Result = '') and (csidl = CSIDL_PROGRAM_FILES) then
    with TRegistryErrorControlled.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if not OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion') then  // I2890
        RaiseLastRegistryError;
      Result := ReadString('ProgramFilesDir');
    finally
      Free;
    end;
  if Result <> '' then
    if Result[Length(Result)] <> '\' then Result := Result + '\';
end;


function IsAdministrator_AccessPriv: string;
var
  s: string;
  hFile: THandle;
begin
  if (GetVersion and $80000000) = $80000000 then
  begin
    Result := 'IsAdmin - Win98';
    Exit;    // Win95/98, not NT
  end;

  { Check registry access permissions - both system\ccs\... and Keyman keys }

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKey(SRegKey_KeyboardLayouts_LM, False) then
    begin
      Result := 'NotAdmin - Could not open HKLM\System\CurrentControlSet\Control\keyboard layouts';
      Exit;
    end;

    CloseKey;
    if not OpenKey(SRegKey_Software_LM, False) then
    begin
      Result := 'NotAdmin - Could not open HKLM\Software';
      Exit;
    end;

    { Get the Keyman keyboard install path }

    s := TKeymanPaths.KeyboardsInstallDir;
    if not DirectoryExists(s) then
      { Don't want to create any directories, so check for write access to CSIDL_COMMON_APPDATA instead }
      s := ExtractFileDir(AppendSlash(GetFolderPath(CSIDL_COMMON_APPDATA)));

    { Check for write access to this path }

    if FileExists(PChar(s+'\kmcheck.tmp')) then DeleteFile(PChar(s+'\kmcheck.tmp'));

    hFile := CreateFile(PChar(s+'\kmcheck.tmp'), GENERIC_READ or GENERIC_WRITE, 0,
      nil, CREATE_ALWAYS, FILE_FLAG_DELETE_ON_CLOSE, 0);
    if hFile = INVALID_HANDLE_VALUE then
    begin
      Result := 'NotAdmin - Could not create file in '+s+'.';
      Exit;
    end;
    CloseHandle(hFile);
  finally
    Free;
  end;

  Result := 'IsAdmin - Has write permissions where required';
end;

function accesspriv_details: string;
var
  s: string;
  hFile: THandle;
begin
  { Check registry access permissions - both system\ccs\... and Keyman keys }

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKey(SRegKey_KeyboardLayouts_LM, False)
      then Result := Result + '   1. NotAdmin - Could not open HKLM\System\CurrentControlSet\Control\keyboard layouts'#13#10
      else Result := Result + '   1. IsAdmin  - Opened HKLM\System\CurrentControlSet\Control\keyboard layouts successfully'#13#10;

    CloseKey;
    if not OpenKey(SRegKey_Software_LM, False)
      then Result := Result + '   2. NotAdmin - Could not open HKLM\Software'#13#10
      else Result := Result + '   2. IsAdmin  - Opened HKLM\Software successfully'#13#10;

    { Get the Keyman keyboard install path }

    s := TKeymanPaths.KeyboardsInstallDir;
    if not DirectoryExists(s) then
      { Don't want to create any directories, so check for write access to CSIDL_COMMON_APPDATA instead }
      s := ExtractFileDir(AppendSlash(GetFolderPath(CSIDL_COMMON_APPDATA)));

    { Check for write access to this path }

    if FileExists(PChar(s+'\kmcheck.tmp')) then DeleteFile(PChar(s+'\kmcheck.tmp'));

    hFile := CreateFile(PChar(s+'\kmcheck.tmp'), GENERIC_READ or GENERIC_WRITE, 0,
      nil, CREATE_ALWAYS, FILE_FLAG_DELETE_ON_CLOSE, 0);
    if hFile = INVALID_HANDLE_VALUE
      then Result := Result + '   3. NotAdmin - Could not create file in '+s+'.'
      else Result := Result + '   3. IsAdmin  - Created file in '+s+' successfully.';

    CloseHandle(hFile);
  finally
    Free;
  end;
end;

function IsAdministrator_SecurityPriv: string;
var
  priv: TTokenPrivileges;
  luid: TLargeInteger;
  hAccessToken: THandle;
  dw: DWord;
const
  SE_SECURITY_NAME = 'SeSecurityPrivilege';
begin
  if (GetVersion and $80000000) = $80000000 then
  begin
    Result := 'IsAdmin - Win98';
    Exit;    // Win95/98, not NT
  end;

  if not OpenThreadToken(GetCurrentThread(), TOKEN_ADJUST_PRIVILEGES, TRUE, hAccessToken) then
  begin
    if GetLastError <> ERROR_NO_TOKEN then
    begin
      Result := 'NotAdmin - OpenThreadToken - Error '+IntToStr(GetLastError);
      Exit;
    end;
    if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES, hAccessToken) then
    begin
      Result := 'NotAdmin - OpenProcessToken - Error '+IntToStr(GetLastError);
      Exit;
    end;
  end;
  try
    if LookupPrivilegeValue(nil, SE_SECURITY_NAME, luid) then
    begin
      priv.PrivilegeCount := 1;
      priv.Privileges[0].luid := luid;
      priv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      if AdjustTokenPrivileges(hAccessToken, FALSE, priv, 0, nil, dw) then
      begin
        if GetLastError = ERROR_NOT_ALL_ASSIGNED then
        begin
          Result := 'NotAdmin - AdjustTokenPrivileges -- cannot adjust to SeSecurityPrivilege privilege';
          Exit;
        end;
        Result := 'IsAdmin - Has SeSecurityPrivilege privilege';
      end;
    end;
  finally
    CloseHandle(hAccessToken);
  end;
end;

function IsAdministrator_Groups: string;
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
  err: Integer;
  bSuccess: Boolean;
begin
  if (GetVersion and $80000000) = $80000000 then
  begin
    Result := 'IsAdmin - Win98';
    Exit;    // Win95/98, not NT
  end;

  InfoBuffer := AllocMem(1024);
  try
    ptgGroups := PTokenGroups(InfoBuffer);
    siaNtAuthority := SECURITY_NT_AUTHORITY;

    if not OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, TRUE, hAccessToken) then
    begin
      if GetLastError <> ERROR_NO_TOKEN then
      begin
        Result := 'NotAdmin - OpenThreadToken - Error '+IntToStr(GetLastError);
        Exit;
      end;
      //
      // retry against process token if no thread token exists
      //
      if not OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, hAccessToken) then
      begin
        Result := 'NotAdmin - OpenProcessToken - Error '+IntToStr(GetLastError);
        Exit;
      end;
    end;

    bSuccess := GetTokenInformation(hAccessToken, TokenGroups, InfoBuffer, 1024, dwInfoBufferSize);
    err := GetLastError;
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
      Result := 'NotAdmin - GetTokenInformation -- error = ' + IntToStr(err);
      Exit;
    end;

    if not AllocateAndInitializeSid(siaNtAuthority, 2, SECURITY_BUILTIN_DOMAIN_RID,
       DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators) then
    begin
      Result := 'NotAdmin - Could not AllocateAndInitializeSid for DOMAIN_ALIAS_RID_ADMINS -- error = '+IntToStr(GetLastError);
      Exit;
    end;

    // assume that we don't find the admin SID.
    bSuccess := FALSE;

    for x := 0 to ptgGroups.GroupCount - 1 do
      if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
      begin
        bSuccess := True;
        Break;
      end;

    FreeSid(psidAdministrators);
    if bSuccess
      then Result := 'IsAdmin - Is a member of local Administrator group'
      else Result := 'NotAdmin - Not a member of local Administrator group.';
  finally
    FreeMem(InfoBuffer);
  end;
end;

procedure Run;
begin
  writeln('IsAdministrator_AccessPriv:    '+IsAdministrator_AccessPriv);
  writeln('IsAdministrator_SecurityPriv:  '+IsAdministrator_SecurityPriv);
  writeln('IsAdministrator_Groups:        '+IsAdministrator_Groups);
  writeln;
  writeln('IsAdministrator_AccessPriv details:'#13#10+accesspriv_details);
end;

function AppendSlash(s: string): string;
begin
  if s = '' then Result := ''
  else if s[Length(s)] <> '\' then Result := s + '\'
  else Result := s;
end;

function DirectoryExists(const Name: string): Boolean;
var
  Code: Dword;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> $FFFFFFFF) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

end.
