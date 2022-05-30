{
Description

Project JEDI Windows Security Code Library (JWSCL)

This unit provides access to the Local Security Authority Subsystem that provides function like LSALogonUser to create a logon session.

Author
Christian Wimmer

License
The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU Lesser General Public License (the  "LGPL License"), in which case the
provisions of the LGPL License are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the LGPL License and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting  the provisions above and
replace  them with the notice and other provisions required by the LGPL
License.  If you do not delete the provisions above, a recipient may use
your version of this file under either the MPL or the LGPL License.

For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html

Note
The Original Code is JwsclLSA.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.


}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclLsa;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils,
  jwaWindows,
  JwsclResource,
  JwsclSid, JwsclToken,
  JwsclTypes, JwsclExceptions,
  JwsclVersion, JwsclConstants, 
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}


type
  {<B>TJwSecurityLsa</B> is the main entry class for LSA calls.}
  TJwSecurityLsa = class
  protected
    fLsaHandle: THandle;
  public
    {<B>Create</B> creates a new instance of TJwSecurityLsa and
	registers the current process as a logon process.
	This call needs TCB privilege!
   @param LogonProcessName This parameter receives a name in ansi format that
     does not exceed 127 characters.

	@raises
	  EJwsclWinCallFailedException: This exception will be raised
	    if the call to LsaRegisterLogonProcess failed.
	}
    constructor Create(const LogonProcessName: AnsiString);

    {<B>CreateUntrusted</B> creates a new instance of TJwSecurityLsa and
	 creates an untrusted connection to LSA 
	 
	 @raises
	  EJwsclWinCallFailedException: This exception will be raised
	    if the call to LsaConnectUntrusted failed.
	}	
  	constructor CreateUntrusted;

    destructor Destroy; override;

    {<B>LsaLogonUser</B> creates a new authentication token where
	even token groups can be adapted.
	
	@param anAuthenticationInformation Use JwCreate_MSV1_0_INTERACTIVE_LOGON for 
		interactive logondata)

	}
    procedure LsaLogonUser(
      const anOriginName: AnsiString;
      aLogonType: SECURITY_LOGON_TYPE;
      const anAuthenticationPackageName: AnsiString;
      anAuthenticationInformation: Pointer;
      anAuthenticationInformationLength: Cardinal;
      aLocalGroups: TJwSecurityIdList;
      aSourceContext: TTokenSource;
      out aProfileBuffer: Pointer;
      out aProfileBufferLength: Cardinal;
      out aTokenLuid: TLUID;
      out aToken: TJwSecurityToken;
      out aQuotaLimits: QUOTA_LIMITS;
      out SubStatus: NTSTATUS);


    property LsaHandle: Cardinal Read fLsaHandle;

  end;


  TJwAccountRightStringW = TJwWideStringArray;
  TJwEnumerationInformation = array of TJwSecurityId;

  {<B>TJwLsaPolicy</B> manages policies}
  TJwLsaPolicy = class
  protected
    fLsaHandle: LSA_HANDLE;


    function GetPrivateData(Key : WideString) : WideString;
    procedure SetPrivateData(Key,Data : WideString);
  public
    constructor CreateAndOpenPolicy(
        const SystemName : WideString;
        const DesiredAccess : TJwAccessMask);

    destructor Destroy; override;

    function EnumerateAccountRights(const
      Sid : TJwSecurityId) : TJwAccountRightStringW;

    function EnumerateAccountsWithUserRights(const
      UserRights : WideString;
      const Sid : TJwSecurityId) : TJwEnumerationInformation;

    procedure AddAccountRights(
      const Sid : TJwSecurityId;
      const UserRights : TJwWideStringArray); overload;

    procedure AddAccountRights(
      const Sid : TJwSecurityId;
      const UserRights : array of WideString); overload;

    procedure RemoveAccountRights(
      const Sid : TJwSecurityId;
      const UserRights : TJwWideStringArray); overload;

    procedure RemoveAccountRights(
      const Sid : TJwSecurityId;
      const UserRights : array of WideString); overload;

    property LsaHandle: LSA_HANDLE Read fLsaHandle;

    property PrivateData[Key : WideString] : WideString
          read GetPrivateData write SetPrivateData;
  end;

  TJwLogonSessionArray = Array of TLuid;

  {<B>TJwLsaLogonSessionData</B> contains readonly information about a logon session.}
  TJwLsaLogonSessionData = class
  protected
    fSize : ULONG;
    fLogonId : TLuid;
    fUserName,
    fLogonDomain,
    fAuthenticationPackage : WideString;
    fLogonType : TSecurityLogonType;
    fSession  : ULONG;
    fSid : TJwSecurityId;
    fLogonTime : LARGE_INTEGER;
    fLogonServer,
    fDnsDomainName,
    fUpn : WideString;


  public
    constructor Create(const SessionData : PSecurityLogonSessionData = nil);
    destructor Destroy; override;

    property Size : ULONG read fSize;
    property LogonId : TLuid read fLogonId;
    property UserName : WideString read fUserName;
    property LogonDomain : WideString read fLogonDomain;
    property AuthenticationPackage : WideString read fAuthenticationPackage;
    property LogonType : TSecurityLogonType read fLogonType;
    property Session  : ULONG read fSession;
    property Sid : TJwSecurityId read fSid;
    property LogonTime : LARGE_INTEGER read fLogonTime;
    property LogonServer : WideString read fLogonServer;
    property DnsDomainName : WideString read fDnsDomainName;
    property Upn : WideString read fUpn;
  end;

  {<B>TJwLsaLogonSession</B> provides function for enumerating principal logon sessions
  and its data.}
  TJwLsaLogonSession = class
  protected
  public
     {<B>GetSessions</B> returns an array of logon ids (TLUID)
      raises
 EJwsclWinCallFailedException:  if a call to LsaEnumerateLogonSessions failed.
     }
     class function GetSessions : TJwLogonSessionArray;
     {<B>GetSessionData</B> returns TJwLsaLogonSessionData for a specific logon id.
     @param LogonID defines the logon id to be retrieved information from.
     raises
 EJwsclWinCallFailedException:  if a call to LsaGetLogonSessionData failed.
     }
     class function GetSessionData(const LogonID : TLuid) :
       TJwLsaLogonSessionData;

  end;


{<B>JwCreate_MSV1_0_INTERACTIVE_LOGON</B> is a helper function to create a MSV1_0_INTERACTIVE_LOGON
structure with strings that are added right behind the structure memory
so it can be used by LsaLogonUser.
The returned pointer can be freed by LocalFree .
}
function JwCreate_MSV1_0_INTERACTIVE_LOGON(
  MessageType: MSV1_0_LOGON_SUBMIT_TYPE;
  const LogonDomainName,
        UserName,
        Password: WideString;
  out authLen: Cardinal): PMSV1_0_INTERACTIVE_LOGON;


function JwInitLsaStringW(
  out pLsaString : LSA_UNICODE_STRING;
  {in} pwszString : WideString) : Boolean;
procedure JWFreeLsaStringW(Lsa : LSA_UNICODE_STRING);

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation


{ TJwSecurityLsa }


function JwInitLsaStringW(
  out pLsaString : LSA_UNICODE_STRING;
  {in} pwszString : WideString) : Boolean;
var
  dwLen : DWORD;
begin
  dwLen := Length(pwszString);
  if (dwLen > $7ffe) then  // String is too large
  begin
    result := FALSE;
    exit;
  end;

  // Store the string.
  GetMem(pLsaString.Buffer, (dwLen+2) * sizeof(WideChar));
  ZeroMemory(pLsaString.Buffer, (dwLen+2) * sizeof(WideChar));
  StringCchCopyW(pLsaString.Buffer, dwLen+2,PWideChar(pwszString));
  pLsaString.Length :=  dwLen * sizeof(WCHAR);
  pLsaString.MaximumLength := (dwLen+2) * sizeof(WCHAR);

  result := true;
end;

procedure JwFreeLsaStringW(Lsa : LSA_UNICODE_STRING);
begin
  FreeMem(Lsa.Buffer);
  ZeroMemory(@Lsa, sizeof(Lsa));
end;


function JwCreateLSAString(const aString: AnsiString): LSA_STRING;
//aString must be AnsiString !!
var
  pStr: PAnsiChar;
begin
  Result.Length := Length(aString);
  Result.MaximumLength := Result.Length;

  GetMem(pStr, Length(aString) + 2);
  FillChar(pStr^, Length(aString) + 2, 0);
  StrLCopy(pStr, PAnsiChar(aString), Length(aString));

  Result.Buffer := pStr;
end;

procedure JwFreeLSAString(var aString: LSA_STRING);
begin
  if aString.Buffer <> nil then
    FreeMem(aString.Buffer);

  FillChar(aString, sizeof(aString), 0);
end;


constructor TJwSecurityLsa.CreateUntrusted;
var res: NTSTATUS;
begin
  res := LsaConnectUntrusted(fLsaHandle);
  if res <> STATUS_SUCCESS then
  begin
    res := LsaNtStatusToWinError(res);
    SetLastError(res);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'CreateUntrusted', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaConnectUntrusted',
      ['LsaRegisterLogonProcess']);
  end;
end;

constructor TJwSecurityLsa.Create(const LogonProcessName: AnsiString);
var
  lsaHostString: LSA_STRING;
  res: NTSTATUS;
  lsaSecurityMode: LSA_OPERATIONAL_MODE;

const
  p1: _LSA_STRING = (Length: 3;
    MaximumLength: 3;
    Buffer: '12'#0);
begin
  lsaHostString := JwCreateLSAString(LogonProcessName);

  res := LsaRegisterLogonProcess(lsaHostString, fLsaHandle, @lsaSecurityMode);

  JwFreeLSAString(lsaHostString);

  if res <> STATUS_SUCCESS then
  begin
    res := LsaNtStatusToWinError(res);
    SetLastError(res);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'Create', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaRegisterLogonProcess',
      ['LsaRegisterLogonProcess']);
  end;
end;


destructor TJwSecurityLsa.Destroy;
begin
  LsaDeregisterLogonProcess(fLsaHandle);
  fLsaHandle := 0;
end;

procedure _initUnicodeString(target: PUNICODE_STRING;
  Source: PWideChar; cbMax: USHORT);
begin
  target.Length := cbMax;//-2;//- sizeof(source^);
  target.MaximumLength := cbMax;
  target.Buffer := Source;
end;


function JwCreate_MSV1_0_INTERACTIVE_LOGON(
  MessageType: MSV1_0_LOGON_SUBMIT_TYPE;
  const LogonDomainName,
        UserName,
        Password: WideString;
  out authLen: Cardinal): PMSV1_0_INTERACTIVE_LOGON;
var
  iSize: integer;
  p: PWCHAR;

  cbHeader, cbDom, cbUser, cbPass: integer;

  pDom, pUser, pPass: PWChar;

const
  iUSHORT = sizeof(USHORT);
  iWCHAR  = sizeof(widechar);
begin
  cbHeader := sizeof(MSV1_0_INTERACTIVE_LOGON);
  cbDom  := Length(LogonDomainName) * iWCHAR;
  cbUser := Length(UserName) * iWCHAR;
  cbPass := Length(Password) * iWCHAR;

  iSize := cbHeader + cbDom + cbUser + cbPass;

  authLen := iSize;

  Result := PMSV1_0_INTERACTIVE_LOGON(LocalAlloc(LMEM_ZEROINIT or
    LMEM_FIXED, iSize));

  Result.MessageType := MessageType;
  p := PWCHAR(Result);
//WARNING: possible 3gb Adress Space problem when p is converted to integer   
  Inc(p, cbHeader);

  pDom  := p;
  pUser := PWChar(p + cbDom);
  pPass := PWChar(p + cbDom + cbUser);

  CopyMemory(pDom, @LogonDomainName[1], cbDom);
  CopyMemory(pUser, @UserName[1], cbUser);
  CopyMemory(pPass, @Password[1], cbPass);

  _initUnicodeString(@Result.LogonDomainName, pDom, cbDom);
  _initUnicodeString(@Result.UserName, pUser, cbUser);
  _initUnicodeString(@Result.Password, pPass, cbPass);
end;

procedure TJwSecurityLsa.LsaLogonUser(
  const anOriginName: AnsiString;
  aLogonType: SECURITY_LOGON_TYPE;
  const anAuthenticationPackageName: AnsiString;
  anAuthenticationInformation: Pointer;
  anAuthenticationInformationLength: Cardinal;
  aLocalGroups: TJwSecurityIdList; aSourceContext: TTokenSource;
  out aProfileBuffer: Pointer; out aProfileBufferLength: Cardinal;
  out aTokenLuid: TLUID; out aToken: TJwSecurityToken;
  out aQuotaLimits: QUOTA_LIMITS; out SubStatus: NTSTATUS);

var
  res: NTSTATUS;
  lsaOrig, lsaPackageName: LSA_STRING;

  pLocalGroups: PTokenGroups;
  hToken: TJwTokenHandle;
  cAuthenticationPackage: Cardinal;

const
  p1: _LSA_STRING = (Length: 20;
    MaximumLength: 20;
    Buffer: ''#0);
begin
  lsaPackageName := JwCreateLSAString(anAuthenticationPackageName);

  cAuthenticationPackage := 0;
  res := LsaLookupAuthenticationPackage(
    fLsaHandle,//HANDLE LsaHandle,
    lsaPackageName,//PLSA_STRING PackageName,
    cAuthenticationPackage);
  JwFreeLSAString(lsaPackageName);


  if res <> STATUS_SUCCESS then
  begin
    res := LsaNtStatusToWinError(res);
    SetLastError(res);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'Create', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaLookupAuthenticationPackage',
      ['LsaRegisterLogonProcess']);
  end;

  lsaOrig := JwCreateLSAString(anOriginName);

  pLocalGroups := nil;
  if Assigned(aLocalGroups) then
  begin
    pLocalGroups := aLocalGroups.Create_PTOKEN_GROUPS;
  end;
  aToken := nil;

  FillChar(aTokenLuid, sizeof(aTokenLuid), 0);
  hToken := 0;
  Fillchar(aQuotaLimits, sizeof(aQuotaLimits), 0);
  SubStatus := 0;

  res := jwaWindows.LsaLogonUser(fLsaHandle,//HANDLE LsaHandle,
    lsaOrig,//PLSA_STRING OriginName,
    aLogonType,//SECURITY_LOGON_TYPE LogonType,
    cAuthenticationPackage,//ULONG AuthenticationPackage,
    anAuthenticationInformation,//PVOID AuthenticationInformation,
    anAuthenticationInformationLength,//ULONG AuthenticationInformationLength,
    pLocalGroups,//PTOKEN_GROUPS LocalGroups,
    @aSourceContext,//PTOKEN_SOURCE SourceContext,
    aProfileBuffer,//PVOID* ProfileBuffer,
    aProfileBufferLength,//PULONG ProfileBufferLength,
    aTokenLuid,//PLUID LogonId,
    hToken, //PHANDLE Token,
    aQuotaLimits,//PQUOTA_LIMITS Quotas,
    SubStatus//PNTSTATUS SubStatus
    );
  JwFreeLSAString(lsaOrig);

  if Assigned(aLocalGroups) and (pLocalGroups <> nil) then
  begin
    aLocalGroups.Free_PTOKEN_GROUPS(pLocalGroups);
  end;

  if res <> STATUS_SUCCESS then
  begin
    res := LsaNtStatusToWinError(res);
    SetLastError(res);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsLSALogonUserFailedSubStatus,
      'Create', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaLogonUser', [SubStatus]);
  end;

  aToken := nil;
  if (hToken <> 0) and (hToken <> INVALID_HANDLE_VALUE) then
    aToken := TJwSecurityToken.Create(hToken, shOwned, TOKEN_ALL_ACCESS);

end;

{ TJwLsaLogonSession }

class function TJwLsaLogonSession.GetSessionData(
  const LogonID: TLuid): TJwLsaLogonSessionData;
var p : PSecurityLogonSessionData;
    res : NTSTATUS;
begin
  p := nil;

  res := LsaGetLogonSessionData(@LogonId,p);

  if res <> STATUS_SUCCESS then
  begin
    SetLastError(LsaNtStatusToWinError(res));
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailedWithNTStatus,
      'GetSessionData', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaGetLogonSessionData', ['LsaGetLogonSessionData', res]);
  end;

  result := TJwLsaLogonSessionData.Create(p);

  LsaFreeReturnBuffer(p);
end;

class function TJwLsaLogonSession.GetSessions: TJwLogonSessionArray;
var List,
    LuidPtr : PLuid;
    Count : ULONG;
    res : NTSTATUS;
  I: Integer;
begin
  List := nil;
  Count := 0;
  res := LsaEnumerateLogonSessions(@count, List);

  if res <> STATUS_SUCCESS then
  begin
    SetLastError(LsaNtStatusToWinError(res));
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailedWithNTStatus,
      'GetSessionData', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaEnumerateLogonSessions', ['LsaEnumerateLogonSessions', res]);
  end;

  SetLength(result, Count);
  LuidPtr := List;
  for I := 0 to Count - 1 do
  begin
    result[i] := LuidPtr^;
    Inc(LuidPtr);
  end;


  LsaFreeReturnBuffer(List);
  List := nil;
end;

{ TJwLsaLogonSessionData }

constructor TJwLsaLogonSessionData.Create(
  const SessionData : PSecurityLogonSessionData = nil);
begin
  fSid := nil;
  if (SessionData <> nil) then
  begin
    fSize := SessionData.Size;
    fLogonId := SessionData.LogonId;
    fLogonType := SessionData.LogonType;
    fSession := SessionData.Session;
    fLogonTime := SessionData.LogonTime;

    if SessionData.Sid <> nil then
      fSid := TJwSecurityId.Create(SessionData.Sid);

    SetLength(fUserName, SessionData.UserName.Length);
    fUserName := SessionData.UserName.Buffer;

    SetLength(fLogonDomain, SessionData.LogonDomain.Length);
    fLogonDomain := SessionData.LogonDomain.Buffer;

    SetLength(fAuthenticationPackage, SessionData.AuthenticationPackage.Length);
    fAuthenticationPackage := SessionData.AuthenticationPackage.Buffer;

    SetLength(fLogonServer, SessionData.LogonServer.Length);
    fLogonServer := SessionData.LogonServer.Buffer;

    SetLength(fDnsDomainName, SessionData.DnsDomainName.Length);
    fDnsDomainName := SessionData.DnsDomainName.Buffer;

    SetLength(fUpn, SessionData.Upn.Length);
    fUpn := SessionData.Upn.Buffer;
  end;
end;


destructor TJwLsaLogonSessionData.Destroy;
begin
  FreeAndNil(fSid);
  inherited;
end;



function TJwLsaPolicy.EnumerateAccountRights(
  const Sid: TJwSecurityId): TJwAccountRightStringW;
var
  ntsResult : NTSTATUS;
  EnumRight,
  UserRights : PLSA_UNICODE_STRING;
  i,
  CountOfRights : Cardinal;
begin
  ntsResult := LsaEnumerateAccountRights(
    fLsaHandle,//__in   LSA_HANDLE PolicyHandle,
    Sid.Sid,//__in   PSID AccountSid,
    UserRights,//__out  PLSA_UNICODE_STRING* UserRights,
    CountOfRights//__out  PULONG CountOfRights
  );

  if ntsResult <> STATUS_SUCCESS then
  begin
    SetLastError(LsaNtStatusToWinError(ntsResult));
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'EnumerateAccountRights', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaOpenPolicy',
      ['LsaOpenPolicy']);
  end;


  EnumRight := UserRights;
  SetLength(result, CountOfRights);
  for i := 0 to CountOfRights-1 do
  begin
    result[i] := WideString(EnumRight.Buffer);
    Inc(EnumRight);
  end;
  LsaFreeMemory(UserRights);
end;

function TJwLsaPolicy.EnumerateAccountsWithUserRights(
  const UserRights: WideString;
  const Sid: TJwSecurityId): TJwEnumerationInformation;
begin
  result := nil;
end;



constructor TJwLsaPolicy.CreateAndOpenPolicy(
  const SystemName: WideString; const DesiredAccess: TJwAccessMask);
var
  ntsResult : NTSTATUS;
  pSystemName : LSA_UNICODE_STRING;
  ObjectAttributes : LSA_OBJECT_ATTRIBUTES;
begin
  ZeroMemory(@ObjectAttributes, sizeof(ObjectAttributes));

  if Length(SystemName) > 0 then
  begin
    JwInitLsaStringW(pSystemName, SystemName);
    ntsResult := LsaOpenPolicy(@pSystemName, ObjectAttributes,
      DesiredAccess, Pointer(fLsaHandle));
    JWFreeLsaStringW(pSystemName);
  end
  else
    ntsResult := LsaOpenPolicy(nil, ObjectAttributes,
      DesiredAccess, Pointer(fLsaHandle));

  if ntsResult <> STATUS_SUCCESS then
  begin
    SetLastError(LsaNtStatusToWinError(ntsResult));
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'CreateAndOpenPolicy', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaOpenPolicy',
      ['LsaOpenPolicy']);
  end;
end;

destructor TJwLsaPolicy.Destroy;
begin
  inherited;
  LsaClose(Pointer(fLsaHandle));
  fLsaHandle := nil;
end;

procedure TJwLsaPolicy.AddAccountRights(const Sid: TJwSecurityId;
  const UserRights: TJwWideStringArray);
var
  ntsResult : NTSTATUS;

  //Privs : PLSA_UNICODE_STRING;
  //PrivCount : Cardinal;

  Arr : Array of LSA_UNICODE_STRING;
  i : Integer;
begin
  SetLength(Arr, Length(UserRights));
  for i := 0 to High(UserRights) do
  begin
    JwInitLsaStringW(Arr[i],UserRights[i]);
  end;


  ntsResult := LsaAddAccountRights(
      fLsaHandle,//__in   LSA_HANDLE PolicyHandle,
      Sid.SID, //__in   PSID AccountSid,
      @Arr[0],//__in  PLSA_UNICODE_STRING UserRights,
      Length(UserRights)//__in  ULONG CountOfRights
    );

  try
    if ntsResult <> STATUS_SUCCESS then
    begin
      SetLastError(LsaNtStatusToWinError(ntsResult));
      raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed,
        'CreateAndOpenPolicy', ClassName, 'JwsclLsa.pas',
        0, True, 'LsaAddAccountRights',
        ['LsaAddAccountRights']);
    end;
  finally
    for i := 0 to High(UserRights) do
    begin
      JWFreeLsaStringW(Arr[i]);
    end;
  end;

end;

procedure TJwLsaPolicy.AddAccountRights(const Sid: TJwSecurityId;
  const UserRights: array of WideString);
var Len,i : Integer;
    Data : TJwWideStringArray;
begin
  SetLength(Data, Length(UserRights));

  Len := Length(Data);
  for i := 0 to high(Data) do
  begin
    if UserRights[i] <> '' then
      Data[i] := UserRights[i]
    else
      Dec(Len);
  end;

  SetLength(Data, Len);

  AddAccountRights(Sid, Data);
end;

procedure TJwLsaPolicy.RemoveAccountRights(const Sid: TJwSecurityId;
  const UserRights: TJwWideStringArray);
var
  ntsResult : NTSTATUS;

  //Privs : PLSA_UNICODE_STRING;
  //PrivCount : Cardinal;

  Arr : Array of LSA_UNICODE_STRING;
  i : Integer;
begin
  SetLength(Arr, Length(UserRights));
  for i := 0 to High(UserRights) do
  begin
    JwInitLsaStringW(Arr[i],UserRights[i]);
  end;

  ntsResult := LsaRemoveAccountRights(
      fLsaHandle,//__in   LSA_HANDLE PolicyHandle,
      Sid.SID, //__in   PSID AccountSid,
      false,
      @Arr[0],//__in  PLSA_UNICODE_STRING UserRights,
      Length(UserRights)//__in  ULONG CountOfRights
    );

  try
    if ntsResult <> STATUS_SUCCESS then
    begin
      SetLastError(LsaNtStatusToWinError(ntsResult));
      raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed,
        'RemoveAccountRights', ClassName, 'JwsclLsa.pas',
        0, True, 'LsaRemoveAccountRights',
        ['LsaRemoveAccountRights']);
    end;
  finally
    for i := 0 to High(UserRights) do
    begin
      JWFreeLsaStringW(Arr[i]);
    end;
  end;

end;



procedure TJwLsaPolicy.RemoveAccountRights(const Sid: TJwSecurityId;
  const UserRights: array of WideString);
var Len,i : Integer;
    Data : TJwWideStringArray;
begin
  SetLength(Data, Length(UserRights));

  Len := Length(Data);
  for i := 0 to high(Data) do
  begin
    if UserRights[i] <> '' then
      Data[i] := UserRights[i]
    else
      Dec(Len);
  end;

  SetLength(Data, Len);

  RemoveAccountRights(Sid, Data);
end;


function TJwLsaPolicy.GetPrivateData(Key: WideString): WideString;
var
  ntsResult : NTSTATUS;
  pData : PLSA_UNICODE_STRING;
  pStr : LSA_UNICODE_STRING;
  dwLen : Cardinal;
begin
  JwInitLsaStringW(pStr, Key);

  ntsResult := LsaRetrievePrivateData(
    fLsaHandle,//__in   LSA_HANDLE PolicyHandle,
    pStr,//__in   PLSA_UNICODE_STRING KeyName,
    pData//__out  PLSA_UNICODE_STRING* PrivateData
  );
  try
    if ntsResult <> STATUS_SUCCESS then
    begin
      SetLastError(LsaNtStatusToWinError(ntsResult));
      raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed,
        'RemoveAccountRights', ClassName, 'JwsclLsa.pas',
        0, True, 'LsaRemoveAccountRights',
        ['LsaRemoveAccountRights']);
    end;

    dwLen := 1 + pData.Length div sizeof(WideChar);
    SetLength(result, dwLen);

    StringCchCopyNW(PWideChar(result), dwLen, pData.Buffer, pData.Length div sizeof(WideChar));
  finally
    JWFreeLsaStringW(pStr);
    LsaFreeMemory(pData);
  end;
end;

procedure TJwLsaPolicy.SetPrivateData(Key, Data: WideString);
var
  ntsResult : NTSTATUS;
  pData,
  pStr : LSA_UNICODE_STRING;
begin
  JwInitLsaStringW(pStr, Key);
  JwInitLsaStringW(pData, Data);

  ntsResult := LsaStorePrivateData(
    fLsaHandle,//__in   LSA_HANDLE PolicyHandle,
    @pStr,//__in   PLSA_UNICODE_STRING KeyName,
    @pData//__out  PLSA_UNICODE_STRING* PrivateData
  );
  try
    if ntsResult <> STATUS_SUCCESS then
    begin
      SetLastError(LsaNtStatusToWinError(ntsResult));
      raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed,
        'RemoveAccountRights', ClassName, 'JwsclLsa.pas',
        0, True, 'LsaRemoveAccountRights',
        ['LsaRemoveAccountRights']);
    end;


  finally
    JWFreeLsaStringW(pStr);
    JWFreeLsaStringW(pData);
  end;
end;

initialization
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INITIALIZATION_SECTION}
  //_----

{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}