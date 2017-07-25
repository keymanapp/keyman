program CreateToken;

{$APPTYPE CONSOLE}

uses
  Dialogs,
  JwaWindows,
  JWsclToken,
  JwsclSid,
  JwsclKnownSid,
  JwsclTypes,
  JwsclAcl,
  JwsclConstants,
  JwsclLsa,
  JwsclExceptions,
  JwsclStrings,
  SysUtils;

//get the logon id of the first interactive user
function GetSession(User : TJwSecurityToken; out Luid : TLUid) : Boolean;
var Sessions : TJwLogonSessionArray;
    i : Integer;
    S : String;
    Data : TJwLsaLogonSessionData;
    Sid : TJwSecurityId;
begin
  result := false;

  Sessions := TJwLsaLogonSession.GetSessions;
  for i := low(Sessions) to high(Sessions) do
  begin
    S := TJwPrivilege.LUIDtoText(Sessions[i]);
    try
      Data := TJwLsaLogonSession.GetSessionData(Sessions[i]);

      Sid := User.TokenUser;
      if Sid.EqualSid(Data.Sid) and
        (Data.LogonType = Interactive)  then
      begin
        Luid := Data.LogonId;
        result := true;

        FreeAndNil(Data);
        exit;
      end;

      FreeAndNil(Data);
    except
      on E : EJwsclWinCallFailedException do
        S := S + ' Error: '+E.GetLastErrorMessage(E.LastError);
    end;
  end;
end;

var
  Token,
  SToken : TJwSecurityToken;
  ObjectAttributes: TObjectAttributes;
  AuthenticationId: TLUID;

  NewGroups,
  UserGroups: TJwSecurityIdList;
  Privileges: TJwPrivilegeSet;
  TokenSource : TTokenSource;

  DefaultDACL : TJwDAccessControlList;

  UserToken : TJwSecurityToken;

  StartInfo : TStartupInfo;
  ProcInfo : TProcessInformation;

  Sid,
  LogonSid,
  User, Owner,
  Group : TJwSecurityId;
  Stats : TJwSecurityTokenStatistics;
begin
try
  //privilege must be enabled in VISTA (at least Sp1)
  //can be added by JwsclLsa.TJwLsaPolicy members for SYSTEM (or any) user
  //adding to SYSTEM needs restart, adding to user needs relogon (or call to LogonUser)
  JwEnablePrivilege(SE_CREATE_TOKEN_NAME,pst_Enable);

  JwInitWellKnownSIDs;

  UserToken := TJwSecurityToken.CreateWTSQueryUserTokenEx(nil, WTS_CURRENT_SESSION);
  Stats := UserToken.GetTokenStatistics;

  //Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
  Privileges := UserToken.GetTokenPrivileges();
  DefaultDACL := UserToken.GetTokenDefaultDacl;

  ZeroMemory(@ObjectAttributes, sizeof(ObjectAttributes));
  ObjectAttributes.Length := sizeof(ObjectAttributes);
//  ObjectAttributes.ObjectName.

  Owner := UserToken.GetTokenOwner;
  User := UserToken.GetTokenUser;
  Group := UserToken.GetPrimaryGroup;


  //create our own logon id
  //this logon ID must be registered first! (don't know how yet)
  //AllocateLocallyUniqueId(AuthenticationId);
  
  //get any logon session we want
  {if not GetSession(UserToken, AuthenticationId) then
    exit;}
  //get the logon ID from the user token object
  AuthenticationId := Stats.AuthenticationId;
  //this one does not work in Vista - createtoken says : the logon id may be already finished
  AllocateLocallyUniqueId(AuthenticationId);

  writeln(Stats.GetText);
  Stats.Free;
  //GetUserName reads the username from the token logon id rather than token user


  //get default token groups
  UserGroups := UserToken.TokenGroups;
  
  
  //add terminal server user (just for testing)
  Sid := TJwSecurityId.Create('S-1-5-13');
  Sid.AttributesType := [sidaGroupMandatory];
  UserGroups.Add(Sid);//S-1-5-1-1-1'));

  //add unknown Sid
  try
    Sid := TJwSecurityId.Create('','NT SERVICE\TrustedInstaller');
    Sid.AttributesType := [sidaGroupMandatory];
    UserGroups.Add(Sid);
  except
    //ignore
  end;


  //JwLocalSystemSID.AttributesType := [sidaGroupOwner];
  //UserGroups.Add(JwLocalSystemSID);

  ZeroMemory(@TokenSource, sizeof(TokenSource));
  TokenSource.SourceName := 'CTTest'; //CreateTokenTest identifier name
  AllocateLocallyUniqueId(TokenSource.SourceIdentifier); //any luid that defines us


  JwEnablePrivilege(SE_TCB_NAME,pst_Enable);
  JwEnablePrivilege(SE_CREATE_TOKEN_NAME,pst_Enable);

  try
    SToken := TJwSecurityToken.CreateNewToken(
    TOKEN_ALL_ACCESS,//const aDesiredAccess: TJwAccessMask;
    ObjectAttributes,//const anObjectAttributes: TObjectAttributes;
    AuthenticationId,//const anAuthenticationId: TLUID;
    0,//const anExpirationTime: int64;
    User,//anUser: TJwSecurityId;
    UserGroups,//aGroups: TJwSecurityIdList;
    Privileges,//aPrivileges: TJwPrivilegeSet;
    Owner,//anOwner,
    Group,//aPrimaryGroup: TJwSecurityId;
    DefaultDACL,//aDefaultDACL: TJwDAccessControlList;
    TokenSource //aTokenSource: TTokenSource
    );
  except
    on e : Exception do
      ShowMessage(E.Message);
  end;


  //Target session ID from user
  SToken.TokenSessionId := UserToken.TokenSessionId;

  ZeroMemory(@StartInfo, sizeof(StartInfo));

  //necessary only in XP and if user is SYSTEM
  StartInfo.lpDesktop := PChar('winsta0\default');

  if not CreateProcessAsUser(SToken.TokenHandle, PChar('C:\Windows\system32\cmd.exe'), nil,
           nil, nil, True, CREATE_NEW_CONSOLE,
           nil, nil, StartInfo, ProcInfo) then
    RaiseLastOSError;

  WaitForSingleObject(ProcInfo.hProcess,INFINITE);


  {WARNING:
   TODO: delete objects here

  }


  {
  these rights are necessary to read desktop content
STANDARD_RIGHTS_READ or
DESKTOP_ENUMERATE or
DESKTOP_READOBJECTS or
DESKTOP_WRITEOBJECTS
  }
except
  on e  : exception do
    showmessage(e.Message);
end;
  
end.
