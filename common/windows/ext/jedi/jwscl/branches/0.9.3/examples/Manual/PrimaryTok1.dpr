{This program is for testing purposes only!
Needs TCB, AssignPrimaryToken and CreateToken Privilege!!
You can activate them in gpedit.msc for local users rights. This program
cannot be used as service without major improvements.

Purpose:
This program creates a new token for the actual user, starts a process
suspended and assigns the newly created token to the process.
This approach is undocumented but CreateProcessAsUser(hToken) does the same (without
CreateToken)


This program calls SecurityID3.exe by default for token information.

Warning:
This programm allocates a lot of memory and does not free it!!

}
program PrimaryTok1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Dialogs,
  JwaWindows,
  JwaVista,

  jwsclConstants,
  jwsclTypes,
  JwsclMapping,
  JwsclSid,
  JwsclToken,
  JwsclResource,
  JwsclDescriptor,
  JwsclACL,
  JwsclLsa,
  JwsclUtils,
  JwsclSecureObjects,
  JwsclKnownSid,
  JwsclAuthCtx;


procedure NTCheck(Res: Cardinal);
begin
  If (Res <>0) then
    ShowMessage(SysErrorMessage(RtlNtStatusToDosError(Res)));
end;


{
Find the logon session of a user
by iterating through all logons.

}
function GetLuid(const User : TJwSecurityId) : TLuid;
var S : TJwLogonSessionArray;
    P : TJwLsaLogonSessionData;
    i : Integer;
begin
  ZeroMemory(@result,sizeof(TLuid));
  writeln(' GetSessions');
  S := TJwLsaLogonSession.GetSessions;

  writeln(' for...');
  for i := low(S) to high(S) do
  begin
    writeln(i);
    P := TJwLsaLogonSession.GetSessionData(S[i]);
    if (P <> nil) and (Assigned(P.SID))and (P.Sid.EqualPrefixSid(User))
     //only for Vista
     //and (P.Session > 0)

     then
    begin
      result := S[i];
      writeln('return: ',i);
      exit;
    end;
  end;
  writeln('Error luid');
end;


procedure DoIt;
var TokenHandle : THandle;
  ProcessAccessTokenrec: record
    hToken : THandle;
    hThread : THandle;
  end;
  NewTokenHandle: THandle;
  oa: OBJECT_ATTRIBUTES;
  tokentime: int64;
  user: TOKEN_USER;
  TokenSource: TTokenSource;
  sysLuid : TLuid;
  SessionId: DWORD;

  TwinToken,
  ProcToken,
  NTToken,
  nToken,
  tok : TJwSecurityToken;

  nGroups,
  nGroups2,
  nGroups3 : TJwSecurityIdList;
  nPrivs : TJwPrivilegeSet;
  nDACL : TJwDAccessControlList;

  Sid,
  SidUser,
  OwnerSid : TJwSecurityId;

  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;

  SD : TJwSecurityDescriptor;
begin
  try
  //enable privileges for create token
  JwEnablePrivilege('SeTcbPrivilege',pst_Enable);
  JwEnablePrivilege('SeCreateTokenPrivilege',pst_Enable);
  JwEnablePrivilege('SeAssignPrimaryTokenPrivilege',pst_Enable);
  JwEnablePrivilege(SE_TAKE_OWNERSHIP_NAME,pst_Enable);

  nToken := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);


  {
  Get a sid for the new token owner.
  This sid will be used for the primary group, the token owner
   a member of the token groups and for the searching for a logon session.
  }
  //OwnerSid := JwAdministratorsSID;
  OwnerSid := JwSecurityProcessUserSID;
  Writeln('User: ',OwnerSid.GetText(true));
  //OwnerSid := TJwSecurityId.Create('','TestBenutzer');

  SidUser := JwGetLogonSID(nToken);

  //OwnerSid:=SidUser;
  //OwnerSid := JwWorldSID;
  //OwnerSid := JwAdministratorsSID;
  //OwnerSid := JwLocalSystemSID;


  {  SD := TJwSecurityDescriptor.CreateDefaultByToken();
    SD.Owner := OwnerSid;
    SD.PrimaryGroup := OwnerSid;
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],TOKEN_ALL_ACCESS,JwWorldSID,false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],TOKEN_ALL_ACCESS,JwSecurityProcessUserSID,false));

  }



    writeln(nToken.PrimaryGroup.GetText(true));
    writeln;

   // nToken.GetTokenOrigin
    nGroups := nToken.TokenGroups;
    //nGroups := TJwSecurityIdList.Create;

    {
    Get Token groups from the default Token.
    This group must contain the TokenOwner as a member otherwise CreatToken fails.

    It also needs a logon SID for creating windows.

    Strange thing:
     On Vista: If the user has the admin group in the group and it is removed or denied
     All access for the new process is denied. This only applies to a logon session for user
     greater than zero.
     In this case SecurityID3.exe fails and closes immediately. Try c:\windows\system32\cmd.exe
     instead and you will see that even a simply command "dir" gets "access denied"
     Dont know why!

    }

  {
    nGroups.Delete(9);      }
    //nGroups.Delete(0);
    //nGroups.Delete(1);

   { nGroups.Delete(2); }

    {Sid := TJwSecurityId.Create('','Christian');}

    {This should add the token owner to the groups.
     Because it is not a group it is simply ignored and the new token
     will not have this SID.
    }
    Sid := OwnerSid;
    Sid.AttributesType := [sidaGroupOwner];
    nGroups.Add(Sid);

    //remove admin - leads to problems on vista
   { try
      nGroups.Delete(nGroups.FindSid(JwAdministratorsSID));
    except
    end;  }

    //remove integrity label
    try
      nGroups.Delete(nGroups.FindSid(JwIntegrityLabelSID[iltHigh]));
    except
    end;
   { nGroups.Delete(9);
    nGroups.Delete(9);
    nGroups.Delete(9);  }

    JwIntegrityLabelSID[iltLow].AttributesType := [sidaGroupIntegrityEnabled];
    nGroups.Add(JwIntegrityLabelSID[iltLow]);

    JwKnownSid[WinBuiltinUsersSid].AttributesType := [sidaGroupEnabled];
    nGroups.Add(JwKnownSid[WinBuiltinPowerUsersSid]);
   { JwAdministratorsSID.AttributesType := [sidaGroupEnabled];
    nGroups.Insert(0, JwAdministratorsSID);    }

   //some additional groups if nec.
 (*
    JwWorldSID.AttributesType := [sidaGroupEnabled];
    nGroups.Add(JwWorldSID);   


  {  JwLocalSystemSID.AttributesType := [sidaGroupEnabled];
    nGroups.Add(JwLocalSystemSID);
      }

    SidUser.AttributesType := [sidaGroupLogonId];
    nGroups.Add(SidUser);

    JwKnownSid[WinBuiltinUsersSid].AttributesType := [sidaGroupEnabled];
    nGroups.Add(JwKnownSid[WinBuiltinUsersSid]);

    JwKnownSid[WinBuiltinUsersSid].AttributesType := [sidaGroupEnabled];
    nGroups.Add(JwKnownSid[WinBuiltinPowerUsersSid]);

    JwKnownSid[WinEnterpriseControllersSid].AttributesType := [sidaGroupEnabled];
    nGroups.Add(JwKnownSid[WinEnterpriseControllersSid]);

    JwKnownSid[WinAuthenticatedUserSid].AttributesType := [sidaGroupEnabled];
    nGroups.Add(JwKnownSid[WinAuthenticatedUserSid]);
   *)
    {
    Add none group. This is a well known RID so the SID differs from
    system to system. We just use its name here.
    Can fail on XP.
    }
    try
      Sid := TJwSecurityId.Create('','None');
      Sid.AttributesType := [sidaGroupEnabled];
      nGroups.Add(Sid);
    except
    end;



      
  {
    try
      JwWorldSID.AttributesType := [sidaGroupEnabled];
      nGroups.Add(JwWorldSID);
    except
    end;   }

  {  Sid := TJwSecurityId.Create('','AdminUser');
    Sid.AttributesType := [sidaGroupEnabled,sidaGroupOwner];
    nGroups.Add(Sid);   }



    writeln(nGroups.GetText(true));


    //nPrivs := TJwPrivilegeSet.Create;

    {
    Get default privilege
    }
    nPrivs := nToken.GetTokenPrivileges;
    writeln(nPrivs.GetText);

    {
    Add some more default ACEs to the token
    In tests this did nothing.

    }
    nDACL := nToken.TokenDefaultDacl;
    nDACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],TOKEN_ALL_ACCESS,OwnerSid,false));
    nDACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],TOKEN_ALL_ACCESS,JwWorldSID,false));


    writeln(nDACL.GetTextMap(TJwSecurityTokenMapping));





  ZeroMemory(@oa, sizeOf(OBJECT_ATTRIBUTES));
  oa.Length := sizeOf(OBJECT_ATTRIBUTES);

  {
  This only applies if you use SessionID of System and try to use another
  user for this token.

  In this case we have to add the user to the token DACL so he can call
  CreateProcessToken successfully.

  I experienced a strange problem. Sometimes CreateToken fails with "SD cannot
  be assigned" so I get the token DACL after the creation and add the ACE.

  You cannot deny any right to the actual user or CreateToken fails.
  }
{  SD := TJwSecureGeneralObject.GetSecurityInfo(nToken.TokenHandle,SE_KERNEL_OBJECT,
    [siOwnerSecurityInformation,siGroupSecurityInformation,siDaclSecurityInformation]);
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],TOKEN_ALL_ACCESS,OwnerSid,false));
  oa.SecurityDescriptor := SD.Create_SD();
}


  tokentime := $FFFFFFFFFFFFFFFF;
  TokenSource.SourceName := 'SAMPLE';


  {
  The final token user and group - and also GetUserName returns it.
  This LUID is the logon session id of the user.
  If you specify e.g. SYSTEM_LUID the new token will hold the logon data
  of the SYSTEM user. GetUserName will return "SYSTEM".
  To get a real user we need a LUID by an already logged on user.
   Thus we call GetLuid.

  }
  //sysLuid := SYSTEM_LUID;
  //sysLuid := LOCALSERVICE_LUID;
  writeln('GetLuid');
  sysLuid := GetLuid(OwnerSid);
  //sysLuid := NETWORKSERVICE_LUID;
  //sysLuid := ANONYMOUS_LOGON_LUID;
  writeln('AllocateLocallyUniqueId');

  {Create a new ID
  }
  Win32Check(AllocateLocallyUniqueId(TokenSource.SourceIdentifier));
  writeln('before ntcreatetoken');


  writeln('0');

  {
  Creates the new token.

  In vista a token can have a partner.
  If the Luid is a user that has a twin token it can happen
  that the second token is applied to this token but has
  less or more rights than we want!

  in this case we also have to change the twin token

  }
  NTToken := TJwSecurityToken.CreateNewToken(
   { TOKEN_READ or TOKEN_ASSIGN_PRIMARY or
    TOKEN_QUERY or TOKEN_WRITE or TOKEN_EXECUTE or TOKEN_DUPLICATE}
    TOKEN_ALL_ACCESS
    ,//TOKEN_ALL_ACCESS,//const aDesiredAccess: TJwAccessMask;
    oa,//const anObjectAttributes: TObjectAttributes;
    sysLuid,//const anAuthenticationId: TLUID;
    tokentime,//const anExpirationTime: int64;
    OwnerSid,//anUser: TJwSecurityId;

    nGroups,//aGroups: TJwSecurityIdList;
    nPrivs,//aPrivileges: TJwPrivilegeSet;

    OwnerSid,//JwLocalSystemSID,//anOwner,

    //Primgroup Sid must be a member of TokenGroups
    OwnerSid,//nToken.PrimaryGroup,//aPrimaryGroup: TJwSecurityId;
    //TJwSecurityID.Create('','None'),
    nil,//nDACL,//aDefaultDACL: TJwDAccessControlList;
    TokenSource,//aTokenSource: TTokenSource);
  );

  try
    TwinToken := NTToken.GetLinkedToken;
    writeln('**** Linked Tolken ****');
    //in this case we also have to change the twin token
    
    nGroups3 := TwinToken.TokenGroups;
    Writeln(nGroups3.GetText(true));
    //remove admin - leads to problems on vista
    try
      nGroups.Delete(nGroups3.FindSid(JwAdministratorsSID));
    except
    end;

    //remove integrity label
    try
      nGroups.Delete(nGroups3.FindSid(JwIntegrityLabelSID[iltHigh]));
    except
    end;



    {
    On Vista- clearing the token groups on this token has no effect
     (also change groups)
    }
    TwinToken.TokenGroups := TJwSecurityIdList.Create; //nGroups3;
    nGroups3 := TwinToken.TokenGroups;

    Writeln(nGroups3.GetText(true));
  except
  end;

  writeln('SetSec');



  {
  Second way of adapting the tokens DACL.

  This only applies if you use SessionID of System and try to use another
  user for this token.

  In this case we have to add the user to the token DACL so he can call
  CreateProcessToken successfully.

  I experienced a strange problem. Sometimes CreateToken fails with "SD cannot
  be assigned" so I get the token DACL after the creation and add the ACE.

  You cannot deny any right to the actual user or CreateToken fails.
  }
(*  SD := TJwSecureGeneralObject.GetSecurityInfo(NTToken.TokenHandle,SE_KERNEL_OBJECT,
    [siOwnerSecurityInformation,siGroupSecurityInformation,siDaclSecurityInformation]);
  Writeln(SD.Text);
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],TOKEN_ALL_ACCESS,OwnerSid,false));
 { SD.Owner := OwnerSid; }

  TJwSecureGeneralObject.SetSecurityInfo(NTToken.TokenHandle,SE_KERNEL_OBJECT,
    [{siOwnerSecurityInformation,}siGroupSecurityInformation,siDaclSecurityInformation],SD);

  SD := TJwSecureGeneralObject.GetSecurityInfo(NTToken.TokenHandle,SE_KERNEL_OBJECT,
    [siOwnerSecurityInformation,siGroupSecurityInformation,siDaclSecurityInformation]);
  Writeln(SD.Text);
  writeln('1');
  *)


  {
  Prepare structure for assigning primary token to new process
  }
  processAccessTokenrec.hThread := 0;
  processAccessTokenrec.hToken := NTToken.TokenHandle;

 // Changing the Session Id probably only has effect if
 // we start a new process, changing the sessionid of a process is not
 // possible!
 { SessionId := StrToIntDef(Edit1.Text, 0);
    if not SetTokenInformation(NewTokenHandle, TokenSessionId, @SessionID,
      SizeOf(SessionID)) then
    begin
      Writeln(Format('SetTokenInformation: %s', [SysErrorMessage(GetLastError)]));
    end;  }
 // Memo1.Lines.Add('before ntsetinfoproc');
   writeln('2');
   //NTToken.ImpersonateLoggedOnUser;
  //ImpersonateLoggedOnUser(NewTokenHandle);


  with StartupInfo do
  begin
    cb          := SizeOf(StartupInfo);
    dwFlags     := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    wShowWindow := SW_SHOW;
    lpDesktop := nil;
  end;

  writeln('CreateProcesS');

  {
  Create Process suspended!

  If the window opens only for a second, the process fails because of access denied.
  Try to use cmd.exe instead.

  Warning: using an empty string for CurrentDirectory results in an error. Use nil instead 

  }
 // NTToken.ImpersonateLoggedOnUser; //we cannot impersonate and then user createprocess to start a process in this context
  if not CreateProcess(
      'SecurityID3.exe',//__in_opt     LPCTSTR lpApplicationName,
      //'c:\windows\system32\cmd.exe',
      nil, //__inout_opt  LPTSTR lpCommandLine,
      nil,//LPSECURITY_ATTRIBUTES(InVars.Parameters.lpProcessAttributes), //__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
      nil,//LPSECURITY_ATTRIBUTES(InVars.Parameters.lpThreadAttributes),//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
      true,//__in         BOOL bInheritHandles,
      CREATE_NEW_CONSOLE or CREATE_SUSPENDED, //__in         DWORD dwCreationFlags,
      nil,//__in_opt     LPVOID lpEnvironment,
      {'C:\temp\',}nil,//'',//TJwPChar(InVars.Parameters.lpCurrentDirectory),//__in_opt     LPCTSTR lpCurrentDirectory,
      StartupInfo,//__in         LPSTARTUPINFO lpStartupInfo,
      ProcessInfo //__out        LPPROCESS_INFORMATION lpProcessInformation
  ) then
    RaiseLastOSError;

 // NTToken.RevertToSelf;


  {Exchange primary token of the new process.
  This is safe because the main thread of this process hasn't started yet
  and so it could not get any security context.
  }
  NTCheck(NtSetInformationProcess(ProcessInfo.hProcess, ProcessAccessToken,
   @processAccessTokenrec, sizeOf(ProcessAccessTokenrec)));



  {
  Dump the newly created token.

  Funnily enough this may differ from the output of the created process
  of 'SecurityID3.exe'
  }
  writeln('AccessUser:',NTToken.TokenUser.GetText);
  writeln('AccessOwner:',NTToken.TokenOwner.GetText);
  writeln('Access: ',NTToken.AccessMask);
  writeln('Access: ',JwFormatAccessRights(NTToken.AccessMask,TokenMapping ));

  writeln('3');

  {
  Get the token from the process itself and dump it.

  Again:
  Funnily enough this may differ from the output of the created process
  of 'SecurityID3.exe'
  }
  ProcToken := TJwSecurityToken.CreateTokenByProcess(ProcessInfo.hProcess,TOKEN_ALL_ACCESS);


  nGroups2 := ProcToken.TokenGroups;
  Writeln('Proc groups:',ngroups2.GetText(true));
  {
  Get the token groups directly from the (suspended) process.

  I tried to adapt the groups here but could get it changed????

  }

 { nGroups.Delete(0);
  nGroups.Delete(0);
  ProcToken.TokenGroups := nGroups;  }
 {  Sid := OwnerSid;
  Sid.AttributesType := [sidaGroupEnabled];
  //nGroups2.Add(Sid);
  nGroups2.Clear;

  try
    nGroups2.Delete(nGroups2.FindSid(JwAdministratorsSID));
  except
  end;    }
  {
  Does not work for the new process.
  }
  ProcToken.TokenGroups := nGroups2;

  {
  Again get process token and dump the group to see that nothing has changed.
  }
  ProcToken := TJwSecurityToken.CreateTokenByProcess(ProcessInfo.hProcess,TOKEN_ALL_ACCESS);
  nGroups2 := ProcToken.TokenGroups;
  Writeln('Proc groups22:'#13#10,ngroups2.GetText(true));

  {
  Also output token user and so on.

  We see here that the user is displayed we directly assigned to the token.
  If we set the logon ID to System it also will dump the correct user.
  However the process itself would output the SYSTEM user.
  }
  writeln('OldProcUser:',PRocToken.TokenUser.GEtText);
//  PRocToken.TokenUser := OwnerSid;
//  writeln('NewProcUser:',PRocToken.TokenUser.GetText);
  writeln('OldProcOwner:',PRocToken.TokenOwner.GEtText);
  PRocToken.TokenOwner := OwnerSid;
  writeln('NewProcOwner:',PRocToken.TokenOwner.GetText);


  {
  Now resume process thread
  }
  writeln('resume');
  if ResumeThread(ProcessInfo.hThread) = ERROR_INVALID_HANDLE then
    RaiseLastOSError;
 { NTCheck(NtSetInformationProcess(GetCurrentProcess, ProcessAccessToken,
    @processAccessTokenrec, sizeOf(ProcessAccessTokenrec)));
      
  tok := TJwSecurityToken.CreateTokenByProcess(0,MAXIMUM_ALLOWED);
  //tok := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
  writeln('Access: ',JwFormatAccessRights(tok.AccessMask,TokenMapping ));
  writeln('Access2:',tok.TokenUser.GetText);
                    }
//  CloseHandle(TokenHandle);



  writeln('OK');
 except
   on E : Exception do
     writeln(E.Message);
 end;
 readln;
 NToken.Free;
end;


begin
  {Get all well known sids}
  JwInitWellKnownSIDsExAll();
  { TODO -oUser -cConsole Main : Hier Code einfügen }
  //DoIt2;
  DoIt;
end.




(*

function getFromToken(TokenHandle : THandle; info : TOKEN_INFORMATION_CLASS) : Pointer;
var
  Res : DWORD;
begin
  GetTokenInformation(TokenHandle, info, nil, 0, Res);
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
     Result := GetMemory(Res);
     GetTokenInformation(TokenHandle, info, Result, Res, Res);
  end;
end;   *)


{
original source which was used to adapt
}
         (*
procedure DoIt2;
var TokenHandle : THandle;
  ProcessAccessTokenrec: record
    hToken : THandle;
    hThread : THandle;
  end;
  NewTokenHandle: THandle;
  oa: OBJECT_ATTRIBUTES;
  tokentime: int64;
  user: TOKEN_USER;
  TokenSource: TTokenSource;
  sysLuid : TLuid;
  SessionId: DWORD;
  tok : TJwSecurityToken;
begin
 try
  Win32Check(OpenProcessToken(GetCurrentProcess, TOKEN_ALL_ACCESS, TokenHandle));
  writeln('OpenProcessToken');
  JwEnablePrivilege('SeTcbPrivilege',pst_Enable);
  JwEnablePrivilege('SeCreateTokenPrivilege',pst_Enable);
  JwEnablePrivilege('SeAssignPrimaryTokenPrivilege',pst_Enable);


  ZeroMemory(@oa, sizeOf(OBJECT_ATTRIBUTES));
  oa.Length := sizeOf(OBJECT_ATTRIBUTES);
  tokentime := $FFFFFFFFFFFFFFFF;
  user.User.Sid := JwWorldSID.SID; //JwLocalSystemSID.SID;
  user.User.Attributes := 0;
  TokenSource.SourceName := 'SAMPLE';
  sysLuid := SYSTEM_LUID;
  Win32Check(AllocateLocallyUniqueId(TokenSource.SourceIdentifier));
  writeln('before ntcreatetoken');

  NTCheck(NtCreateToken(@newTokenHandle, TOKEN_ALL_ACCESS, @oa, TokenPrimary,
       @sysLuid,
       @Tokentime,
       @user,
       getFromToken(TokenHandle, TokenGroups),
       getFromToken(TokenHandle, TokenPrivileges),
       nil, //getFromToken(TokenHandle, TokenOwner),
       getFromToken(TokenHandle, TokenPrimaryGroup),
       getFromToken(TokenHandle, TokenDefaultDacl),
       @tokenSource));
    processAccessTokenrec.hThread := 0;
    processAccessTokenrec.hToken := NewTokenHandle;

    // Changing the Session Id probably only has effect if
    // we start a new process, changing the sessionid of a process is not
    // possible!
{    SessionId := StrToIntDef(Edit1.Text, 0);
    if not SetTokenInformation(NewTokenHandle, TokenSessionId, @SessionID,
      SizeOf(SessionID)) then
    begin
      writeln(Format('SetTokenInformation: %s', [SysErrorMessage(GetLastError)]));
    end;}
  writeln('before ntsetinfoproc');

  ImpersonateLoggedOnUser(NewTokenHandle);
  NTCheck(NtSetInformationProcess(GetCurrentProcess, ProcessAccessToken, @processAccessTokenrec, sizeOf(ProcessAccessTokenrec)));

  writeln('GetTok');
  tok := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
  writeln(tok.AccessMask);
  writeln(tok.TokenUser.GetText);
  writeln('Access: ',JwFormatAccessRights(tok.AccessMask,TokenMapping ));

  CloseHandle(TokenHandle);
 except
   on E : Exception do
     writeln(E.Message);
 end;
 readln;
end;       *)

