(*
This demonstration shows how to change the owner of a file
and add a full access ACE to the file.

Remarks:
 This demonstration does not free all created objects if an error occurs (TODO)
*)

program ChangeOwner;
{$APPTYPE CONSOLE}

uses
{$IFDEF FPC}
  interfaces,
  Forms,
{$ENDIF}
  SysUtils,
  Dialogs,
  Controls,
  JwaWindows,
  JwsclTypes,
  JwsclExceptions,
  JwsclConstants,
  JwsclMapping,
  JwsclVersion,
  JwsclProcess,
  JwsclSid,
  JwsclAcl,
  JwsclCredentials,
  JwsclDescriptor,
  JwsclToken,
  JwsclKnownSid,
  JwsclAccounts,
  JwsclSecureObjects,
  JwsclStrings;


procedure ShowUserName(Caption : String);
var
    pLoggedOnUserName : array[0..255] of char;
    iSize : Cardinal;
    str : String;
begin
    FillChar(pLoggedOnUserName,sizeof(pLoggedOnUserName),0);
    iSize := sizeof(pLoggedOnUserName);
    GetUserName(@pLoggedOnUserName, iSize);
    str := 'GetUserName: Your username is : ' + String(pLoggedOnUserName);
    MessageBox(0,PChar(str),PChar(Caption),MB_OK);
end;

var Token : TJwSecurityToken;
    Prompt : TJwCredentialsPrompt;
    sUser, sDomain, sPass : TJwString;

    FileName : String;

    fSecureFileObjects : TJwSecureFileObject;

    adminUser,
    oldOwner,
    anOwner : TJwSecurityId;
    anGroup : TJwSecurityId;
    aDACL : TJwDAccessControlList;
    aSACL : TJwSAccessControlList;
begin
{$IFDEF FPC}
  Application.Initialize;  //LCL needs initialization
{$ENDIF}
  
  FileName := ParamStr(1);

  if not FileExists(FileName) then
  begin
    MessageDlg('File not found.',mtError,[mbok],0);
    exit;
  end;

  try
    Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
  except
    on E : Exception do
    begin
      MessageDlg(E.Message,mtError,[mbok],0);
      exit;
    end
    else
      raise;
  end;

  if (not Token.PrivilegeAvailable[SE_TAKE_OWNERSHIP_NAME]) then
  begin
    Token.Free;

    //create user logon prompt dialog
    Prompt := TJwCredentialsPrompt.Create;
    Prompt.UserName := 'Administrator';
    Prompt.Password := '';
    Prompt.Caption := 'Take ownership demonstration';
    Prompt.MessageText := 'Logon as an User with privilege SeTakeOwnershipPrivilege (usually Administrator) to get ownership of a file and add full access to it.';
    Prompt.Flags := [cfFlagsDoNotPersist];

    try
      if not Prompt.ShowModal(false) then
      begin
        FreeAndNil(Prompt);
        MessageDlg('Demonstration cancelled.',mtInformation,[mbok],0);
        exit;
      end;

      //parse domain and username from Prompt.UserName
      TJwCredentialsTools.ParseUserName(Prompt.UserName, sUser, sDomain);
    except
      FreeAndNil(Prompt);
      raise;
    end;

    //Logon user
    try
       Token := TJwSecurityToken.CreateLogonUser(sUser, sDomain, Prompt.Password, LOGON32_LOGON_INTERACTIVE,0);
    except
      on E : EJwsclSecurityException do
      begin
        MessageDlg(E.Message,mtError,[mbok],0);
        exit;
      end
      else
        raise;
    end;
  end;


  //we need the thread to impersonate the logged on user, so we can change ownership of the file
  Token.ConvertToImpersonatedToken(jwaWindows.SecurityImpersonation,
   TOKEN_WRITE or
   TOKEN_READ or
   TOKEN_ADJUST_PRIVILEGES or
   TOKEN_IMPERSONATE);
  Token.SetThreadToken(0);

  //if not Token.PrivilegeAvailable[SE_TAKE_OWNERSHIP_NAME] then  //or
  if not JwIsPrivilegeSet(SE_TAKE_OWNERSHIP_NAME,pqt_Available) then
  begin
    MessageDlg('You need administrator rights.',mtError,[mbok],0);
    Token.Free;
    exit;
  end;

  
  ShowUserName('Logged on user');

  //we need it only if we dont have WRITE_OWNER and WRITE_DAC rights to the file
  //in this case we could change the owner of the file and reset the dacl so we can get full access to it
  Token.PrivilegeEnabled[SE_TAKE_OWNERSHIP_NAME] := true;


  if (MessageDlg(Format('Do you really want to to become the owner of and get full access to the file : "%s" ?',[FileName]),mtConfirmation,[mbYes,mbNo],0) = mrNo) then
  begin
    Token.Free;
    exit;
  end;

  //open file to change security information
  fSecureFileObjects := TJwSecureFileObject.Create(FileName);

  //Get Owner
  try
    anOwner := fSecureFileObjects.GetTempOwner;
  except
    on E : EJwsclSecurityException do
    begin
      MessageDlg(E.Message,mtError,[mbok],0);
      exit;
    end
    else
      raise;
  end;

  //Get DACL
  try
    aDACL := fSecureFileObjects.GetTempDACL;
  except
    on E : EJwsclSecurityException do
    begin
      MessageDlg(E.Message,mtError,[mbok],0);
      exit;
    end
    else
      raise;
  end;

  //get thread user (logged on user)
  adminUser := Token.GetTokenUser; //
  try
    fSecureFileObjects.SetOwner(adminUser);
  except
    on E : EJwsclSecurityException do
    begin
      MessageDlg(E.Message,mtError,[mbok],0);
      exit;
    end
    else
      raise;
  end;

  //Add full access to the file for the logged on user
  aDACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(
        nil,   //List owner - is set automatically here
        [],    //ACE inherit flags
        FILE_ALL_ACCESS ,  //desired access
        adminUser,  //SID
        false)); //automatically free SID ?

  try
    fSecureFileObjects.SetDACL(aDACL);
  except
    on E : EJwsclSecurityException do
    begin
      MessageDlg(E.Message,mtError,[mbok],0);
      exit;
    end
    else
      raise;
  end;

  fSecureFileObjects.Free;
  adminUser.Free;
  Token.Free;

  MessageDlg('Access granted. Demonstration has ended. Use the Windows Explorer to see the result.',mtInformation,[mbok],0);

end.
