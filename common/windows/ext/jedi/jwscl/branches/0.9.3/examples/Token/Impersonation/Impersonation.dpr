{This demo shows how to use LogonUser, ImpersonateLoggedOnUser and the
class TJwCredentialsPrompt.
It gets your logon information (Administrator by default) and tries to log you on
and tell your name.

Be aware that Windows Vista disables the Administrator account.
}
program Impersonation;

{.$APPTYPE CONSOLE}

uses
{$IFDEF FPC}
  interfaces,
  Forms,
{$ENDIF}
  SysUtils,
  jwaWindows,

  JwsclToken,
  JwsclTypes,
  JwsclCredentials,
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

begin
{$IFDEF FPC}
  Application.Initialize;  //LCL needs initialization
{$ENDIF}
  {create token by thread or process.
   Only process token is set!

   }
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);

  Prompt := TJwCredentialsPrompt.Create;
  Prompt.UserName := 'Administrator';
  Prompt.Password := '123'; 
  Prompt.Caption := 'Impersonation demonstration';
  Prompt.MessageText := 'Enter a account name and passwort to impersonate that user.';
  Prompt.Flags := [cfFlagsDoNotPersist];

  try

    if not Prompt.ShowModal(false) then
      exit;

    TJwCredentialsTools.ParseUserName(Prompt.UserName, sUser, sDomain);
  except
    FreeAndNil(Prompt);
    FreeAndNil(Token);

    raise;
  end;

  try
     Token := TJwSecurityToken.CreateLogonUser(sUser, sDomain, Prompt.Password, LOGON32_LOGON_INTERACTIVE,0);
  except
    //do error checking
    raise;
  end;

  ShowUserName('Before impersonation');

   Token.ImpersonateLoggedOnUser; //impersonate user

  ShowUserName('After impersonation');

   Token.RevertToSelf;


  ShowUserName('Revert to self ');

  Token.Free;

end.
