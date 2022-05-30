program SecurityID3;

{$APPTYPE CONSOLE}

uses
  sysutils,
  JwaWindows,
  jwsclConstants,
  jwsclTypes,
  JwsclMapping,
  JwsclSid,
  JwsclToken,
  JwsclResource,
  JwsclUtils,
  JwsclAcl,
  JwsclDescriptor;

procedure PrintTokenInfo(const Token : TJwSecurityToken);
var SID, SID2 : TJwSecurityID;
begin
  writeln('Access: ',JwFormatAccessRights(Token.AccessMask,TokenMapping ));

  Sid := Token.TokenUser;
  writeln('TokenUser: ',SID.GetText(true));
  Sid.Free;

  Sid := Token.TokenOwner;
  writeln('TokenOwner: ',SID.GetText(true));
  Sid.Free;

  writeln('TokenGroups: '#13#10,Token.TokenGroups.GetText(true));
end;

var
    Token,LToken : TJwSecurityToken;
    i : Cardinal;
    p : array[0..1000] of char;
begin
  try

  Token := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
  PrintTokenInfo(Token);

  i := 1000;
  GetUserNameA(@p,i);
  writeln('UserName:',p);



  Writeln('*********** Linked Token ************');
  try
    LToken := Token.GetLinkedToken;
    PrintTokenInfo(LToken);
  except
    On E : Exception do
      writeln('not found: ',E.Message);
  end;


  except
   on E : Exception do
     Writeln(E.message);
  end;

  readln;
  Token.Free;

end.
