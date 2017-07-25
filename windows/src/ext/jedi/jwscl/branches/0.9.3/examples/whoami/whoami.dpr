{
Whoami is a command line application that outputs 
information about the current user.

Version: 0.5
Release: 1.10.2007
Written by Christian Wimmer

Notes:
This example may raises lots of exceptions in the Delphi IDE but should work
fine when started from the Windows Explorer or command line.
Raising exceptions in the Delphi is normal because even catched exception
will be displayed in the Delphi IDE. You can change this behaviour in
Tools-Debugger options-> Language exceptions 

Known bugs:
This example will not work on Windows XP Sp1 due to a unsupported
parameter in GetTokenInformation. This example must be adapted to
check for the occuring exception.
}
program whoami;


{$APPTYPE CONSOLE}

uses
  SysUtils,
  jwaWindows,
  JwsclSecureObjects,
  JwsclACL,
  JwsclTypes,
  JwsclMapping,
  JwsclToken,
  JwsclSID,
  JwsclWinStations,
  JwsclConstants,
  JwsclStrings
  ;

function DumpEnvironmentW(lpEnvironment: Pointer) : WideString;
var
  Env: PWideChar;
begin
  result := '';
  Env := lpEnvironment;
  while (lstrlenW(Env) > 0) do
  begin
    if WideString(Env)[1] <> '=' then
     result := result + #13#10 + WideString(Env);
    Env := PWideChar(DWORD(Env) + DWORD(lstrlenW(Env) + 1) * DWORD(sizeof(Env^)));
  end;
 // Delete(result,1,2)
end;

procedure ShowWinstationDACL;
var hAWinst : HWINSTA;
    anOwner, aGroup : TJwSecurityId;
    deskDACL,
    DACL : TJwDAccessControlList;
    aSACL : TJwSAccessControlList;
    hADesk : HDESK;
begin
  hAWinst := OpenWindowStation(
             'winsta0',
             FALSE,
             MAXIMUM_ALLOWED   //damit auch später ShowMessage noch funkz - nach SetProcessWindowStation
             //READ_CONTROL or WRITE_DAC
             );


  hADesk := OpenDesktop(
             'default',
             0,
             FALSE,
             READ_CONTROL or
             WRITE_DAC or
             DESKTOP_WRITEOBJECTS or DESKTOP_READOBJECTS);


  try
    //TJwSecureGeneralObject.GetSecurityInfo(
    //    hAWinst,SE_WINDOW_OBJECT,[siDaclSecurityInformation],anOwner,aGroup,DACL,aSACL);
    TJwSecureGeneralObject.GetSecurityInfo(hAWinst,SE_WINDOW_OBJECT,[siDaclSecurityInformation],anOwner,aGroup,DACL,aSACL);


    writeln;
    writeln('**********************');
    writeln('WinStation DACL');
    writeln('**********************');
    writeln(DACL.GetTExtMap(TJwSecurityWinStationMapping));
    DACL.Free;

  except
    on e : Exception do
      writeln(e.Message);
  end;

  try
    TJwSecureGeneralObject.GetSecurityInfo(
        hADesk,SE_WINDOW_OBJECT,[siDaclSecurityInformation],anOwner,aGroup,deskDACL,aSACL);
        writeln;
    writeln('**********************');
    writeln('Desktop DACL');
    writeln('**********************');
    writeln(deskDACL.GetTExtMap(TJwSecurityDesktopMapping));
    //writeln(deskDACL.GetTExtMap(TJwSecurityGenericMapping));

     deskDACL.Free;

  except
    on e : Exception do
      writeln(e.Message);
  end;
end;

procedure ShowTokenGroups;
var Token : TJwSecurityToken;
    TokenGroups : TJwSecurityIdList;
    i,i2 : Integer;
    s : TJwString;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_QUERY or TOKEN_READ);
  try
    TokenGroups := Token.TokenGroups;

    writeln;
    writeln('***********************');
    writeln('Token groups (count: '+IntToStr(TokenGroups.Count)+')');
    writeln('***********************');

    for i := 0 to TokenGroups.Count-1 do
    begin
      try
        s := TokenGroups[i].AccountName[''];
      except
        s := TokenGroups[i].StringSID;
      end;
      Writeln(' '+s+ ', '+#9+TokenGroups[i].GetAttributeString(JwSidAttributeHumanStrings));
    end;
  finally
    FreeAndNil(Token);
  end;
  writeln;
  writeln;
end;

var Token : TJwSecurityToken;
    SID : TJwSecurityId;
    Privs : TJwPrivilegeSet;

    Name : Array[0..255] of char;
    n : CArdinal;
    sourcename : shortString;
    sourceLuid : TLuid;
    groups : TJwSecurityIdList;
    lpEnv : Pointer;
begin
  {Should change Code Page to display characters correctly.
   if not SetConsoleOutputCP(xxx) then
    RaiseLastOSError;}
    
  { TODO -oUser -cConsole Main : Hier Code einfügen }
  try
    Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);//TOKEN_QUERY or TOKEN_READ);


  SID := Token.TokenOwner;

  writeln('Logged on as : '+SID.AccountName['']);
  writeln('Token source LUID (h:l): 0x'+IntToHex(Token.GetTokenOrigin.highPart,2)+':0x'+IntToHex(Token.GetTokenOrigin.LowPart,2));
  Token.GetTokenSource(sourcename,sourceLuid);
  writeln('Sourcename~SourceLUID: '+SourceName+'~0x'+IntToHex(sourceLuid.highPart,2)+':0x'+IntToHex(sourceLuid.LowPart,2));

  writeln(Token.GetTokenStatistics.GetText);

  //
  n := sizeof(Name);
  GetUserName(@Name,n);
  writeln;
  writeln('Call to GetUserName   : '+String(Name));

  n := sizeof(Name);
  GetComputerName(@Name,n);
  writeln('Call to GetComputerName   : '+String(Name));
  writeln;

  ShowTokenGroups;



//  writeln('Eigene Dateien Pfad    : '+ GetSpecDirectory(SD_PERSONAL));  //aus DelphiTools


  Privs := Token.GetTokenPrivileges;
  writeln;
  if Assigned(Privs.PrivByName[SE_TCB_NAME]) then
  begin
    writeln('!!!!!! Principal is member of Trusted Computing Base!!!!!!!!!');
  end;



  writeln;
  writeln('***********************');
  writeln('Privileges : ');
  writeln('***********************');
  writeln(Privs.GetText);



  writeln;
  writeln('***********************');
  writeln('Principals user environment : ');
  writeln('***********************');
  lpEnv := nil;
  if CreateEnvironmentBlock(@lpEnv,Token.TokenHandle,false) then
    writeln(DumpEnvironmentW(lpEnv));

  ShowWinstationDACL;

  SID.Free;
  Token.Free;
  except
    on e : Exception do
    begin
      writeln(E.Message);
      writeln('[Hit return key]');
      readln;
      exit;
    end;
  end;

  writeln;
  writeln;
  writeln('[Hit return key]');
  readln;
end.
