{This program shows possible access to a file or folder.
Call this program with a parameter.
}
program FileAccessCheck;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  jwsclConstants,
  JwsclMapping,
  JwsclToken,
  JwsclDescriptor,
  JwsclSecureObjects,
  JwsclExceptions,
  JwsclStrings;


function CheckAccessToFile(DesiredAccess: DWORD; const FileName: TJwString): Boolean;
var SD : TJwSecurityDescriptor;
    FileSecurity : TJwSecureFileObject;
begin
  FileSecurity := TJwSecureFileObject.Create(FileName);
  try
    result := FileSecurity.AccessCheck(DesiredAccess);
  finally
    FileSecurity.Free;
  end;
end;

function GetMaximumAccess(const FileName: TJwString): Cardinal;
var SD : TJwSecurityDescriptor;
    FileSecurity : TJwSecureFileObject;
    Privs : TJwPrivilegeSet;
    Status : Boolean;
begin
  FileSecurity := TJwSecureFileObject.Create(FileName);
  try
    FileSecurity.AccessCheck(MAXIMUM_ALLOWED,Privs,result,Status);
  finally
    FileSecurity.Free;
    Privs.Free;
  end;
end;

procedure OutputFileAccess(const GrantedAccess : Cardinal);
var i : Integer;
begin
  writeln('- generic');
  for i := low(FileFolderMapping) to high(FileFolderMapping) do
  begin
    if i = 7 then
      writeln('- specific');
    if i = 17 then
      writeln('- standard');

    if GrantedAccess and FileFolderMapping[i].Right =
      FileFolderMapping[i].Right then
      write('[X] ')
    else
      write('[ ] ');

    //names may vary depeding on resource string contents
    writeln(FileFolderMapping[i].Name);
  end;
end;


begin
  Writeln('This program checks access to file or folder: ', ParamStr(1));
  writeln;
  
  try
    writeln('Full access allowed? ',CheckAccessToFile(GENERIC_ALL,ParamStr(1)));
  except
    on E : EJwsclSecurityException do
      Writeln(E.Message);
  end;

  try
    writeln('Full read access allowed? ',CheckAccessToFile(GENERIC_ALL,ParamStr(1)));
  except
    on E : EJwsclSecurityException do
      Writeln(E.Message);
  end;

  try
    writeln('Allowed Access: ',
      TJwSecurityFileFolderMapping.MapAccessMaskToString(
        GetMaximumAccess(ParamStr(1))));
  except
    on E : EJwsclSecurityException do
      Writeln(E.Message);
  end;

  Writeln('Full list:');
  OutputFileAccess(GetMaximumAccess(ParamStr(1)));

  Writeln('Hit [enter]');
  readln;
end.
