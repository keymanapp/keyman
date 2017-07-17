program ReadFileSecurity;

{$APPTYPE CONSOLE}

uses
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

function CheckAccessToFile(
  DesiredAccess: DWORD; const FileName: WideString): Boolean;
var FileObject : TJwSecureFileObject;
begin
  FileObject := TJwSecureFileObject.Create(FileName);
  try
    result := FileObject.AccessCheck(DesiredAccess);
  finally
    FileObject.Free;
  end;
end;

function CheckMaximumAccessToFile(const FileName: WideString): DWORD;
var FileObject : TJwSecureFileObject;
    PrivilegeSet: TJwPrivilegeSet;
    AccessStatus: boolean;
begin
  FileObject := TJwSecureFileObject.Create(FileName);
  try
    FileObject.AccessCheck(
      MAXIMUM_ALLOWED,//DesiredAccess: TJwAccessMask;
      PrivilegeSet,//out PrivilegeSet: TJwPrivilegeSet;
      result,//out GrantedAccess: TJwAccessMask;
      AccessStatus,//out AccessStatus: boolean;
      nil//const ClientToken: TJwSecurityToken = nil);
      );
  finally
    PrivilegeSet.Free;
    FileObject.Free;
  end;
end;

var AccessMask : DWORD;
    FileName : String;
begin
  FileName := ParamStr(1);
  if Length(FileName) = 0 then
    FileName := ParamStr(0);

  writeln('Check access for '+FileName);
  try
    if CheckAccessToFile(FILE_ALL_ACCESS,FileName) then
      writeln('Full control allowed')
    else
      writeln('Full control denied');
  except
    On E : Exception do
     Writeln(E.Message);
  end;

  try
    AccessMask := CheckMaximumAccessToFile(FileName);
    writeln('Maximum access possible: ['+TJwSecurityFileMapping.MapAccessMaskToString(AccessMask)+']');
  except
    On E : Exception do
     Writeln(E.Message);
  end;

  Writeln('[Hit return]');
  readln;
end.
