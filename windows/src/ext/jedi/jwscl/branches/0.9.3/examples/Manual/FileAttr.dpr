{This program shows possible access to a file or folder.
Call this program with a parameter.
}
program FileAttr;

{$APPTYPE CONSOLE}

uses
  //FastMM4,
  SysUtils,
  JwaWindows,
  jwsclConstants,
  JwsclMapping,
  JwsclToken,
  JwsclDescriptor,
  JwsclSecureObjects,
  JwsclExceptions,
  JwsclACL,
  JwsclTypes,
  JwsclKnownSid,
  JwsclStrings;

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

var F: Textfile;
    Name : String;

    Token : TJwSecurityToken;
begin
  JwInitWellKnownSIDs;

  try
    Name := ParamStr(0)+'.test';
    AssignFile(f,Name);
    try
      Rewrite(F);
    except
      Reset(F);
    end;
    CloseFile(F);

    writeln(JwFormatAccessRights(GetMaximumAccess(Name),FileFolderMapping));


    Token := TJwSecurityToken.CreateLogonUser(
      'AdminUser','','adminuser',
        LOGON32_LOGON_INTERACTIVE, 0);
        //LOGON32_LOGON_NETWORK_CLEARTEXT, 0);

    Token.ImpersonateLoggedOnUser;

    writeln;
    writeln('1');
    writeln(JwFormatAccessRights(GetMaximumAccess(Name),FileFolderMapping));


    if not SetFileAttributes(TJwPchar(Name),FILE_ATTRIBUTE_READONLY) then
      writeln(GetLastError())
    else
      writeln('OK');
  except
    on E : Exception do
     writeln('Exception:' ,E.Message);
  end;

  //DeleteFile(PChar(Name));
  readln;
end.
