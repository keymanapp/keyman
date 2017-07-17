{This program shows possible access to a file or folder.
Call this program with a parameter.
}
program FileDACL;

{$APPTYPE CONSOLE}

uses
  //FastMM4,
  SysUtils,
  Dialogs,
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



var F: Textfile;
    Name : String;

    FileSecurity : TJwSecureFileObject;
begin
  JwInitWellKnownSIDs;

  FileSecurity := TJwSecureFileObject.Create('C:\Windows');
  ShowMessage(FileSecurity.DACL.GetTextMap(TJwSecurityFileFolderMapping));
  FileSecurity.Free;

  readln;
end.
