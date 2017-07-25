{This program shows possible access to a file or folder.
Call this program with a parameter.
}
program FileInheritance;

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
    SD:  TJwSecurityDescriptor;
begin
  JwInitWellKnownSIDs;

  FileSecurity := TJwSecureFileObject.Create('P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\examples\Manual\InhFlow');
  SD := FileSecurity.GetSecurityDescriptor([siDaclSecurityInformation]);


  if SD.InheritanceDACLProtection = aclpProtected then
    writeln('geschützt')
  else
    writeln('UNgeschützt');

  SD.InheritanceDACLProtection := aclpForceUnprotect;

  FileSecurity.SetSecurityDescriptor(SD,[siDaclSecurityInformation]);

  FileSecurity.Free;

  readln;
end.
