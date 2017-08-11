program SecurityDescriptor2;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  jwsclConstants,
  jwsclTypes,
  JwsclMapping,
  JwsclDescriptor;

var SD : TJwSecurityDescriptor;
begin
  SD := TJwSecurityDescriptor.CreateDefaultByToken();
  writeln(SD.Text);
  readln;

end.
