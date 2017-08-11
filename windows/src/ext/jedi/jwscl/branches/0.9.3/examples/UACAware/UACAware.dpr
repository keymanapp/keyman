program UACAware;

{$APPTYPE CONSOLE}
{$R manifest.res}

uses
  SysUtils,
  jwaWindows,
  jwsclToken, jwsclLsa, jwsclSid, jwsclTypes, jwsclImpersonation,jwsclStrings;

const Name = 'ok.txt';
var F : TextFile;
begin
  writeln('enter...');
  Sleep(4000);
  AssignFile(f,Name);
  if FileExists(Name) then
    Append(f)
  else
    Rewrite(f);

  Writeln(f,TimeToStr(now),' Admin: ',JwCheckAdministratorAccess );
  CloseFile(f);
  { TODO -oUser -cConsole Main : Hier Code einfügen }
end.
