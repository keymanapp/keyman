program del_uuidgen;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  main in 'main.pas',
  crypt_guid in '..\..\..\global\delphi\productactivation\crypt_guid.pas',
  DCPcrypt2 in '..\..\..\global\delphi\crypt\DCPcrypt2.pas',
  DCPsha1 in '..\..\..\global\delphi\crypt\Hashes\DCPsha1.pas',
  DCPbase64 in '..\..\..\global\delphi\crypt\DCPbase64.pas',
  DCPconst in '..\..\..\global\delphi\crypt\DCPconst.pas';

begin
  { TODO -oUser -cConsole Main : Insert code here }
  Run;
end.
