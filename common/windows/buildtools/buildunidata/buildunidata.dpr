program buildunidata;

{$APPTYPE CONSOLE}

uses
  ActiveX,
  SysUtils,
  buildunidata_main in 'buildunidata_main.pas',
  TTInfo in '..\..\global\delphi\general\TTInfo.pas',
  UnicodeData in '..\..\global\delphi\charmap\UnicodeData.pas',
  ADOX_TLB in '..\..\global\delphi\tlb\ADOX_TLB.pas',
  ADODB_TLB in '..\..\global\delphi\tlb\ADODB_TLB.pas',
  utilsystem in '..\..\global\delphi\general\utilsystem.pas',
  utilstr in '..\..\global\delphi\general\utilstr.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  GetOsVersion in '..\..\global\delphi\general\GetOsVersion.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  JRO_TLB in '..\..\global\delphi\tlb\JRO_TLB.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\global\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  utildir in '..\..\global\delphi\general\utildir.pas';

begin
  CoInitializeEx(nil, 0);
  Run;
  CoUninitialize;
end.
