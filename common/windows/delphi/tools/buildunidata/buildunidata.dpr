program buildunidata;

{$APPTYPE CONSOLE}

uses
  ActiveX,
  SysUtils,
  buildunidata_main in 'buildunidata_main.pas',
  TTInfo in '..\..\..\..\..\windows\src\global\delphi\general\TTInfo.pas',
  UnicodeData in '..\..\..\..\..\windows\src\global\delphi\charmap\UnicodeData.pas',
  ADOX_TLB in '..\..\..\..\..\windows\src\global\delphi\tlb\ADOX_TLB.pas',
  ADODB_TLB in '..\..\..\..\..\windows\src\global\delphi\tlb\ADODB_TLB.pas',
  utilsystem in '..\..\..\..\..\windows\src\global\delphi\general\utilsystem.pas',
  utilstr in '..\..\..\..\..\windows\src\global\delphi\general\utilstr.pas',
  Unicode in '..\..\..\..\..\windows\src\global\delphi\general\Unicode.pas',
  GetOsVersion in '..\..\..\..\..\windows\src\global\delphi\general\GetOsVersion.pas',
  RegistryKeys in '..\..\..\..\..\windows\src\global\delphi\general\RegistryKeys.pas',
  JRO_TLB in '..\..\..\..\..\windows\src\global\delphi\tlb\JRO_TLB.pas',
  ErrorControlledRegistry in '..\..\..\..\..\windows\src\global\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\..\..\..\windows\src\global\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\..\..\..\windows\src\global\delphi\general\KeymanVersion.pas',
  utildir in '..\..\..\..\..\windows\src\global\delphi\general\utildir.pas';

begin
  CoInitializeEx(nil, 0);
  Run;
  CoUninitialize;
end.
