program buildunidata;

{$APPTYPE CONSOLE}

uses
  ActiveX,
  SysUtils,
  buildunidata_main in 'buildunidata_main.pas',
  TTInfo in '..\..\..\..\..\common\windows\delphi\general\TTInfo.pas',
  UnicodeData in '..\..\charmap\UnicodeData.pas',
  ADOX_TLB in '..\..\..\..\..\common\windows\delphi\tlb\ADOX_TLB.pas',
  ADODB_TLB in '..\..\..\..\..\common\windows\delphi\tlb\ADODB_TLB.pas',
  utilsystem in '..\..\..\..\..\common\windows\delphi\general\utilsystem.pas',
  utilstr in '..\..\..\..\..\common\windows\delphi\general\utilstr.pas',
  Unicode in '..\..\..\..\..\common\windows\delphi\general\Unicode.pas',
  GetOsVersion in '..\..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  RegistryKeys in '..\..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  JRO_TLB in '..\..\..\..\..\common\windows\delphi\tlb\JRO_TLB.pas',
  ErrorControlledRegistry in '..\..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  utilexecute in '..\..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  KeymanVersion in '..\..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  utildir in '..\..\..\..\..\common\windows\delphi\general\utildir.pas';

begin
  CoInitializeEx(nil, 0);
  Run;
  CoUninitialize;
end.
