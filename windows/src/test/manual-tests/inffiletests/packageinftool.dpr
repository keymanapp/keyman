program packageinftool;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  PackageInfo in '..\..\..\..\common\windows\delphi\packages\PackageInfo.pas',
  utilfiletypes in '..\..\..\..\common\windows\delphi\general\utilfiletypes.pas',
  utilstr in '..\..\..\..\common\windows\delphi\general\utilstr.pas',
  StockFileNames in '..\..\..\..\common\windows\delphi\general\StockFileNames.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  VersionInfo in '..\..\..\..\common\windows\delphi\general\VersionInfo.pas',
  utilsystem in '..\..\..\..\common\windows\delphi\general\utilsystem.pas',
  utilexecute in '..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  utildir in '..\..\..\..\common\windows\delphi\general\utildir.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  GetOsVersion in '..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  kpsfile in '..\..\..\..\developer\src\common\delphi\packages\kpsfile.pas',
  PackageFileFormats in '..\..\..\..\common\windows\delphi\packages\PackageFileFormats.pas',
  packageinf in 'packageinf.pas',
  kmpinffile in '..\..\..\..\common\windows\delphi\packages\kmpinffile.pas',
  JsonUtil in '..\..\..\..\common\windows\delphi\general\JsonUtil.pas';

begin
  try
    run
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
