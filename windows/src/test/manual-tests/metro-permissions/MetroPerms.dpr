program MetroPerms;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Keyman.System.Test.MetroPermissions in 'Keyman.System.Test.MetroPermissions.pas',
  Keyman.System.Security in '..\..\..\global\delphi\general\Keyman.System.Security.pas',
  RegistryKeys in '..\..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  utilsystem in '..\..\..\..\..\common\windows\delphi\general\utilsystem.pas',
  Keyman.Winapi.VersionHelpers in '..\..\..\global\delphi\winapi\Keyman.Winapi.VersionHelpers.pas',
  KeymanVersion in '..\..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  GetOsVersion in '..\..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  utilexecute in '..\..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  Unicode in '..\..\..\..\..\common\windows\delphi\general\Unicode.pas';

begin
  try
    EnginePostInstall;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
