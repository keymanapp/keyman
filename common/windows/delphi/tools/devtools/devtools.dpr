program devtools;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DevIncludePaths in 'DevIncludePaths.pas',
  DevInstallPackages in 'DevInstallPackages.pas',
  DevUtils in 'DevUtils.pas',
  DevReleaseBuildCheck in 'DevReleaseBuildCheck.pas',
  DevCheckGitStatus in 'DevCheckGitStatus.pas',
  RegistryKeys in '..\..\..\..\..\windows\src\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\..\windows\src\global\delphi\general\KeymanVersion.pas',
  utilsystem in '..\..\..\..\..\windows\src\global\delphi\general\utilsystem.pas',
  utilexecute in '..\..\..\..\..\windows\src\global\delphi\general\utilexecute.pas',
  Unicode in '..\..\..\..\..\common\windows\delphi\general\Unicode.pas',
  GetOsVersion in '..\..\..\..\..\common\windows\delphi\general\GetOsVersion.pas',
  SourceRootPath in 'SourceRootPath.pas',
  Keyman.System.DevTools.BuildMessageConstants in 'Keyman.System.DevTools.BuildMessageConstants.pas',
  Keyman.System.DevTools.BuildSetupStringTranslations in 'Keyman.System.DevTools.BuildSetupStringTranslations.pas',
  SetupStrings in '..\..\..\..\..\windows\src\desktop\setup\SetupStrings.pas',
  Keyman.System.AndroidStringToKeymanLocaleString in '..\..\..\..\..\windows\src\global\delphi\general\Keyman.System.AndroidStringToKeymanLocaleString.pas';

begin
  try
    DevUtils.Run;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 5;
    end;
  end;
end.
