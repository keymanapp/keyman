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
  RegistryKeys in '..\..\general\RegistryKeys.pas',
  KeymanVersion in '..\..\general\KeymanVersion.pas',
  utilsystem in '..\..\general\utilsystem.pas',
  utilexecute in '..\..\general\utilexecute.pas',
  Unicode in '..\..\general\Unicode.pas',
  GetOsVersion in '..\..\general\GetOsVersion.pas',
  SourceRootPath in 'SourceRootPath.pas',
  Keyman.System.DevTools.BuildMessageConstants in 'Keyman.System.DevTools.BuildMessageConstants.pas',
  Keyman.System.DevTools.BuildSetupStringTranslations in 'Keyman.System.DevTools.BuildSetupStringTranslations.pas',
  SetupStrings in '..\..\..\..\..\windows\src\desktop\setup\SetupStrings.pas',
  Keyman.System.AndroidStringToKeymanLocaleString in '..\..\general\Keyman.System.AndroidStringToKeymanLocaleString.pas',
  Keyman.System.DevTools.BuildLocaleIndex in 'Keyman.System.DevTools.BuildLocaleIndex.pas';

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
