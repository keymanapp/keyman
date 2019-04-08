program kmbrowserhost;

uses
  Keyman.System.CEFManager in '..\..\global\delphi\chromium\Keyman.System.CEFManager.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas';

{$R *.res}

begin
  FInitializeCEF := TCEFManager.Create(SFolderKeymanEngine);
  try
    FInitializeCEF.Start;
  finally
    FInitializeCEF.Free;
  end;
end.
