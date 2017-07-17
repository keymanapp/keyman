program isadmin;

uses
  main in 'main.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  KeymanPaths in '..\..\global\delphi\general\KeymanPaths.pas',
  DebugPaths in '..\..\global\delphi\general\DebugPaths.pas';

{$R VERSION.RES}

begin
  Run;
end.
