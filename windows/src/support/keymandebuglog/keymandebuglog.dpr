program keymandebuglog;

uses
  Vcl.Forms,
  UfrmKeymanDebugLogMain in 'UfrmKeymanDebugLogMain.pas' {frmDebugLog},
  DebugManager in '..\..\global\delphi\debug\DebugManager.pas',
  UserMessages in '..\..\global\delphi\general\UserMessages.pas',
  ErrorControlledRegistry in '..\..\global\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\global\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\global\delphi\general\KeymanVersion.pas',
  SystemDebugPath in '..\..\global\delphi\general\SystemDebugPath.pas',
  Unicode in '..\..\global\delphi\general\Unicode.pas',
  utilstr in '..\..\global\delphi\general\utilstr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDebugLog, frmDebugLog);
  Application.Run;
end.
