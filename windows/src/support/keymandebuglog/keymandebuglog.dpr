program keymandebuglog;

uses
  Vcl.Forms,
  UfrmKeymanDebugLogMain in 'UfrmKeymanDebugLogMain.pas' {frmDebugLog},
  DebugManager in '..\..\global\delphi\debug\DebugManager.pas',
  UserMessages in '..\..\..\..\common\windows\delphi\general\UserMessages.pas',
  ErrorControlledRegistry in '..\..\..\..\common\windows\delphi\vcl\ErrorControlledRegistry.pas',
  RegistryKeys in '..\..\..\..\common\windows\delphi\general\RegistryKeys.pas',
  KeymanVersion in '..\..\..\..\common\windows\delphi\general\KeymanVersion.pas',
  SystemDebugPath in '..\..\global\delphi\general\SystemDebugPath.pas',
  Unicode in '..\..\..\..\common\windows\delphi\general\Unicode.pas',
  utilstr in '..\..\..\..\common\windows\delphi\general\utilstr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDebugLog, frmDebugLog);
  Application.Run;
end.
