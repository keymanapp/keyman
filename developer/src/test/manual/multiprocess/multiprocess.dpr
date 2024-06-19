program multiprocess;

uses
  Vcl.Forms,
  Keyman.MultiProcess.UI.UfrmMultiProcess in 'Keyman.MultiProcess.UI.UfrmMultiProcess.pas' {frmMultiProcess},
  Keyman.Developer.System.MultiProcess in '..\..\..\tike\main\Keyman.Developer.System.MultiProcess.pas',
  utilexecute in '..\..\..\..\..\common\windows\delphi\general\utilexecute.pas',
  Unicode in '..\..\..\..\..\common\windows\delphi\general\Unicode.pas';

{$R *.res}

begin
  CreateMultiProcessCoordinator(TfrmMultiProcess.ClassName, 'Software\Keyman\Test\MultiProcess');
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMultiProcess, frmMultiProcess);
  Application.Run;
end.
