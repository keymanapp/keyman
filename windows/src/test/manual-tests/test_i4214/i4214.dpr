program i4214;

uses
  Vcl.Forms,
  Ufrm4214Main in 'Ufrm4214Main.pas' {Form1},
  Ufrm4214Dialog in 'Ufrm4214Dialog.pas' {frm4214Dialog},
  Ufrm4214DialogWebBrowser in 'Ufrm4214DialogWebBrowser.pas' {frm4214DialogWebBrowser},
  Ufrm4214DialogEWB in 'Ufrm4214DialogEWB.pas' {frm4214DialogEWB},
  Vcl.OleCtrls in '..\..\global\delphi\vcl\Vcl.OleCtrls.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
