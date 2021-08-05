program i4208;

uses
  Vcl.Forms,
  i4208main in 'i4208main.pas' {Form1},
  KeymanEmbeddedWB in '..\..\global\delphi\comp\KeymanEmbeddedWB.pas',
  UfrmKeymanBase in 'UfrmKeymanBase.pas' {frmKeymanBase},
  UfrmWebContainer in 'UfrmWebContainer.pas' {frmWebContainer},
  UfrmHelp in 'UfrmHelp.pas' {frmHelp};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
