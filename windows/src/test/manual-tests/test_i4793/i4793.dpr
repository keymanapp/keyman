program i4793;

uses
  Vcl.Forms,
  test_i4793 in 'test_i4793.pas' {frmI4793};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmI4793, frmI4793);
  Application.Run;
end.
