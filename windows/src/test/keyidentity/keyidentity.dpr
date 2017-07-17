program keyidentity;

uses
  Vcl.Forms,
  UfrmKeyIdentity in 'UfrmKeyIdentity.pas' {Form1},
  UfrmKeyIdentityLog in 'UfrmKeyIdentityLog.pas' {frmKeyIdentityLog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmKeyIdentityLog, frmKeyIdentityLog);
  Application.Run;
end.
