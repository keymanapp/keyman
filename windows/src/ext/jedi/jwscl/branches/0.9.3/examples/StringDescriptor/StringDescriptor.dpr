program StringDescriptor;

uses
  Forms,
  UMainForm in 'UMainForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
