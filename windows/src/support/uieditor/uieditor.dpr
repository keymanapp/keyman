program uieditor;

uses
  Forms,
  UfrmLocaleEditor in 'UfrmLocaleEditor.pas' {frmLocaleEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmLocaleEditor, frmLocaleEditor);
  Application.Run;
end.
