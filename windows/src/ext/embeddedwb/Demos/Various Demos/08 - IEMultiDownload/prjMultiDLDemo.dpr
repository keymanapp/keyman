program prjMultiDLDemo;

uses
  Forms,
  utMultiDL in 'utMultiDL.pas' {frmMultiDownloader};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMultiDownloader, frmMultiDownloader);
  Application.Run;
end.

