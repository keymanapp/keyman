program prjDownloaderDemo;

uses
  Forms,
  Downloader in 'Downloader.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

