unit Downloader;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  SysUtils, StdCtrls, ExtCtrls, IEDownload, ComCtrls;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    edtAddress: TEdit;
    ProgressBar1: TProgressBar;
    memDL: TMemo;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure MyProgress(Sender: TBSCB; ulProgress, ulProgressMax,
      ulStatusCode, FileSize: ULONG; szStatusText: LPCWSTR; Downloaded,
      ElapsedTime, Speed, RemainingTime, Status, Percent: string);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  IED: TIEDownload;
begin
  IED := TIEDownload.Create(Self);
  try
    with IED do
    begin
      OpenDownloadFolder := True;
      OnProgress := MyProgress;
      Form2.Caption := 'Downloading from: ' + edtAddress.Text;
      Go(edtAddress.Text);
      with memDL.Lines do
      begin
        Clear;
        Add('Downloaded File: ' + DownloadedFile);
        Add('Download Folder: ' + DownloadFolder);
        Add('File Name: ' + FileName);
        Add('Server IP: ' + ServerIP);
        Add('Server Address: ' + ServerAddress);
        Add('File type: ' + MimeType);
        Add('File extension: ' + FileExtension);
        Add('File Size: ' + FormatSize(FileSize));
      end;
    end;
  finally
    IED.Free;
    Form2.Caption := 'Done.';
  end;
end;

procedure TForm2.MyProgress(Sender: TBSCB; ulProgress, ulProgressMax,
  ulStatusCode, FileSize:  ULONG; szStatusText: LPCWSTR; Downloaded,
  ElapsedTime, Speed, RemainingTime, Status, Percent: string);
begin
  ProgressBar1.Max := ulProgressMax;
  ProgressBar1.Position := ulProgress;
  Label2.Caption := 'Percents: ' + Percent;
end;

end.

