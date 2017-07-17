//This cool demo was build by BitMaker

unit Unit1;

interface

uses
  SysUtils, Classes, Forms, Buttons, SHDocVw_EWB, EmbeddedWB, StdCtrls,
  Controls, OleCtrls, EwbCore, ExtCtrls, IEAddress;

type
  TForm1 = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    edURL: TIEAddress;
    procedure FormShow(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure EmbeddedWB1ShowDialog(Sender: TObject; h: Cardinal;
      StyleEx: Integer; OldCaption: string; var NewCaption: WideString;
      var Cancel: Boolean);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  EmbeddedWB1.Navigate(edURL.Text);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  EmbeddedWB1.GoHome;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  sPreviewFromTemplatePath: string;
begin
  if EmbeddedWB1.DocumentLoaded then
  begin
    sPreviewFromTemplatePath := ExtractFilePath(Application.ExeName) + 'template.htm';
    EmbeddedWB1.PrintPreviewFromTemplate(sPreviewFromTemplatePath);
  end;
end;

procedure TForm1.EmbeddedWB1ShowDialog(Sender: TObject; h: Cardinal;
  StyleEx: Integer; OldCaption: string; var NewCaption: WideString;
  var Cancel: Boolean);
begin
  NewCaption := 'Print Preview';
end;

end.

