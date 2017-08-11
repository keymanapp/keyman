unit Unit1;

interface

{$I EWB.inc}

uses
  ExportFavorites, StdCtrls, Dialogs, Controls, OleCtrls, SHDocVw_EWB, EwbCore,
  EmbeddedWB, ComCtrls, Forms, Classes, ExtCtrls{$IFDEF DELPHI7_UP}, XPMan{$ENDIF};

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    StatusBar1: TStatusBar;
    ExportFavorite1: TExportFavorite;
    EmbeddedWB1: TEmbeddedWB;
    Panel2: TPanel;
    ChkExploreFavFileOnComplete: TCheckBox;
    ChkNavigateOnComplete: TCheckBox;
    edTargetFileName: TEdit;
    Label1: TLabel;
    edTargetPath: TEdit;
    Label2: TLabel;
    Log: TListBox;
    edHTMLTitle: TEdit;
    Label3: TLabel;
    Panel3: TPanel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure ExportFavorite1Success(Sender: TObject; const Msg: string);
    procedure ExportFavorite1Error(Sender: TObject; const ErrorMsg: String;
      const ErrorCode: Byte);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Log.Clear;
  with ExportFavorite1 do
  begin
    ExploreFavFileOnComplete := ChkExploreFavFileOnComplete.Checked;
    NavigateOnComplete := ChkNavigateOnComplete.Checked;
    TargetFileName := edTargetFileName.Text;
    TargetPath := edTargetPath.Text;
    ExportFavorite1.Localization.HTMLTitle := edHTMLTitle.Text;
    ExportFavorites;
  end;
end;

procedure TForm2.ExportFavorite1Success(Sender: TObject;
  const Msg: string);
begin
  MessageDlg(Msg, mtInformation, [MbOk], 0);
end;

procedure TForm2.ExportFavorite1Error(Sender: TObject;
  const ErrorMsg: String; const ErrorCode: Byte);
begin
  Log.Items.add(ErrorMsg + ' - ' + ExportFavorite1.Localization.ChangeItMessage);
  case ErrorCode of
    ERROR_FAV_TARGET_PATH_INVALID: edTargetPath.SetFocus;
    ERROR_FAV_TARGET_FILENAME_INVALID: edTargetFileName.SetFocus;
  end;
end;

end.

