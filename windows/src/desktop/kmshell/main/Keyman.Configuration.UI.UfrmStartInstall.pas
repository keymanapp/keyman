{
  Keyman is copyright (C) SIL Global. MIT License.
}
unit Keyman.Configuration.UI.UfrmStartInstall;
interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,
  UfrmKeymanBase,
  UserMessages,
  Vcl.Imaging.pngimage;

type
  // The 4 valid installation form scenarios plus a None case for validation
  TInstallCase = (
    icNone, // Not a valid case, can be used as check before calling creating form
    icDownloadRestartMetered,
    icDownloadRestart,
    icDownload,
    icReadyToInstall, // Metered warning never needed if ReadyToInstall
    icDownloadMetered
  );

  TfrmStartInstall = class(TfrmKeymanBase)
    cmdInstall: TButton;
    cmdLater: TButton;
    lblUpdateMessage: TLabel;
    lblDownloadRequired: TLabel;
    imgKeymanLogo: TImage;
    lblMeteredWarning: TLabel;
    imgCaution: TImage;
    procedure FormCreate(Sender: TObject);
  private
    FScenario: TInstallCase;
  public
    constructor Create(
      AOwner: TComponent;
      const AScenario: TInstallCase); reintroduce;
  end;

implementation

uses
  MessageIdentifiers,
  MessageIdentifierConsts;

{$R *.dfm}

constructor TfrmStartInstall.Create(
  AOwner: TComponent;
  const AScenario: TInstallCase);
begin
  Assert(AScenario <> icNone, 'Invalid install case');
  FScenario := AScenario;
  inherited Create(AOwner);
end;

procedure TfrmStartInstall.FormCreate(Sender: TObject);
begin
  inherited;
  cmdInstall.Caption := MsgFromId(S_Update_Now);
  cmdLater.Caption := MsgFromId(S_Later);

  // Default UI configuration state - metered warnings hidden initially
  lblUpdateMessage.Visible := False;
  lblDownloadRequired.Visible := False;
  lblMeteredWarning.Visible := False;
  imgCaution.Visible := False;

  case FScenario of
    icDownloadRestartMetered:
    begin
      lblUpdateMessage.Caption := MsgFromId(S_Update_Restart_Req);
      lblUpdateMessage.Visible := True;
      lblDownloadRequired.Caption := MsgFromId(S_Update_Download_Required);
      lblDownloadRequired.Visible := True;
      lblMeteredWarning.Caption := MsgFromId(S_Metered_Warning);
      lblMeteredWarning.Visible := True;
      imgCaution.Visible := True;
    end;

    icDownloadRestart:
    begin
      lblUpdateMessage.Caption := MsgFromId(S_Update_Restart_Req);
      lblUpdateMessage.Visible := True;
      lblDownloadRequired.Caption := MsgFromId(S_Update_Download_Required);
      lblDownloadRequired.Visible := True;
    end;

    icDownload:
    begin
      lblDownloadRequired.Caption := MsgFromId(S_Update_Download_Required);
      lblDownloadRequired.Visible := True;
    end;

    icReadyToInstall:
    begin
      lblUpdateMessage.Caption := MsgFromId(S_Ready_To_Install);
      lblUpdateMessage.Visible := True;
    end;

    icDownloadMetered:
    begin
      lblDownloadRequired.Caption := MsgFromId(S_Update_Download_Required);
      lblDownloadRequired.Visible := True;
      lblMeteredWarning.Caption := MsgFromId(S_Metered_Warning);
      lblMeteredWarning.Visible := True;
      imgCaution.Visible := True;
    end;
  end;
end;

end.
