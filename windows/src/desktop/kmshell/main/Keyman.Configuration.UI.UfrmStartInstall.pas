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
    icRestartRequiredMetered,
    icRestartRequiredNotMetered,
    icReadyToInstallNotMetered, // Metered warning never needed if ReadyToInstall
    icNoInstallMessageMetered
  );

  TfrmStartInstall = class(TfrmKeymanBase)
    cmdInstall: TButton;
    cmdLater: TButton;
    lblUpdateMessage: TLabel;
    imgKeymanLogo: TImage;
    shpMeteredWarning: TShape;
    lblMeteredWarning: TLabel;
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
  lblUpdateMessage.Visible := True;
  lblMeteredWarning.Visible := False;
  shpMeteredWarning.Visible := False;

  case FScenario of
    icRestartRequiredMetered:
    begin
      lblUpdateMessage.Caption := MsgFromId(S_Update_Restart_Req);
      lblMeteredWarning.Caption := MsgFromId(S_Metered_Warning);
      lblMeteredWarning.Visible := True;
      shpMeteredWarning.Visible := True;
    end;

    icRestartRequiredNotMetered:
    begin
      lblUpdateMessage.Caption := MsgFromId(S_Update_Restart_Req);
    end;

    icReadyToInstallNotMetered:
    begin
      lblUpdateMessage.Caption := MsgFromId(S_Ready_To_Install);
    end;

    icNoInstallMessageMetered:
    begin
      lblUpdateMessage.Visible := False;
      lblMeteredWarning.Caption := MsgFromId(S_Metered_Warning);
      lblMeteredWarning.Visible := True;
      shpMeteredWarning.Visible := True;
    end;
  end;
end;

end.
