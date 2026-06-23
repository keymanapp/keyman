{
  Keyman is copyright (C) SIL Global. MIT License.

  // TODO: #12887 Localise all the labels and captions.
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
  UtilNetworkConnection,
  Vcl.Imaging.pngimage;

type
  TfrmStartInstall = class(TfrmKeymanBase)
    cmdInstall: TButton;
    cmdLater: TButton;
    lblUpdateMessage: TLabel;
    imgKeymanLogo: TImage;
    shpMeteredWarning: TShape;
    lblMeteredWarning: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    FRestartRequired: Boolean;
    FReadyToInstall: Boolean;
  public
  constructor Create(
    AOwner: TComponent;
    const RestartRequired: Boolean;
    const ReadyToInstall: Boolean = False); reintroduce;
  end;

implementation
uses
  MessageIdentifiers,
  MessageIdentifierConsts;

{$R *.dfm}

constructor TfrmStartInstall.Create(
  AOwner: TComponent;
  const RestartRequired: Boolean;
  const ReadyToInstall: Boolean = False);
begin
  inherited Create(AOwner);
  FRestartRequired := RestartRequired;
  FReadyToInstall := ReadyToInstall;
end;

procedure TfrmStartInstall.FormCreate(Sender: TObject);
var
  IsMetered: Boolean;
begin
  inherited;
  cmdInstall.Caption := MsgFromId(S_Update_Now);
  cmdLater.Caption := MsgFromId(S_Later);
  if FRestartRequired then
    lblUpdateMessage.Caption := MsgFromId(S_Update_Restart_Req)
  else
    lblUpdateMessage.Caption := MsgFromId(S_Ready_To_Install);

  IsMetered := UtilNetworkConnection.IsMetered;
  // Show warning if on a metered connection. If FReadyToInstall the update is 
  // already downloaded, so no use in displaying the warning.
  lblMeteredWarning.Visible := IsMetered and not FReadyToInstall;
  shpMeteredWarning.Visible := IsMetered and not FReadyToInstall;
  if IsMetered then
    lblMeteredWarning.Caption := MsgFromId(S_Metered_Warning);
end;

end.
