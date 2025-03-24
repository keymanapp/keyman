{
  Keyman is copyright (C) SIL Global. MIT License.

  // TODO: #12887 Localise all the labels and captions.
}
unit Keyman.Configuration.UI.UfrmStartInstallNow;
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
  UserMessages;

type
  TfrmStartInstallNow = class(TfrmKeymanBase)
    cmdInstall: TButton;
    cmdLater: TButton;
    lblUpdateMessage: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation
uses
  MessageIdentifiers,
  MessageIdentifierConsts;

{$R *.dfm}


procedure TfrmStartInstallNow.FormCreate(Sender: TObject);
begin
  inherited;
  cmdInstall.Caption := MsgFromId(S_Update);
  cmdLater.Caption := MsgFromId(S_Button_Close);
  lblUpdateMessage.Caption := MsgFromId(S_Update_Restart_Req);
end;

end.
