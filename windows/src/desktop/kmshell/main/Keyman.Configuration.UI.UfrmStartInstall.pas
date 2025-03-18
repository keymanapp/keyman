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
  UserMessages, Vcl.Imaging.pngimage;

type
  TfrmStartInstall = class(TfrmKeymanBase)
    cmdInstall: TButton;
    cmdLater: TButton;
    lblInstallUpdate: TLabel;
    imgKeymanLogo: TImage;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;


implementation
uses
  MessageIdentifiers,
  MessageIdentifierConsts;

{$R *.dfm}

procedure TfrmStartInstall.FormCreate(Sender: TObject);
begin
  inherited;
  cmdInstall.Caption := MsgFromId(S_Update_Now);
  cmdLater.Caption := MsgFromId(S_Later);
  lblInstallUpdate.Caption := MsgFromId(S_Ready_To_Install);
end;

end.
