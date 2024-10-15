{
  Keyman is copyright (C) SIL Global. MIT License.
  
  // TODO-WINDOWS-UPDATES: Localise all the labels and captions.
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
  UserMessages;

type
  TfrmStartInstall = class(TfrmKeymanBase)
    cmdInstall: TButton;
    cmdLater: TButton;
    lblInstallUpdate: TLabel;
    procedure cmdInstallClick(Sender: TObject);
    procedure cmdLaterClick(Sender: TObject);
  private
  public
  end;


implementation

{$R *.dfm}

procedure TfrmStartInstall.cmdInstallClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmStartInstall.cmdLaterClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
