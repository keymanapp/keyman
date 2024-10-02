unit Keyman.Configuration.UI.UfrmStartInstall;
{
  Copyright:    © SIL Global.
  // TODO: Localise all the labels and captions.
}
interface

uses

  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  UserMessages,
  StdCtrls,
  ExtCtrls,
  UfrmKeymanBase;

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
