unit UfrmStartInstallNow;
{
  Copyright:    © SIL Global.
  // TODO: Localise all the labels and captions.
}
interface

uses

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UserMessages, StdCtrls, ExtCtrls, UfrmKeymanBase;

type
  TfrmStartInstallNow = class(TfrmKeymanBase)
    cmdInstall: TButton;
    cmdLater: TButton;
    lblUpdateMessage: TLabel;
    lblUpdateNow: TLabel;
    procedure cmdInstallClick(Sender: TObject);
    procedure cmdLaterClick(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}


// TODO remove events as they are properties on the buttons

procedure TfrmStartInstallNow.cmdInstallClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmStartInstallNow.cmdLaterClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
