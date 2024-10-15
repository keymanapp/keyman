{
  Keyman is copyright (C) SIL Global. MIT License.
  
  // TODO-WINDOWS-UPDATES: Localise all the labels and captions.
}
unit Keyman.Configuration.UI.UfrmStartInstallNow;
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


// TODO-WINDOWS-UPDATES: remove events as they are properties on the buttons

procedure TfrmStartInstallNow.cmdInstallClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmStartInstallNow.cmdLaterClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
