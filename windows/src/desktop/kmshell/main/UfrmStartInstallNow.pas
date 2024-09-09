unit UfrmStartInstallNow;

interface

uses

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UserMessages, StdCtrls, ExtCtrls, UfrmKeymanBase;

type
  TfrmStartInstallNow = class(TfrmKeymanBase)
    Install: TButton;
    Later: TButton;
    InstallUpdate: TLabel;
    procedure InstallClick(Sender: TObject);
    procedure LaterClick(Sender: TObject);
  private
  public
  end;

var
  frmStartInstall: TfrmStartInstallNow;

implementation

{$R *.dfm}

procedure TfrmStartInstallNow.InstallClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmStartInstallNow.LaterClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
