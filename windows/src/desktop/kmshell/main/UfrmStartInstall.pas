unit UfrmStartInstall;

interface

uses

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UserMessages, StdCtrls, ExtCtrls, UfrmKeymanBase;

type
  TfrmStartInstall = class(TfrmKeymanBase)
    Install: TButton;
    Later: TButton;
    InstallUpdate: TLabel;
    procedure InstallClick(Sender: TObject);
    procedure LaterClick(Sender: TObject);
  private
  public
  end;

var
  frmStartInstall: TfrmStartInstall;

implementation

{$R *.dfm}

procedure TfrmStartInstall.InstallClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmStartInstall.LaterClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
