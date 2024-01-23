unit UfrmStartInstall;

interface

uses

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UserMessages, StdCtrls, ExtCtrls, UfrmKeymanBase;

type
  TfrmStartInstall = class(TfrmKeymanBase)
    LabelMessage: TLabel;
    InstallButton: TButton;
    CancelButton: TButton;
    procedure InstallButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
  public
  end;

var
  frmStartInstall: TfrmStartInstall;

implementation

{$R *.dfm}

procedure TfrmStartInstall.InstallButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmStartInstall.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
