unit Keyman.Developer.UI.Import.UfrmImportWindowsKeyboard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmTike, Vcl.StdCtrls;

type
  TForm2 = class(TTikeForm)
    lbKeyboards: TListBox;
    lblKeyboards: TLabel;
    cmdOK: TButton;
    cmdCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lbKeyboardsDblClick(Sender: TObject);
    procedure lbKeyboardsClick(Sender: TObject);
  private
    procedure FillKeyboardList;
    procedure EnableControls;
    function GetKLID: DWORD;
  public
    property KLID: DWORD read GetKLID;
  end;

implementation

{$R *.dfm}

procedure TForm2.EnableControls;
begin
  cmdOK.Enabled := lbKeyboards.ItemIndex >= 0;
end;

procedure TForm2.FillKeyboardList;
begin

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  inherited;
  FillKeyboardList;
  EnableControls;
end;

function TForm2.GetKLID: DWORD;
begin
  Result := DWORD(lbKeyboards.Items.Objects[lbKeyboards.ItemIndex]);
end;

procedure TForm2.lbKeyboardsClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TForm2.lbKeyboardsDblClick(Sender: TObject);
begin
  if cmdOK.Enabled then
    cmdOK.Click;
end;

end.
