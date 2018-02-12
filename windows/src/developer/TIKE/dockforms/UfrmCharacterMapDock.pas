unit UfrmCharacterMapDock;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmCharacterMapNew, UfrmTIKEDock,
  JvComponentBase, JvDockControlForm;

type
  TfrmCharacterMapDock = class(TTikeDockForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    procedure RefreshOptions;
  end;

var
  frmCharacterMapDock: TfrmCharacterMapDock;

implementation

{$R *.dfm}

uses
  RegistryKeys,
  UfrmMain;

procedure TfrmCharacterMapDock.FormCreate(Sender: TObject);
begin
  frmCharacterMapNew := TfrmCharacterMapNew.Create(Self, SRegKey_IDECharacterMap_CU);
  frmCharacterMapNew.Align := alClient;
  frmCharacterMapNew.BorderStyle := bsNone;
  frmCharacterMapNew.Parent := Self;
  frmCharacterMapNew.Visible := True;

  frmCharacterMapNew.OnCancelFocus := frmKeymanDeveloper.CharMapCancelFocus;
  frmCharacterMapNew.OnInsertCode := frmKeymanDeveloper.CharMapInsertCode;
  frmCharacterMapNew.OnCanInsertCode := frmKeymanDeveloper.CharMapCanInsertCode;

end;

procedure TfrmCharacterMapDock.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(frmCharacterMapNew);
end;

procedure TfrmCharacterMapDock.RefreshOptions;
begin
  frmCharacterMapNew.Reload;
end;

end.
