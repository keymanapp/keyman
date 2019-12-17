unit Keyman.UI.SettingsManager.UfrmSettingsManagerMain;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  Keyman.System.Settings, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TfrmSettingsManagerMain = class(TForm)
    gridDebugOption: TStringGrid;
    memoDescription: TMemo;
    cmdSave: TButton;
    cmdClearAll: TButton;
    cmdExit: TButton;
    editRegKey: TEdit;
    editDefault: TEdit;
    Button1: TButton;
    imgElevate: TImage;
    procedure FormCreate(Sender: TObject);
    procedure gridDebugOptionClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure gridDebugOptionSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure cmdSaveClick(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure cmdClearAllClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure gridDebugOptionDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    IsAdmin: Boolean;
    procedure FillGrid;
    function SelectedSetting: TKeymanSetting;
  public
    Settings: TKeymanSettings;
  end;

var
  frmSettingsManagerMain: TfrmSettingsManagerMain;

implementation

{$R *.dfm}

uses
  System.Win.Registry,

  Keyman.System.SettingsManager,
  RegistryKeys;

procedure TfrmSettingsManagerMain.Button1Click(Sender: TObject);
begin
  SelectedSetting.Reset;
  FillGrid;
end;

procedure TfrmSettingsManagerMain.cmdClearAllClick(Sender: TObject);
begin
  Settings.Reset;
  FillGrid;
end;

procedure TfrmSettingsManagerMain.cmdExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSettingsManagerMain.cmdSaveClick(Sender: TObject);
var
  s: TKeymanSettings;
  i: Integer;
  AdminChanges: Boolean;
begin
  if not IsAdmin then
  begin
    AdminChanges := False;
    s := TKeymanSettings.Create;
    try
      TKeymanSettingsManager.Load(s);
      for i := 0 to s.Count - 1 do
        if s[i].Base.RequiresAdministrator and not s[i].Equals(Settings[i]) then
        begin
          AdminChanges := True;
          Break;
        end;
    finally
      s.Free;
    end;

    if AdminChanges then
      ShowMessage('Cannot save changes to HKLM values; re-run as Administrator to make these changes.');
  end;

  TKeymanSettingsManager.Save(Settings, IsAdmin);
end;

procedure TfrmSettingsManagerMain.FillGrid;
var
  i: Integer;
begin
  gridDebugOption.RowCount := Settings.Count + 1;
  gridDebugOption.Cells[0, 0] := 'Name';
  gridDebugOption.Cells[1, 0] := 'Value';

  for i := 0 to Settings.Count - 1 do
  begin
    gridDebugOption.Objects[0, i+1] := Settings[i];
    gridDebugOption.Cells[0, i+1] := Settings[i].Base.Name;
    if Settings[i].Base.ValueType = kstString
      then gridDebugOption.Cells[1, i+1] := Settings[i].ValueStr
      else gridDebugOption.Cells[1, i+1] := IntToStr(Settings[i].ValueInt);
  end;

  gridDebugOption.ColWidths[0] := 160;
  gridDebugOption.ColWidths[1] := gridDebugOption.ClientWidth - 160 - 1;
  gridDebugOptionClick(gridDebugOption);
end;

procedure TfrmSettingsManagerMain.FormCreate(Sender: TObject);
var
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    IsAdmin := r.OpenKey(SRegKey_KeymanEngine_LM, True);
  finally
    r.Free;
  end;

  Settings := TKeymanSettings.Create;

  TKeymanSettingsManager.Load(Settings);

  FillGrid;
end;

procedure TfrmSettingsManagerMain.FormResize(Sender: TObject);
begin
  gridDebugOption.ColWidths[1] := gridDebugOption.ClientWidth - gridDebugOption.ColWidths[0] - 1;
end;

function TfrmSettingsManagerMain.SelectedSetting: TKeymanSetting;
begin
  Result := gridDebugOption.Objects[0, gridDebugOption.Row] as TKeymanSetting;
end;

procedure TfrmSettingsManagerMain.gridDebugOptionClick(Sender: TObject);
begin
  memoDescription.Text := SelectedSetting.Base.Description;
  if SelectedSetting.Base.RootKey = HKCU
    then editRegKey.Text := 'HKCU\'+SelectedSetting.Base.Key
    else editRegKey.Text := 'HKLM\'+SelectedSetting.Base.Key;
  editDefault.Text := SelectedSetting.Base.DefaultAsString;
  imgElevate.Visible := not IsAdmin and SelectedSetting.Base.RequiresAdministrator;
end;

procedure TfrmSettingsManagerMain.gridDebugOptionDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  s: TKeymanSetting;
  tf: TTextFormat;
begin
  if gdFixed in State then
  begin
    gridDebugOption.Canvas.Brush.Color := clLtGray;
    gridDebugOption.Canvas.Font.Color := clBlack;
  end
  else if gdSelected in State then
  begin
    gridDebugOption.Canvas.Brush.Color := clHighlight;
    gridDebugOption.Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    gridDebugOption.Canvas.Brush.Color := clWindow;
    gridDebugOption.Canvas.Font.Color := clWindowText;
  end;

  if (ARow = 0) then
  begin
    gridDebugOption.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 1, gridDebugOption.Cells[ACol, ARow]);
    Exit;
  end;
  s := gridDebugOption.Objects[0, ARow] as TKeymanSetting;
  if not s.IsEmpty
    then gridDebugOption.Canvas.Font.Style := [fsBold]
    else gridDebugOption.Canvas.Font.Style := [];
  gridDebugOption.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 1, gridDebugOption.Cells[ACol, ARow]);

  if (ACol = 0) and s.Base.RequiresAdministrator and not IsAdmin then
  begin
    gridDebugOption.Canvas.Draw(Rect.Left + 2 + gridDebugOption.Canvas.TextWidth(gridDebugOption.Cells[ACol, ARow]) + 2, Rect.Top, imgElevate.Picture.Graphic);
  end;
end;

procedure TfrmSettingsManagerMain.gridDebugOptionSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  v: Integer;
begin
  if SelectedSetting.Base.ValueType = kstInteger then
  begin
    if not TryStrToInt(Value, v) then Exit;
    SelectedSetting.ValueInt := v;
  end
  else
    SelectedSetting.ValueStr := Value.Trim;

  // Repaint caption
  InvalidateRect(gridDebugOption.Handle, gridDebugOption.CellRect(0, ARow), False);
end;

end.
