unit Keyman.Configuration.UI.UfrmSettingsManager;

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
  TfrmSettingsManager = class(TForm)
    gridDebugOption: TStringGrid;
    memoDescription: TMemo;
    cmdApply: TButton;
    cmdClearAll: TButton;
    cmdClose: TButton;
    editRegKey: TEdit;
    editDefault: TEdit;
    cmdReset: TButton;
    imgElevate: TImage;
    cmdExport: TButton;
    chkShowDevelopmentSettings: TCheckBox;
    cmdImport: TButton;
    dlgImport: TOpenDialog;
    dlgExport: TSaveDialog;
    lblDefault: TLabel;
    cmdTSFApplicationSettings: TButton;
    imgElevateTSFApplications: TImage;
    procedure FormCreate(Sender: TObject);
    procedure gridDebugOptionClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure gridDebugOptionSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure cmdApplyClick(Sender: TObject);
    procedure cmdCloseClick(Sender: TObject);
    procedure cmdClearAllClick(Sender: TObject);
    procedure cmdResetClick(Sender: TObject);
    procedure gridDebugOptionDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure chkShowDevelopmentSettingsClick(Sender: TObject);
    procedure cmdExportClick(Sender: TObject);
    procedure cmdImportClick(Sender: TObject);
    procedure cmdTSFApplicationSettingsClick(Sender: TObject);
  private
    IsAdmin: Boolean;
    FWasSaved: Boolean;
    procedure FillGrid;
    function SelectedSetting: TKeymanSetting;
    procedure EnableControls;
    function FindSettingRow(const ID: string): Integer;
  public
    Settings: TKeymanSettings;
    class function Execute(AOwner: TComponent = nil): Boolean;
  end;

implementation

{$R *.dfm}

uses
  System.Math,
  System.UITypes,
  System.Win.Registry,

  Keyman.System.SettingsManager,
  Keyman.System.SettingsManagerFile,
  Keyman.Configuration.UI.UfrmSettingsAddTSFApp,
  RegistryKeys;

procedure TfrmSettingsManager.FormCreate(Sender: TObject);
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
  EnableControls;
end;

procedure TfrmSettingsManager.FormResize(Sender: TObject);
begin
  gridDebugOption.ColWidths[1] := gridDebugOption.ClientWidth - gridDebugOption.ColWidths[0] - 1;
end;

procedure TfrmSettingsManager.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
  if settings.Modified then
  begin
    case MessageDlg('Do you want to save changes before closing?', mtConfirmation, mbYesNoCancel, 0) of
      mrYes: cmdApplyClick(cmdApply);
      mrNo: ;
      mrCancel: CanClose := False;
    end;
  end;
end;


procedure TfrmSettingsManager.cmdResetClick(Sender: TObject);
begin
  SelectedSetting.Reset;
  FillGrid;
  EnableControls;
end;

procedure TfrmSettingsManager.chkShowDevelopmentSettingsClick(
  Sender: TObject);
begin
  FillGrid;
end;

procedure TfrmSettingsManager.cmdClearAllClick(Sender: TObject);
begin
  Settings.Reset;
  FillGrid;
  EnableControls;
end;

procedure TfrmSettingsManager.cmdCloseClick(Sender: TObject);
begin
  if fsModal in FormState then
  begin
    if FWasSaved
      then ModalResult := mrOk
      else ModalResult := mrCancel;
  end
  else
    Close;
end;

procedure TfrmSettingsManager.cmdExportClick(Sender: TObject);
begin
  if dlgExport.Execute then
  begin
    TKeymanSettingsManagerFile.Export(Settings, dlgExport.FileName);
  end;
end;

procedure TfrmSettingsManager.cmdImportClick(Sender: TObject);
begin
  if dlgImport.Execute then
  begin
    TKeymanSettingsManagerFile.Import(Settings, dlgImport.FileName);
    FillGrid;
    EnableControls;
  end;
end;

procedure TfrmSettingsManager.cmdApplyClick(Sender: TObject);
var
  AdminChanges: Boolean;
  setting: TKeymanSetting;
begin
  if not IsAdmin then
  begin
    AdminChanges := False;
    for setting in settings do
      if setting.Base.RequiresAdministrator and setting.Modified then
        AdminChanges := True;
    if AdminChanges then
      ShowMessage('Cannot save changes to HKLM values; re-run as Administrator to make these changes.');
  end;

  TKeymanSettingsManager.Save(Settings, IsAdmin);
  // We'll recreate the Settings list so that settings that have been removed
  // will be refreshed
  FreeAndNil(Settings);
  Settings := TKeymanSettings.Create;
  TKeymanSettingsManager.Load(Settings);
  FillGrid;
  gridDebugOption.Invalidate;
  FWasSaved := True;
  EnableControls;
end;

procedure TfrmSettingsManager.cmdTSFApplicationSettingsClick(
  Sender: TObject);
var
  frm: TfrmSettingsAddTSFApp;
  s: TKeymanSetting;
begin
  frm := TfrmSettingsAddTSFApp.Create(Self);
  try
    if frm.ShowModal = mrOk then
    begin
      s := TKeymanSetting.CreateCustom_TSFApp(CustomKeymanSetting_TSFApp.ID + frm.Filename);
      s.ValueInt := frm.ValueInt;
      Settings.Add(s);
      FillGrid;
      gridDebugOption.Row := FindSettingRow(s.Base.ID);
      gridDebugOptionClick(gridDebugOption);
    end;
  finally
    frm.Free;
  end;
  EnableControls;
end;

function TfrmSettingsManager.FindSettingRow(const ID: string): Integer;
var
  i: Integer;
begin
  for i := 1 to gridDebugOption.RowCount - 1 do
    if SameText((gridDebugOption.Objects[0, i] as TKeymanSetting).Base.ID, ID) then
      Exit(i);
  Result := -1;
end;

procedure TfrmSettingsManager.EnableControls;
begin
  cmdApply.Enabled := Settings.Modified;
end;

class function TfrmSettingsManager.Execute(AOwner: TComponent): Boolean;
var
  frm: TfrmSettingsManager;
begin
  frm := TfrmSettingsManager.Create(AOwner);
  try
    Result := frm.ShowModal = mrOk;
  finally
    frm.Free;
  end;
end;

procedure TfrmSettingsManager.FillGrid;
var
  s: TKeymanSetting;
  MaxWidth, i: Integer;
begin
  gridDebugOption.RowCount := Settings.Count + 1;
  gridDebugOption.Cells[0, 0] := 'Name';
  gridDebugOption.Cells[1, 0] := 'Value';

  gridDebugOption.Canvas.Font.Style := [fsBold];

  i := 1; MaxWidth := 0;
  for s in Settings do
  begin
    if chkShowDevelopmentSettings.Checked or not s.Base.ID.StartsWith('development.') then
    begin
      gridDebugOption.Objects[0, i] := s;
      gridDebugOption.Cells[0, i] := s.Base.ID;
      if s.Base.ValueType = kstString
        then gridDebugOption.Cells[1, i] := s.ValueStr
        else gridDebugOption.Cells[1, i] := IntToStr(s.ValueInt);
      MaxWidth := System.Math.Max(MaxWidth, gridDebugOption.Canvas.TextWidth(s.Base.ID+' *'));
      Inc(i);
    end;
  end;

  gridDebugOption.RowCount := i;

  gridDebugOption.ColWidths[0] := MaxWidth + 8;
  gridDebugOption.ColWidths[1] := gridDebugOption.ClientWidth - MaxWidth - 8 - 1;
  gridDebugOptionClick(gridDebugOption);
end;


function TfrmSettingsManager.SelectedSetting: TKeymanSetting;
begin
  Result := gridDebugOption.Objects[0, gridDebugOption.Row] as TKeymanSetting;
end;

procedure TfrmSettingsManager.gridDebugOptionClick(Sender: TObject);
begin
  memoDescription.Text := SelectedSetting.Base.Description;
  if SelectedSetting.Base.RootKey = HKCU
    then editRegKey.Text := 'HKCU\'+SelectedSetting.Base.Key
    else editRegKey.Text := 'HKLM\'+SelectedSetting.Base.Key;
  editDefault.Text := SelectedSetting.Base.DefaultAsString;
  imgElevate.Visible := not IsAdmin and SelectedSetting.Base.RequiresAdministrator;

  imgElevateTSFApplications.Visible := not IsAdmin and (SelectedSetting.Base.ID = 'engine.compatibility.text_services_framework');
  cmdTSFApplicationSettings.Visible := SelectedSetting.Base.ID = 'engine.compatibility.text_services_framework';
end;

procedure TfrmSettingsManager.gridDebugOptionDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  s: TKeymanSetting;
  t: string;
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

  t := gridDebugOption.Cells[ACol, ARow];

  if (ARow = 0) then
  begin
    gridDebugOption.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 1, t);
    Exit;
  end;
  s := gridDebugOption.Objects[0, ARow] as TKeymanSetting;
  if not s.IsEmpty
    then gridDebugOption.Canvas.Font.Style := [fsBold]
    else gridDebugOption.Canvas.Font.Style := [];

  if (ACol = 0) and (s.Modified) then
    t := t + ' *';
  gridDebugOption.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 1, t);

  if (ACol = 0) and s.Base.RequiresAdministrator and not IsAdmin then
  begin
    gridDebugOption.Canvas.Draw(Rect.Left + 2 + gridDebugOption.Canvas.TextWidth(t) + 2, Rect.Top, imgElevate.Picture.Graphic);
  end;
end;

procedure TfrmSettingsManager.gridDebugOptionSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  v: Integer;
  s: TKeymanSetting;
begin
  s := gridDebugOption.Objects[0, ARow] as TKeymanSetting;
  if s.Base.ValueType = kstInteger then
  begin
    if not TryStrToInt(Value, v) then
      Exit;
    s.ValueInt := v;
  end
  else
  begin
    s.ValueStr := Value.Trim;
  end;

  // Repaint caption
  InvalidateRect(gridDebugOption.Handle, gridDebugOption.CellRect(0, ARow), False);
  EnableControls;
end;

end.
