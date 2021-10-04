unit UfrmDebugStatus_Options;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Winapi.Messages,
  Winapi.Windows,

  debugkeyboard,
  UfrmDebugStatus_Child, Vcl.StdCtrls;

type
  TDebugKeyboardOption = class
    Name, InitialValue, CurrentValue: string;
    Values: TStringList;
    lbl: TLabel;
    cb: TComboBox;
    constructor Create(AOwner: TComponent; AParent: TScrollBox);
    destructor Destroy; override;
  end;

  TfrmDebugStatus_Options = class(TfrmDebugStatus_Child)
    sb: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FOptions: TObjectDictionary<string, TDebugKeyboardOption>;
    FChanging: Boolean;
    procedure AnalyseDebugKeyboardOptions;
    procedure LayoutGrid;
    procedure UpdateOptions;
    procedure cbChange(Sender: TObject);
    procedure EnableControls;
  protected
    procedure DebugKeyboardChanged; override;
    procedure DebugEventChanged; override;
  public
    procedure StartBatch; override;
    procedure FinishBatch; override;
    procedure SetOptionValue(const name, value: string);
  end;

implementation

uses
  kmxfile,
  kmxfileconsts,
  kmxfileutils;

{$R *.dfm}

procedure TfrmDebugStatus_Options.AnalyseDebugKeyboardOptions;
var
  group: TDebugGroup;
  key: TDebugKey;

  function BaseString(dp: DWord): string;
  var
    pch: PByte;
  begin
    if dp = 0 then
      Exit('');
    pch := PByte(debugkeyboard.Memory.Memory);
    Inc(pch, dp);
    Result := PChar(pch);
  end;

  procedure AddOption(Store1, Store2: PKeyboardFileStore);
  var
    name: string;
    opt: TDebugKeyboardOption;
  begin
    name := BaseString(Store1.dpName);
    if not FOptions.ContainsKey(name) then
    begin
      opt := TDebugKeyboardOption.Create(Self, sb);
      opt.Name := name;
      opt.InitialValue := BaseString(Store1.dpString);
      opt.Values.Add(opt.InitialValue);
      FOptions.Add(name, opt);
    end;
    FOptions[name].Values.Add(BaseString(Store2.dpString));
  end;

  procedure FindOptionsInXString(s: string);
  var
    p: PChar;
    rec: TSentinelRecord;
  begin
    p := PChar(s);
    while (p <> nil) and (p^ <> #0) do
    begin
      rec := ExpandSentinel(p, PKeyboardFileHeader(debugkeyboard.Memory.Memory));
      if rec.IsSentinel and (rec.Code = CODE_IFOPT) then
      begin
        AddOption(rec.IfOpt.Store1, rec.IfOpt.Store2);
      end else if rec.IsSentinel and (rec.Code = CODE_SETOPT) then
      begin
        AddOption(rec.SetOpt.Store1, rec.SetOpt.Store2);
      end;
      p := incxstr(p);
    end;
  end;

begin
  FOptions.Clear;
  if not Assigned(debugkeyboard) then Exit;
  for group in debugkeyboard.Groups do
  begin
    for key in group.Keys do
    begin
      FindOptionsInXString(key.dpContext);
      FindOptionsInXString(key.dpOutput);
    end;
    FindOptionsInXString(group.Match);
    FindOptionsInXString(group.NoMatch);
  end;
end;

procedure TfrmDebugStatus_Options.DebugEventChanged;
begin
  inherited;
  EnableControls;
end;

procedure TfrmDebugStatus_Options.DebugKeyboardChanged;
begin
  AnalyseDebugKeyboardOptions;
  LayoutGrid;
  UpdateOptions;
end;

procedure TfrmDebugStatus_Options.EnableControls;
var
  opt: TDebugKeyboardOption;
  e: Boolean;
begin
  e := CurrentEvent = nil;
  for opt in FOptions.Values do
  begin
    opt.cb.Enabled := e;
    opt.lbl.Enabled := e;
  end;
end;

procedure TfrmDebugStatus_Options.FinishBatch;
begin
  inherited;
  UpdateOptions;
  EnableControls;
end;

procedure TfrmDebugStatus_Options.FormCreate(Sender: TObject);
begin
  inherited;
  FOptions := TObjectDictionary<string, TDebugKeyboardOption>.Create;
end;

procedure TfrmDebugStatus_Options.FormDestroy(Sender: TObject);
begin
  inherited;
  FOptions.Free;
end;

procedure TfrmDebugStatus_Options.FormResize(Sender: TObject);
begin
  inherited;
  LayoutGrid;
end;

procedure TfrmDebugStatus_Options.UpdateOptions;
var
  opt: TDebugKeyboardOption;
begin
  FChanging := True;
  try
    for opt in FOptions.Values do
    begin
      opt.CurrentValue := DebugCore.GetOption(opt.Name);
      opt.cb.Text := opt.CurrentValue;
    end;
  finally
    FChanging := False;
  end;
end;

procedure TfrmDebugStatus_Options.LayoutGrid;
var
  opt: TDebugKeyboardOption;
  y: Integer;
begin
  y := 4;
  for opt in FOptions.Values do
  begin
    opt.lbl.Caption := opt.Name;
    opt.lbl.Left := 4;
    opt.lbl.Top := y;
    opt.lbl.SetBounds(4, y, sb.Width div 2 - 8, opt.lbl.Height);
    opt.lbl.Visible := True;
    opt.cb.Items.Assign(opt.Values);
    opt.cb.SetBounds(sb.Width div 2, y, sb.Width div 2 - 4 - GetSystemMetrics(SM_CXVSCROLL), opt.cb.Height);
    opt.cb.Visible := True;
    opt.cb.Tag := Integer(Pointer(opt));
    opt.cb.OnChange := cbChange;
    Inc(y, opt.cb.Height + 2);
  end;
end;

procedure TfrmDebugStatus_Options.SetOptionValue(const name, value: string);
var
  opt: TDebugKeyboardOption;
begin
  if FOptions.TryGetValue(name, opt) then
  begin
    opt.CurrentValue := value;
    opt.cb.Text := value;
  end;
end;

procedure TfrmDebugStatus_Options.StartBatch;
begin
  inherited;
  EnableControls;
end;

procedure TfrmDebugStatus_Options.cbChange(Sender: TObject);
var
  value: string;
  opt: TDebugKeyboardOption;
begin
  if FChanging or (DebugCore = nil) then Exit;
  value := (Sender as TComboBox).Text;
  opt := TDebugKeyboardOption((Sender as TComboBox).Tag);
  opt.CurrentValue := value;
  DebugCore.SetOption(opt.Name, opt.CurrentValue);
end;

{ TDebugKeyboardOption }

constructor TDebugKeyboardOption.Create(AOwner: TComponent; AParent: TScrollBox);
begin
  inherited Create;
  Values := TStringList.Create;
  Values.Duplicates := dupIgnore;
  Values.Sorted := True;

  lbl := TLabel.Create(AOwner);
  lbl.Parent := AParent;
  lbl.Visible := False;

  cb := TComboBox.Create(AOwner);
  cb.Parent := AParent;
  cb.Visible := False;
end;

destructor TDebugKeyboardOption.Destroy;
begin
  FreeAndNil(Values);
  inherited Destroy;
end;

end.
