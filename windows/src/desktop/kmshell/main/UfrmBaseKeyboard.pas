unit UfrmBaseKeyboard;

interface

uses
  System.Contnrs,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmWebContainer;

type
  TfrmBaseKeyboard = class(TfrmWebContainer)
    procedure TntFormCreate(Sender: TObject);
  private
    FBaseKeyboardID: Integer;
    procedure Footer_Cancel;
    procedure Footer_OK(params: TStringList);
  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  end;

function ConfigureBaseKeyboard(out BaseKeyboardID: Integer): Boolean;
function SetBaseKeyboard(WindowHandle: THandle; BaseKeyboardID: Integer): Boolean;
function MCompileBaseKeyboard(const BaseKeyboardIDText: string): Boolean;

implementation

{$R *.dfm}

uses
  BaseKeyboards,
  ErrorControlledRegistry,
  RegistryKeys,
  keymanapi_TLB,
  kmint,
  utilkmshell;

function ConfigureBaseKeyboard(out BaseKeyboardID: Integer): Boolean;
begin  with TfrmBaseKeyboard.Create(nil) do
  try
    Result := ShowModal = mrOk;
    if Result then
      BaseKeyboardID := FBaseKeyboardID;
  finally
    Free;
  end;
end;

procedure TfrmBaseKeyboard.TntFormCreate(Sender: TObject);
begin
  inherited;
  FRenderPage := 'basekeyboard';
  HelpTopic := 'context/base-keyboard';
  Content_Render;
end;

procedure TfrmBaseKeyboard.FireCommand(const command: WideString;
  params: TStringList);
begin
  if command = 'footer_ok' then Footer_OK(params)
  else if command = 'footer_cancel' then Footer_Cancel
  else inherited;
end;

procedure TfrmBaseKeyboard.Footer_Cancel;
begin
  ModalResult := mrCancel;
end;

procedure TfrmBaseKeyboard.Footer_OK(params: TStringList);
var
  v: Integer;
begin
  if not TryStrToInt('$'+params.Values['id'], v) then Exit;
  FBaseKeyboardID := v;
  ModalResult := mrOk;
end;

function MCompileBaseKeyboard(const BaseKeyboardIDText: string): Boolean;
var
  BaseKeyboardID: Integer;
  PreviousBaseKeyboardID: Integer;
  PreviousBaseKeyboardValue: string;
  PreviousBaseKeyboardValueExists: Boolean;

  procedure SavePreviousRegistryBaseKeyboardValue;
  var
      Reg: TRegistryErrorControlled;
  begin
    PreviousBaseKeyboardValueExists := False;
    PreviousBaseKeyboardValue := '';

    Reg := TRegistryErrorControlled.Create;
    try
      if Reg.OpenKeyReadOnly(SRegKey_KeymanEngine_CU) and Reg.ValueExists(SRegValue_UnderlyingLayout) then
      begin
        PreviousBaseKeyboardValueExists := True;
        PreviousBaseKeyboardValue := Reg.ReadString(SRegValue_UnderlyingLayout);
      end;
    finally
      Reg.Free;
    end;
  end;

  procedure RestorePreviousBaseKeyboardValue;
  var
    Reg: TRegistryErrorControlled;
  begin
    Reg := TRegistryErrorControlled.Create;
    try
      if Reg.OpenKey(SRegKey_KeymanEngine_CU, True) then
        if PreviousBaseKeyboardValueExists then
          Reg.WriteString(SRegValue_UnderlyingLayout, PreviousBaseKeyboardValue)
        else if Reg.ValueExists(SRegValue_UnderlyingLayout) then
          Reg.DeleteValue(SRegValue_UnderlyingLayout);
    finally
      Reg.Free;
    end;
  end;

  procedure ForceBaseLayoutChange;
  var
    Reg: TRegistryErrorControlled;
  begin
    // This is hacky, maybe just remove the registry value, however that
    // would not force a recompile if the was the default base layout.
    // Options.Apply re-compiles only when it observes a changed base layout.
    // The caller may be repairing missing files for the already-selected layout.
    Reg := TRegistryErrorControlled.Create;
    try
      if Reg.OpenKey(SRegKey_KeymanEngine_CU, True) then
        Reg.WriteString(SRegValue_UnderlyingLayout, '00000000');
    finally
      Reg.Free;
    end;
  end;

begin
  Result := False;
  if not TryStrToInt('$' + BaseKeyboardIDText, BaseKeyboardID) or
    not kmcom.SystemInfo.IsAdministrator then
    Exit;

  SavePreviousRegistryBaseKeyboardValue;
  PreviousBaseKeyboardID := kmcom.Options['koBaseLayout'].Value;
  kmcom.Options['koBaseLayout'].Value := BaseKeyboardID;
  try
    if PreviousBaseKeyboardID = BaseKeyboardID then
      ForceBaseLayoutChange;
    kmcom.Options.Apply;
    Result := True;
  finally
    kmcom.Options['koBaseLayout'].Value := PreviousBaseKeyboardID;
    RestorePreviousBaseKeyboardValue;
  end;
end;

function BaseKeyboardNeedsMCompile(BaseKeyboardID: Integer): Boolean;
var
  I: Integer;
  Keyboard: IKeymanKeyboardInstalled;
  BaseFileName: string;
  BaseKeyboardIDHex: string;
begin
  BaseKeyboardIDHex := IntToHex(BaseKeyboardID, 8);
  for I := 0 to kmcom.Keyboards.Count - 1 do
  begin
    Keyboard := kmcom.Keyboards.Items[I];
    BaseFileName := Keyboard.Filename;
    if FileExists(BaseFileName) and
      (not FileExists(ChangeFileExt(BaseFileName, '') + '-' + BaseKeyboardIDHex + '.kmx') or
       not FileExists(ChangeFileExt(BaseFileName, '') + '-' + BaseKeyboardIDHex + '-d.kmx')) then
      Exit(True);
  end;
  Result := False;
end;

function SetBaseKeyboard(WindowHandle: THandle; BaseKeyboardID: Integer): Boolean;
var
  MCompileResult: Boolean;
begin
  MCompileResult := True;
  Result := False;
  if BaseKeyboardNeedsMCompile(BaseKeyboardID) and not kmcom.SystemInfo.IsAdministrator then
      MCompileResult := WaitForElevatedConfiguration(WindowHandle, '-mcompile ' + IntToHex(BaseKeyboardID, 8)) = 0;
  if not MCompileResult then
    Exit;
  kmcom.Options['koBaseLayout'].Value := BaseKeyboardID;
  kmcom.Options.Apply; // This will trigger a recompile if needed
  Result := True;
end;

end.
