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
function CompileForBaseKeyboard(BaseKeyboardID: Integer): Boolean;

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
begin
  Result := False;
  if not TryStrToInt('$' + BaseKeyboardIDText, BaseKeyboardID) or
    not kmcom.SystemInfo.IsAdministrator then
    Exit;
  Result := CompileForBaseKeyboard(BaseKeyboardID);
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
  if BaseKeyboardNeedsMCompile(BaseKeyboardID) then
  begin
    if not kmcom.SystemInfo.IsAdministrator then
      begin
        MCompileResult := WaitForElevatedConfiguration(WindowHandle, '-mcompilekbds ' + IntToHex(BaseKeyboardID, 8)) = 0;
      end
    else
      MCompileResult := CompileForBaseKeyboard(BaseKeyboardID);
  end;
  if not MCompileResult then
    Exit;

  kmcom.Options['koBaseLayout'].Value := BaseKeyboardID;
  kmcom.Options.Apply;
  Result := True;
end;

function CompileForBaseKeyboard(BaseKeyboardID: Integer): Boolean;
var
  i: Integer;
  kbd: IKeymanKeyboardInstalled;
begin
  for i := 0 to kmcom.Keyboards.Count - 1 do
    begin
      kbd := kmcom.Keyboards[i];
      (kbd as IKeymanKeyboardInstalled2).MCompileForBaseKeyboard(BaseKeyboardID);
    end;
end;

end.
