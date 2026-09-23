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

(**
 * Displays a form for the user to select a base keyboard. If the user selects a base
 * keyboard, the KLID is used to Set the Base Keyboard.
 *
 * @returns  True  if the user selected base keyboard has been set.
 *)
function ConfigureAndSetBaseKeyboard(WindowHandle: THandle): Boolean;


implementation

{$R *.dfm}

uses
  BaseKeyboards,
  ErrorControlledRegistry,
  RegistryKeys,
  keymanapi_TLB,
  Keyman.Configuration.System.BaseKeyboard,
  kmint,
  utilkmshell;


function ConfigureAndSetBaseKeyboard(WindowHandle: THandle): Boolean;
begin
  with TfrmBaseKeyboard.Create(nil) do
  try
    Result := ShowModal = mrOk;
    if Result then
      SetBaseKeyboard(WindowHandle, FBaseKeyboardID)
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


end.
