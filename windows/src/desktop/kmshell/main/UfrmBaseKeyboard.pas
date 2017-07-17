unit UfrmBaseKeyboard;

interface

uses
  System.Contnrs,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmWebContainer, Vcl.OleCtrls,
  SHDocVw_EWB, EwbCore, EmbeddedWB, KeymanEmbeddedWB;

type
  TfrmBaseKeyboard = class(TfrmWebContainer)
    procedure TntFormCreate(Sender: TObject);
  private
    procedure Footer_Cancel;
    procedure Footer_OK(params: TStringList);
  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  end;

function ConfigureBaseKeyboard: Boolean;

implementation

{$R *.dfm}

uses
  BaseKeyboards,
  GenericXMLRenderer,
  kmint;

function ConfigureBaseKeyboard: Boolean;
begin
  with TfrmBaseKeyboard.Create(nil) do
  try
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

procedure TfrmBaseKeyboard.TntFormCreate(Sender: TObject);
var
  xml: WideString;
begin
  inherited;

  xml := TBaseKeyboards.EnumerateXML(kmcom.Options['koBaseLayout'].Value);

  XMLRenderers.RenderTemplate := 'BaseKeyboard.xsl';
  XMLRenderers.Add(TGenericXMLRenderer.Create(xml));

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
  kmcom.Options['koBaseLayout'].Value := v;
  kmcom.Options.Apply;
  ModalResult := mrOk;
end;

end.
