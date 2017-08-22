unit UfrmVKKeyMap;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, OnScreenKeyboard, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    kbd: TOnScreenKeyboard;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    procedure MapKeys;
  protected
    procedure WMInputlangchange(var Message: TMessage);
      message WM_INPUTLANGCHANGE;
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Registry, RegistryKeys, VKeys;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  MapKeys;
end;

procedure TForm1.MapKeys;
var
  i: Integer;
  x: Integer;
  buf: array[0..KL_NAMELENGTH] of char;
  s: string;
begin
  GetKeyboardLayoutName(buf);
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeyboardLayouts + '\' + buf) and ValueExists('Layout Text')
      then s := ReadString('Layout Text')
      else s := buf;
  finally
    Free;
  end;
  panel1.Caption := s;
  kbd.UnderlyingLayout := GetKeyboardLayout(0);
  for i := 0 to kbd.Keys.Count - 1 do
  begin
//    kbd.Keys[i].KeyCaps[0] := VKeyNames[kbd.Keys[i].VKey];
    if kbd.Keys[i].USVKey <> kbd.Keys[i].VKey then
      kbd.Keys[i].KeyValue := IntToHex(kbd.Keys[i].VKey, 2)
    else
      kbd.Keys[i].KeyValue := '';
  end;
end;

procedure TForm1.WMInputlangchange(var Message: TMessage);
begin
  MapKeys;
end;

end.
