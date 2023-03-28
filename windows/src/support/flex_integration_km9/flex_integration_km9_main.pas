unit flex_integration_km9_main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections;

type
  TKeymanKeyboard = class
    id: Integer;
    name: string;
  end;

  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fKeyboards: TObjectList<TKeymanKeyboard>;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  PGETACTIVEKEYMANIDFUNC = function: Integer; stdcall;

procedure TForm1.Button1Click(Sender: TObject);
var
  hKeyman: HMODULE;
  pGetActiveKeymanID: PGETACTIVEKEYMANIDFUNC;
  nActiveKeymanId: Integer;
  iki: Integer;
begin
	hKeyman := GetModuleHandle('keyman32-ver17.0.48-alpha-local.dll');
	if hKeyman = 0 then
  begin
    ShowMessage('No keyman32.dll loaded');
    Exit;
  end;

  pGetActiveKeymanID := GetProcAddress(hKeyman, 'GetActiveKeymanID');
	if @pGetActiveKeymanID = nil then
  begin
    ShowMessage('No GetActiveKeymanID');
    Exit;
  end;

	nActiveKeymanId := pGetActiveKeymanID;

	if nActiveKeymanId = -1 then
  begin
    ShowMessage('Keyman ActiveID = NONE');
    Exit;
  end;

	for iki := 0 to FKeyboards.Count-1 do
  begin
    if FKeyboards[iki].id = nActiveKeymanID then
    begin
      ShowMessage(Format('Keyman ID is %d : %s', [nActiveKeymanID, FKeyboards[iki].Name]));
      Exit;
    end;
  end;

  ShowMessage('Keyman ID '+IntToStr(nActiveKeymanID)+' not found');
end;

type
  TKeyboardInfo = record
	KeymanID: DWORD;
	HotKey: DWORD;
	KeyboardID: DWORD;
	Name: array[0..255] of ansichar;
	// The original type for pKeyboard was LPKEYBOARD Keyboard, but we don't have defn of
	// LPKEYBOARD
	pKeyboard: Pointer;
	nIMDLLs: DWORD;
	IMDLLs: DWORD;
	// JohnT: The definition Marc gave me didn't have this, but the objects version 6 returns
	// are four bytes bigger than Marc's typedef indicated. This aligns things properly and
	// makes sure we allocate enough memory.
	//DWORD Dummy;
  end;

  pKeyboardInfo = ^TKeyboardInfo;
  PFNKeyman_BuildKeyboardList = function(p:pKeyboardInfo; var n: Integer): BOOL; stdcall;

procedure TForm1.Button2Click(Sender: TObject);
var
  hKeyman: HModule;
  pKeyboards: PKeyboardInfo;
  pKeyman_BuildKeyboardList: PFNKeyman_BuildKeyboardList;
  nKeyboards: Integer;
  i: Integer;
  k: TKeymanKeyboard;
begin
  FKeyboards := TObjectList<TKeymanKeyboard>.Create;

	hKeyman := GetModuleHandle('keyman32-ver17.0.48-alpha-local.dll');
	if hKeyman = 0 then Exit;
  pKeyman_BuildKeyboardList := GetProcAddress(hKeyman, 'Keyman_BuildKeyboardList');
  if @pKeyman_BuildKeyboardList = nil then
    Exit;

  nKeyboards := 0;
	pKeyman_BuildKeyboardList(nil, nKeyboards);
	if nKeyboards = 0 then Exit;

  pKeyboards := AllocMem(nKeyboards * sizeof(TKeyboardInfo));
	pKeyman_BuildKeyboardList(pKeyboards, nKeyboards);

  for i := 0 to nKeyboards - 1 do
  begin
    k := TKeymanKeyboard.Create;
    k.id := pKeyboards.KeymanID;
    k.Name := string(pKeyboards.Name);
    FKeyboards.Add(k);
    memo1.Lines.Add(IntToStr(k.id)+':'+ k.Name);
    Inc(pKeyboards);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  hKeyman: HModule;
  pKeyboards: PKeyboardInfo;
  pKeyman_BuildKeyboardList: PFNKeyman_BuildKeyboardList;
  nKeyboards: Integer;
  i: Integer;
  k: TKeymanKeyboard;
begin
  FKeyboards := TObjectList<TKeymanKeyboard>.Create;

	hKeyman := GetModuleHandle('keyman32-ver17.0.48-alpha-local.dll');
	if hKeyman = 0 then Exit;
  pKeyman_BuildKeyboardList := GetProcAddress(hKeyman, 'Keyman_BuildKeyboardList');
  if @pKeyman_BuildKeyboardList = nil then
    Exit;

  nKeyboards := 0;
	pKeyman_BuildKeyboardList(nil, nKeyboards);
	if nKeyboards = 0 then Exit;

  pKeyboards := AllocMem(nKeyboards * sizeof(TKeyboardInfo));
	pKeyman_BuildKeyboardList(pKeyboards, nKeyboards);

  for i := 0 to nKeyboards - 1 do
  begin
    k := TKeymanKeyboard.Create;
    k.id := pKeyboards.KeymanID;
    k.Name := string(pKeyboards.Name);
    FKeyboards.Add(k);
    memo1.Lines.Add(IntToStr(k.id)+':'+ k.Name);
    Inc(pKeyboards);
  end;
end;

end.
