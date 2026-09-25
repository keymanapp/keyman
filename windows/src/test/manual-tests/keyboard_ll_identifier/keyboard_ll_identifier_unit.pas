unit keyboard_ll_identifier_unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure AppOnMessage(var Msg: TMsg; var Handled: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  hh: Cardinal;

type
  TKbdLlHookStruct = record
    vkCode, scanCode, flags, time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  PKbdLlHookStruct = ^TKbdLlHookStruct;

function kbdll(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  s: PKbdllhookstruct;
begin
  if nCode < 0 then
  begin
    Exit(CallNextHookEx(hh, nCode, wParam, lParam));
  end;

  s := PkbdllHookStruct(lParam);

  if s.vkCode in [VK_LCONTROL, VK_RCONTROL, VK_LMENU, VK_RMENU, VK_LSHIFT, VK_RSHIFT, VK_CONTROL, VK_SHIFT, VK_MENU] then
    if Assigned(Form1) then
    begin
      Form1.memo1.Lines.Add(Format('%08.8x %08.8x %08.8x', [s.vkCode, s.scanCode, s.flags]));
    end;

  Exit(CallNextHookEx(hh, nCode, wParam, lParam));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  Uncomment to monitor WM_KEY* and WM_SYSKEY* messages 
//  Application.OnMessage := AppOnMessage;


  hh := SetWindowsHookEx(WH_KEYBOARD_LL, kbdll, GetModuleHandle(nil), 0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Form1 := nil;
  UnhookWindowsHookEx(hh);
end;

procedure TForm1.AppOnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if (Msg.message = WM_KEYDOWN) or (Msg.message = WM_SYSKEYDOWN) or (Msg.message = WM_KEYUP) or (Msg.message = WM_SYSKEYUP) then
  begin
    if Msg.wParam in [VK_CONTROL, VK_SHIFT, VK_MENU] then
      if (Msg.LParam and $FFFF) = 1 then
      begin
        memo1.Lines.Add(Format('%08.8x %08.8x', [Msg.wParam, Msg.lParam]));
      end;
  end;
end;

end.
