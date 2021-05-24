unit ShiftStateMap;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.AppEvnts;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    cmdPressLShift: TButton;
    Timer1: TTimer;
    cmdPressRShift: TButton;
    cmdPressRCtrl: TButton;
    cmdPressLCtrl: TButton;
    cmdPressLAlt: TButton;
    cmdPressRAlt: TButton;
    cmdReleaseLShift: TButton;
    cmdReleaseRShift: TButton;
    cmdReleaseRCtrl: TButton;
    cmdReleaseLCtrl: TButton;
    cmdReleaseLAlt: TButton;
    cmdReleaseRAlt: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ApplicationEvents1: TApplicationEvents;
    cmdMapVirtualKey: TButton;
    procedure cmdPressClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure cmdReleaseClick(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure cmdMapVirtualKeyClick(Sender: TObject);
  private
    LastS: string;
    procedure DoKeyEvent(st: Integer; flags: DWord);
    procedure LogMessage(msg: tagMsg);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TKeyInfo = record
    Code: Integer;
    Scan: Word;
    Flags: DWord;
  end;

const
  Keys: array[0..5] of TKeyInfo = (
    (Code: VK_SHIFT;   Scan: $2A; Flags: 0),
    (Code: VK_SHIFT;   Scan: $36; Flags: 0), //KEYEVENTF_EXTENDEDKEY),
    (Code: VK_CONTROL; Scan: $1D; Flags: 0),
    (Code: VK_CONTROL; Scan: $1D; Flags: KEYEVENTF_EXTENDEDKEY),
    (Code: VK_MENU;    Scan: $38; Flags: 0),
    (Code: VK_MENU;    Scan: $38; Flags: KEYEVENTF_EXTENDEDKEY)
  );

procedure TForm1.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  if (Msg.message = WM_KEYDOWN) or (msg.message = WM_KEYUP) or
    (Msg.message = WM_SYSKEYDOWN) or (msg.message = WM_SYSKEYUP) then
    LogMessage(Msg);
end;

procedure TForm1.cmdMapVirtualKeyClick(Sender: TObject);
begin
  memo1.Lines.Add(Format('MVK: Shift=(L:%x R:%x %x) Ctrl=(L:%x R:%x %x) Alt=(L:%x R:%x %x)',
    [
    MapVirtualKey(VK_LSHIFT, MAPVK_VK_TO_VSC),
    MapVirtualKey(VK_RSHIFT, MAPVK_VK_TO_VSC),
    MapVirtualKey(VK_SHIFT,  MAPVK_VK_TO_VSC),
    MapVirtualKey(VK_LCONTROL, MAPVK_VK_TO_VSC),
    MapVirtualKey(VK_RCONTROL, MAPVK_VK_TO_VSC),
    MapVirtualKey(VK_CONTROL,  MAPVK_VK_TO_VSC),
    MapVirtualKey(VK_LMENU, MAPVK_VK_TO_VSC),
    MapVirtualKey(VK_RMENU, MAPVK_VK_TO_VSC),
    MapVirtualKey(VK_MENU,  MAPVK_VK_TO_VSC)
    ]));

  memo1.Lines.Add(Format('MVKEx: Shift=(L:%x R:%x %x) Ctrl=(L:%x R:%x %x) Alt=(L:%x R:%x %x)',
    [
    MapVirtualKeyEx(VK_LSHIFT, MAPVK_VK_TO_VSC, 0),
    MapVirtualKeyEx(VK_RSHIFT, MAPVK_VK_TO_VSC, 0),
    MapVirtualKeyEx(VK_SHIFT,  MAPVK_VK_TO_VSC, 0),
    MapVirtualKeyEx(VK_LCONTROL, MAPVK_VK_TO_VSC, 0),
    MapVirtualKeyEx(VK_RCONTROL, MAPVK_VK_TO_VSC, 0),
    MapVirtualKeyEx(VK_CONTROL,  MAPVK_VK_TO_VSC, 0),
    MapVirtualKeyEx(VK_LMENU, MAPVK_VK_TO_VSC, 0),
    MapVirtualKeyEx(VK_RMENU, MAPVK_VK_TO_VSC, 0),
    MapVirtualKeyEx(VK_MENU,  MAPVK_VK_TO_VSC, 0)
    ]));

  memo1.Lines.Add(Format('MVKExEx: Shift=(L:%x R:%x %x) Ctrl=(L:%x R:%x %x) Alt=(L:%x R:%x %x)',
    [
    MapVirtualKeyEx(VK_LSHIFT, MAPVK_VK_TO_VSC_EX, 0),
    MapVirtualKeyEx(VK_RSHIFT, MAPVK_VK_TO_VSC_EX, 0),
    MapVirtualKeyEx(VK_SHIFT,  MAPVK_VK_TO_VSC_EX, 0),
    MapVirtualKeyEx(VK_LCONTROL, MAPVK_VK_TO_VSC_EX, 0),
    MapVirtualKeyEx(VK_RCONTROL, MAPVK_VK_TO_VSC_EX, 0),
    MapVirtualKeyEx(VK_CONTROL,  MAPVK_VK_TO_VSC_EX, 0),
    MapVirtualKeyEx(VK_LMENU, MAPVK_VK_TO_VSC_EX, 0),
    MapVirtualKeyEx(VK_RMENU, MAPVK_VK_TO_VSC_EX, 0),
    MapVirtualKeyEx(VK_MENU,  MAPVK_VK_TO_VSC_EX, 0)
    ]));
end;

procedure TForm1.cmdPressClick(Sender: TObject);
begin
  DoKeyEvent(TButton(Sender).Tag, 0);
end;

procedure TForm1.cmdReleaseClick(Sender: TObject);
begin
  DoKeyEvent(TButton(Sender).Tag, KEYEVENTF_KEYUP);
end;

procedure TForm1.DoKeyEvent(st: Integer; flags: DWord);
var
  i: TInput;
begin
  FillChar(i, sizeof(TINPUT), 0);
  i.Itype := INPUT_KEYBOARD;
  i.ki.wVk := Keys[st].Code;
  i.ki.wScan := Keys[st].Scan; //$FF;
  i.ki.dwFlags := flags or Keys[st].Flags;
  memo1.SetFocus;
  if SendInput(1, i, sizeof(TINPUT)) = 0 then
    RaiseLastOSError;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  ks: TKeyboardState;
  s: string;
  function KSS(n: Byte): string;
  begin
    if (ks[n] and $80) <> 0 then Exit('DOWN') else Exit('    ');
  end;

begin
  GetKeyboardState(ks);
  s := Format('Shift=(L:%s R:%s %s) Ctrl=(L:%s R:%s %s) Alt=(L:%s R:%s %s)',
    [KSS(VK_LSHIFT), KSS(VK_RSHIFT), KSS(VK_SHIFT),
    KSS(VK_LCONTROL), KSS(VK_RCONTROL), KSS(VK_CONTROL),
    KSS(VK_LMENU), KSS(VK_RMENU), KSS(VK_MENU)]);
  if s <> LastS then
  begin
    memo1.Lines.Add(s);
    LastS := s;
  end;
end;

procedure TForm1.LogMessage(msg: tagMsg);
var
  m: string;
begin
  case msg.message of
    WM_KEYDOWN: m := 'WM_KEYDOWN';
    WM_KEYUP: m := 'WM_KEYUP';
    WM_SYSKEYDOWN: m := 'WM_SYSKEYDOWN';
    WM_SYSKEYUP: m := 'WM_SYSKEYUP';
    else m := '?';
  end;
  memo1.Lines.Add(Format('%s wParam=%x (%s) lParam=%x', [m, msg.wParam, '?', msg.lParam]));
end;

end.
