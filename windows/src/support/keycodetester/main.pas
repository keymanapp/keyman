unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    memoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure AppMsg(var Message: Tmsg; var Handled: Boolean);
    procedure LogKeyEvent(msg: Cardinal; wParam, lParam: Integer);
    procedure LogKeyPress(msg: Cardinal; wParam, lParam: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  application.OnMessage := AppMsg;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Application.OnMessage := nil;
end;

procedure TForm1.AppMsg(var Message: Tmsg; var Handled: Boolean);
begin
  Handled := False;
  case Message.message of
    WM_KEYDOWN, WM_KEYUP, WM_SYSKEYDOWN, WM_SYSKEYUP:
      if Message.hwnd = memo1.Handle then LogKeyEvent(Message.message, Message.wParam, Message.lParam);

    WM_CHAR, WM_SYSCHAR:
      if Message.hwnd = memo1.Handle then LogKeyPress(Message.message, Message.wParam, Message.lParam);
  end;

end;

procedure TForm1.LogKeyPress(msg: Cardinal; wParam, lParam: Integer);
begin

end;

procedure TForm1.LogKeyEvent(msg: Cardinal; wParam, lParam: Integer);
var
  amsg: string;
  sc: DWord;
  ext: Integer;
  bt: TKeyboardState;
  buf: array[0..2] of Word;
  ch: Char;
  chs: string;
begin
  if Msg = WM_KEYDOWN then
    amsg := 'WM_KEYDOWN' else amsg := 'WM_KEYUP';
  sc := (lParam and $00FF0000) shr 16;
  if (lParam and $01000000) = $1000000 then ext := 1 else ext := 0;

  GetKeyboardState(bt);
  ToAscii(wParam, sc, bt, @buf, 0);
  ch := Chr(buf[0]); if ch = #0 then chs := '' else chs := ch;


  memoLog.Lines.Add(Format('%s: key: 0x%x scan: 0x%x ext: %d char: ''%s'' [0x%08.8X, 0x%08.8x]',
                [amsg, wParam, sc, ext, chs, wParam, lParam]));
end;

end.
