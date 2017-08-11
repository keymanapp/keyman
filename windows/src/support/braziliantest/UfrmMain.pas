unit UfrmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    tabs: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    TabSheet8: TTabSheet;
    Label16: TLabel;
    Label17: TLabel;
    memoLog: TMemo;
    cmdExit: TButton;
    cmdNext: TButton;
    editTestBr: TEdit;
    editTestUS: TEdit;
    editTestKM: TEdit;
    Label9: TLabel;
    Label14: TLabel;
    procedure cmdExitClick(Sender: TObject);
    procedure cmdNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure KeyboardInfo(s: string);
    { Private declarations }
  public
    function IsShortCut(var Message: TWMKey): Boolean; override;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.cmdExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.KeyboardInfo(s: string);
var
  n: Integer;
  str: string;
  buf: array[0..256] of char;
begin
  n := GetKeyboardLayout(0);
  GetKeyboardLayoutName(buf); str := buf;
  memoLog.Lines.Add(Format('Selected %s keyboard: %8.8X, %s', [s, n, str]));
end;

procedure TForm1.cmdNextClick(Sender: TObject);
begin
  tabs.ActivePageIndex := tabs.ActivePageIndex + 1;
  case tabs.ActivePageIndex of
    1: KeyboardInfo('Brazilian');
    2: editTestBr.SetFocus;
    3: KeyboardInfo('US English');
    4: editTestUS.SetFocus;
    5: KeyboardInfo('Keyman/Brazilian');
    6: editTestKM.SetFocus;
  end;

  if tabs.ActivePageIndex = 7 then cmdNext.Enabled := False;
end;

function TForm1.IsShortCut(var Message: TWMKey): Boolean;
var
  msg: string;
  sc: DWord;
  ext: Integer;
  bt: TKeyboardState;
  buf: array[0..2] of Word;
  ch: Char;
begin
  if (Message.Msg = CN_KEYDOWN) and (editTestBR.Focused or editTestKM.Focused or editTestUS.Focused) then
  begin
    //if Message.Msg = CN_KEYDOWN then
    msg := 'WM_KEYDOWN';// else msg := 'WM_KEYUP';
    sc := (Message.KeyData and $00FF0000) shr 16;
    if (Message.KeyData and $01000000) = $1000000 then ext := 1 else ext := 0;

    GetKeyboardState(bt);
    ToAscii(Message.CharCode, sc, bt, @buf, 0);
    ch := Chr(buf[0]);

    memoLog.Lines.Add(Format('%s: key: 0x%x scan: 0x%x ext: %d char: ''%s'' [0x%08.8X, 0x%08.8x]',
                  [msg, Message.CharCode, sc, ext, ch, Message.CharCode, Message.KeyData]));
  end;
  Result := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  tabs.ActivePageIndex := 0;
end;

end.
