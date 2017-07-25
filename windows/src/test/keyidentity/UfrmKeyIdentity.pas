unit UfrmKeyIdentity;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    lblVK: TLabel;
    Label3: TLabel;
    lblScan: TLabel;
    Label5: TLabel;
    lblChar: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    BaseTickCount: Cardinal;
    procedure Log(s: string);
    procedure LogKey(const msg: string; CharCode: Word; Keydata: DWord);
  protected
    procedure WMSysKeyDown(var Message: TWMKeyDown); message WM_SYSKEYDOWN;
    procedure WMSysKeyUp(var Message: TWMKeyUp); message WM_SYSKEYUP;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    { Private declarations }
  public

    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  UfrmKeyIdentityLog;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BaseTickCount := GetTickCount;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Left := 64;
  Top := 64;
  frmKeyIdentityLog.Left := Left+Width;
  frmKeyIdentityLog.Top := Top;
  frmKeyIdentityLog.Show;
end;

procedure TForm1.WMChar(var Message: TWMChar);
begin
  lblChar.Caption := IntToStr(Message.CharCode) + ' 0x'+IntToHex(message.CharCode,2)+' "'+Char(Message.CharCode)+'"';
  Log('WM_CHAR '+IntToStr(Message.CharCode) + ' 0x'+IntToHex(message.CharCode,2)+' "'+Char(Message.CharCode)+'"');
end;

procedure TForm1.LogKey(const msg: string; CharCode: Word; Keydata: DWord);
var
  ScanCode, RepeatCount: Integer;
  IsExtended: Boolean;
  IsKeyAlreadyDown: Boolean;
  IsTransition: Boolean;
begin
  ScanCode := (KeyData and $FF0000) shr 16;
  RepeatCount := (KeyData and $FFFF);
  IsExtended := (KeyData and $1000000) <> 0;
  IsKeyAlreadyDown := (KeyData and $40000000) <> 0;
  IsTransition := (KeyData and $80000000) <> 0;

  if IsTransition = IsKeyAlreadyDown then
    Log(msg+' ' +IntToStr(CharCode) + ' ' + '0x'+IntToHex(CharCode,2) + '     transition='+BoolToStr(IsTransition,True)+' down='+BoolToStr(IsKeyAlreadyDown, True)+' repeat='+IntToStr(RepeatCount)+'; scan=0x'+IntToHex(ScanCode,2)+'; Extended='+BoolToStr(IsExtended, True));
end;

procedure TForm1.WMKeyDown(var Message: TWMKeyDown);
begin
  lblScan.Caption := IntToStr((Message.KeyData and $FF0000) shr 16) +
   ' 0x'+IntToHex((Message.KeyData and $FF0000) shr 16, 2);
  lblVk.Caption := IntToStr(Message.CharCode) + ' ' + '0x'+IntToHex(Message.CharCode,2);
  LogKey('WM_KEYDOWN', Message.CharCode, Message.KeyData);
end;

procedure TForm1.WMKeyUp(var Message: TWMKeyUp);
begin
  LogKey('WM_KEYUP  ', Message.CharCode, Message.KeyData);
end;

procedure TForm1.WMSysKeyDown(var Message: TWMKeyDown);
begin
  LogKey('WM_SYSKEYDOWN', Message.CharCode, Message.KeyData);
end;

procedure TForm1.WMSysKeyUp(var Message: TWMKeyUp);
begin
  LogKey('WM_SYSKEYUP', Message.CharCode, Message.KeyData);
end;

procedure TForm1.Log(s: string);
begin
  frmKeyIdentityLog.memoLog.Lines.Add(Format('%8d', [GetTickCount-BaseTickCount])+' '+s);
end;

end.
