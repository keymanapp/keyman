unit UfrmSendInputTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, TntStdCtrls;

type
  TForm1 = class(TForm)
    editText: TTntEdit;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    chkTimer: TTntCheckBox;
    memoLog: TTntMemo;
    cmdGo: TTntButton;
    lblCountdown: TTntLabel;
    timerSend: TTimer;
    timerCountdown: TTimer;
    procedure timerCountdownTimer(Sender: TObject);
    procedure cmdGoClick(Sender: TObject);
    procedure timerSendTimer(Sender: TObject);
  private
    FCountdown: Integer;
    FIndex: Integer;
    procedure Fire;
    procedure FireNext;
    procedure Log(s: WideString);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  KEYEVENTF_UNICODE = 4;

procedure TForm1.cmdGoClick(Sender: TObject);
begin
  FCountdown := 4;
  cmdGo.Enabled := False;
  lblCountdown.Caption := IntToStr(FCountdown);
  timerCountdown.Enabled := True;
end;

procedure TForm1.Log(s: WideString);
begin
  memoLog.Text := memoLog.Text + s + #13#10;
end;

procedure TForm1.Fire;
var
  pi, inputs: PInput;
  I: Integer;
  n: Integer;
begin
  if chkTimer.Checked then
  begin
    FIndex := 1;
    timerSend.Enabled := True;
  end
  else
  begin
    n := Length(editText.Text);
    inputs := AllocMem(n * sizeof(TInput) * 2);
    pi := inputs;
    for I := 1 to n do
    begin
      pi.Itype := INPUT_KEYBOARD;
      pi.ki.wVk := 0;
      pi.ki.wScan := Ord(editText.Text[I]);
      pi.ki.dwFlags := KEYEVENTF_UNICODE;
      pi.ki.time := 0;
      pi.ki.dwExtraInfo := 0;
      Inc(pi);

      pi.Itype := INPUT_KEYBOARD;
      pi.ki.wVk := 0;
      pi.ki.wScan := Ord(editText.Text[I]);
      pi.ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
      pi.ki.time := 0;
      pi.ki.dwExtraInfo := 0;
      Inc(pi);
    end;

    Log('Sending ['+editText.Text+']');
    SendInput(n * 2, inputs^, sizeof(TInput));

    FreeMem(inputs);

    cmdGo.Enabled := True;
    lblCountdown.Caption := '';
  end;
end;

procedure TForm1.FireNext;
var
  input: array[0..1] of TInput;
  pi: PInput;
begin
  if FIndex <= Length(editText.Text) then
  begin
    pi := @input[0];
    pi.Itype := INPUT_KEYBOARD;
    pi.ki.wVk := 0;
    pi.ki.wScan := Ord(editText.Text[FIndex]);
    pi.ki.dwFlags := KEYEVENTF_UNICODE;
    pi.ki.time := 0;
    pi.ki.dwExtraInfo := 0;
    Inc(pi);

    pi.Itype := INPUT_KEYBOARD;
    pi.ki.wVk := 0;
    pi.ki.wScan := Ord(editText.Text[FIndex]);
    pi.ki.dwFlags := KEYEVENTF_UNICODE or KEYEVENTF_KEYUP;
    pi.ki.time := 0;
    pi.ki.dwExtraInfo := 0;

    Log('Sending ['+editText.Text[FIndex]+']');
    SendInput(2, input[0], sizeof(TInput));

    Inc(FIndex);
  end
  else
  begin
    timerSend.Enabled := False;
    cmdGo.Enabled := True;
    lblCountdown.Caption := '';
  end;
end;

procedure TForm1.timerCountdownTimer(Sender: TObject);
begin
  Dec(FCountdown);
  if FCountdown = 0 then
  begin
    timerCountdown.Enabled := False;
    lblCountdown.Caption := 'Go';
    Fire;
  end
  else
    lblCountdown.Caption := IntToStr(FCountdown);
end;

procedure TForm1.timerSendTimer(Sender: TObject);
begin
  FireNext;
end;

end.
