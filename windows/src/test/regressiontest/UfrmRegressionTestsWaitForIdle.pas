unit UfrmRegressionTestsWaitForIdle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmRegressionTestsWaitForIdle = class(TForm)
    cmdCancel: TButton;
    Label1: TLabel;
    lblDownKeys: TLabel;
    Label3: TLabel;
    lblActiveKeys: TLabel;
    cmdForce: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmdForceClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FShouldShowModal: Boolean;
    function FindKeys: Boolean;
  public
    property ShouldShowModal: Boolean read FShouldShowModal;
  end;

implementation

uses KeyNames;

{$R *.DFM}

function TfrmRegressionTestsWaitForIdle.FindKeys: Boolean;
var
  keys: TKeyboardState;
  sa, sd: string;
  i: Integer;
begin
  sa := ''; sd := '';
  GetKeyboardState(keys);
  for i := 0 to 255 do
  begin
    if ((keys[i] and 1) = 1) and
        (i in [VK_CAPITAL, VK_NUMLOCK, VK_SCROLL]) then
      sa := sa + SKeyNames[i] + ' ';
    if (keys[i] and $80) = $80 then
      sd := sd + SKeyNames[i] + ' ';
  end;
  Result := (sa <> '') or (sd <> '');
  lblActiveKeys.Caption := sa;
  lblDownKeys.Caption := sd;
end;

procedure TfrmRegressionTestsWaitForIdle.FormCreate(Sender: TObject);
begin
  FShouldShowModal := FindKeys;
end;

procedure TfrmRegressionTestsWaitForIdle.FormKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if not FindKeys then ModalResult := mrOk;
end;

procedure TfrmRegressionTestsWaitForIdle.cmdForceClick(Sender: TObject);
var
  keys: TKeyboardState;
  i: Integer;
begin
  for i := 0 to 255 do keys[i] := 0;
  SetKeyboardState(keys);
  if not FindKeys then ModalResult := mrOk;
end;

procedure TfrmRegressionTestsWaitForIdle.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FindKeys;
end;

end.
