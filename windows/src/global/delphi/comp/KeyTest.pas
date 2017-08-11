unit KeyTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
  CN_SYSKEYUP = CN_BASE + WM_SYSKEYUP;
  
type
  TExtShiftState = set of (essShift, essCtrl, essAlt, essLCtrl, essRCtrl, essLAlt, essRAlt);
  TKeyPressedEvent = procedure(Sender: TObject; Key: Word; KeyData: DWord; ShiftState: TExtShiftState) of object;

  TKeyTest = class(TCustomControl)
  private
    FOnKeyPressed: TKeyPressedEvent;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMSysKeyDown(var Message: TWMKeyDown); message WM_SYSKEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMSysKeyUp(var Message: TWMKeyUp); message WM_SYSKEYUP;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;

    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNSysKeyDown(var Message: TWMKeyDown); message CN_SYSKEYDOWN;
    procedure CNKeyUp(var Message: TWMKeyUp); message CN_KEYUP;
    procedure CNSysKeyUp(var Message: TWMKeyUp); message CN_SYSKEYUP;

  protected
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
  public
    b: Integer;
    constructor Create(AOwner: TComponent); override;
  published
    property TabStop;
    property OnKeyPressed: TKeyPressedEvent read FOnKeyPressed write FOnKeyPressed;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Keyman', [TKeyTest]);
end;

{ TKeyTest }

procedure TKeyTest.CMEnter(var Message: TCMGotFocus);
begin
  inherited;
  Repaint;
end;

procedure TKeyTest.CMExit(var Message: TCMExit);
begin
  inherited;
  Repaint;
end;

procedure TKeyTest.CNKeyDown(var Message: TWMKeyDown);
begin

end;

procedure TKeyTest.CNKeyUp(var Message: TWMKeyUp);
begin

end;

procedure TKeyTest.CNSysKeyDown(var Message: TWMKeyDown);
begin

end;

procedure TKeyTest.CNSysKeyUp(var Message: TWMKeyUp);
begin

end;

constructor TKeyTest.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := True;
end;

procedure TKeyTest.Paint;
begin
  inherited;
  if Focused then
    Canvas.TextOut(0, 0, 'KeyTest')
  else
    Canvas.TextOut(0, 0, '!');
end;

procedure TKeyTest.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTALLKEYS;
end;

procedure TKeyTest.WMKeyDown(var Message: TWMKeyDown);
begin
  if Assigned(FOnKeyPressed) then FOnKeyPressed(Self, Message.CharCode, Message.KeyData, []);
end;

procedure TKeyTest.WMKeyUp(var Message: TWMKeyUp);
begin

end;

procedure TKeyTest.WMLButtonDown(var Message: TWMLButtonDown);
begin
  SetFocus;
  Repaint;
end;

procedure TKeyTest.WMSysKeyDown(var Message: TWMKeyDown);
begin

end;

procedure TKeyTest.WMSysKeyUp(var Message: TWMKeyUp);
begin

end;

procedure TKeyTest.WndProc(var Message: TMessage);
begin
{  if (Message.Msg < WM_KEYFIRST) or (Message.Msg > WM_KEYLAST) or (csDesigning in ComponentState) then}
    inherited;
end;

end.
