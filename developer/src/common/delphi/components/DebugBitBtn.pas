unit DebugBitBtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TDebugBitBtn = class(TBitBtn)
  private
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  end;

procedure Register;

implementation

{ TDebugBitBtn }

procedure TDebugBitBtn.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure Register;
begin
  RegisterComponents('Keyman', [TDebugBitBtn]);
end;

end.
