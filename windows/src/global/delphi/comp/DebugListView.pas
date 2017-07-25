unit DebugListView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

type
  TDebugListView = class(TListView)
  private
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  end;

procedure Register;

implementation

procedure TDebugListView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure Register;
begin
  RegisterComponents('Keyman', [TDebugListView]);
end;

end.
