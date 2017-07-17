unit OleCtrlsFix;

interface

uses
  OleCtrls, Windows, Messages;

type
  TOleControlFix = class(TOleControl)
  protected
  //  procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    function GetIDispatchProp(Index: Integer): IDispatch;
    function GetIUnknownProp(Index: Integer): IUnknown;
  end;

implementation

{ TOleControlFix }

{procedure TOleControlFix.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
begin
  if Message.DC = 0 then
  begin
    BeginPaint(Handle, PS);
    EndPaint(Handle, PS);
  end;
end; }

function TOleControlFix.GetIDispatchProp(Index: Integer): IDispatch;
var
  Temp: TVarData;
begin
  GetProperty(Index, Temp);
// Result := IDispatch(Temp.VDispatch); ***** Change to:
  Pointer(Result) := Temp.VDispatch; //this avoids the extra AddRef
end;

function TOleControlFix.GetIUnknownProp(Index: Integer): IUnknown;
var
  Temp: TVarData;
begin
  GetProperty(Index, Temp);
// Result := IDispatch(Temp.VUnknown); ***** Change to:
  Pointer(Result) := Temp.VUnknown; //this avoids the extra AddRef
end;
end.

