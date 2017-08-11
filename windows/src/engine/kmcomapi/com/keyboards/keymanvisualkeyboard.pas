(*
  Name:             keymanvisualkeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    25 Jan 2011
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Add visual keyboard reference updating
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    25 Jan 2011 - mcdurdin - I2329 - Fix issues printing OSK on x64
*)
unit keymanvisualkeyboard;

interface

uses
  ComObj, ActiveX, keymanapi_TLB, internalinterfaces, StdVcl, keymanautoobject, keymancontext;

type
  TKeymanVisualKeyboard = class(TKeymanAutoObject, IKeymanVisualKeyboard)
  private
    FKeyboardName, FFileName: string;
    function GetOwnerKeyboard: IKeymanKeyboardInstalled;

    { IKeymanVisualKeyboard }
    function Get_Filename: WideString; safecall;

    procedure Uninstall; safecall;
  public
    constructor Create(AContext: TKeymanContext; const AFileName, AKeyboardName: string);
  end;

implementation

uses ComServ, custinterfaces, kpuninstallvisualkeyboard, keymankeyboardinstalled;

constructor TKeymanVisualKeyboard.Create(AContext: TKeymanContext;
  const AFileName, AKeyboardName: string);
begin
  inherited Create(AContext, IKeymanVisualKeyboard);
  FKeyboardName := AKeyboardName;
  FFileName := AFileName;
end;

function TKeymanVisualKeyboard.GetOwnerKeyboard: IKeymanKeyboardInstalled;  // I2329
var
  i: Integer;
begin
  with Context do
  begin
    for i := 0 to (Keyboards as IKeymanKeyboardsInstalled).Count - 1 do
    begin
      if (Keyboards.Items[i] as IKeymanKeyboardInstalled).ID = FKeyboardName then
      begin
        Result := Keyboards.Items[i] as IKeymanKeyboardInstalled;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TKeymanVisualKeyboard.Get_Filename: WideString;
begin
  Result := FFileName;
end;

procedure TKeymanVisualKeyboard.Uninstall;
var
  ParentKeyboard: IKeymanKeyboardInstalled;
begin
  with TKPUninstallVisualKeyboard.Create(Context) do
  try
    Execute(FKeyboardName);
  finally
    Free;
  end;

  ParentKeyboard := GetOwnerKeyboard;
  if Assigned(ParentKeyboard) then
    with ParentKeyboard as IIntKeymanKeyboardInstalled do
    begin
      RegKeyboard.Load(True);
      ClearVisualKeyboard;
    end;
end;

end.
