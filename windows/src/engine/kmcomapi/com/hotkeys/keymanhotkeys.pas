(*
  Name:             keymanhotkeys
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Fix IKeymanHotkeys reference
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit keymanhotkeys;

interface

uses
  System.Win.ComObj,
  Winapi.ActiveX,
  keymanapi_TLB,
  keymanautoobject,
  internalinterfaces,
  keymancontext,
  keymanhotkey;

type
  TKeymanHotkeyList = TAutoObjectList;

  TKeymanHotkeys = class(TKeymanAutoCollectionObject, IIntKeymanHotkeys, IKeymanHotkeys)
  private
    FHotkeys: TKeymanHotkeyList;
  protected
    procedure DoRefresh; override;

    { IKeymanHotkeys }
    function Get_Items(Index: Integer): IKeymanHotkey; safecall;
    procedure Apply; safecall;
    procedure Reset; safecall;
  public
    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  Classes,
  ComServ,
  keymanerrorcodes,
  ErrorControlledRegistry, 
  RegistryKeys,
  SysUtils,
  Variants;

function TKeymanHotkeys.Get_Items(Index: Integer): IKeymanHotkey;
begin
  if (Index < Get_Count) and (Index >= 0) then
    Result := FHotkeys[Index] as IKeymanHotkey
  else
    ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([IntToStr(Index)]));
end;

procedure TKeymanHotkeys.Apply;
var
  i: Integer;
  si: string;
begin
  { save the hotkeys }
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey(SRegKey_KeymanHotkeys_CU,True) then
    begin
      for i := 0 to FHotkeys.Count - 1 do
      begin
        si := IntToStr(i);

        with FHotkeys[i] as IKeymanHotkey do
          if IsEmpty then
          begin
            if ValueExists(si) then
              DeleteValue(si);
          end
          else
            WriteInteger(si, RawValue);
      end;
    end;
  finally
    Free;
  end;
  Context.Control.AutoApplyKeyman;
end;

constructor TKeymanHotkeys.Create(AContext: TKeymanContext);
begin
  FHotkeys := TKeymanHotkeyList.Create;
  inherited Create(AContext, IKeymanHotkeys, FHotkeys);
  Refresh;
end;

destructor TKeymanHotkeys.Destroy;
begin
  FHotkeys.Free;
  inherited Destroy;
end;

procedure TKeymanHotkeys.DoRefresh;
var
  Value, i: Integer;
  HasRegItems: Boolean;
begin
  inherited;

  { Load hotkey }
  FHotkeys.Clear;

  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    HasRegItems := OpenKeyReadOnly(SRegKey_KeymanHotkeys_CU);
    for i := kh__Low to kh__High do
    begin
      if HasRegItems and ValueExists(IntToStr(i))
        then Value := ReadInteger(IntToStr(i))
        else Value := 0;

      FHotkeys.Add(TKeymanHotkey.Create(Context, Value, i));
    end;
  finally
    Free;
  end;
end;

procedure TKeymanHotkeys.Reset;
var
  i: Integer;
begin
  for i := 0 to FHotkeys.Count - 1 do
    (FHotkeys[i] as IKeymanHotkey).Clear;
end;

end.

