(*
  Name:             keymanhotkey
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    1 Aug 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
*)
unit keymanhotkey;

interface

uses
  System.Win.ComObj,
  System.Classes,
  Winapi.ActiveX,
  internalinterfaces,
  keymanapi_TLB,
  KeymanContext,
  keymanautoobject;

type
  TKeymanHotkey = class(TKeymanAutoObject, IKeymanHotkey)
  private
    FValue: Integer;
    FTarget: Integer;

  protected
    { IKeymanHotkey }
    function Get_Modifiers: TOleEnum; safecall;
    procedure Set_Modifiers(Value: TOleEnum); safecall;
    function Get_RawValue: Integer; safecall;
    procedure Set_RawValue(Value: Integer); safecall;
    function Get_Target: TOleEnum; safecall;
    procedure Set_Target(Value: TOleEnum); safecall;
    function Get_VirtualKey: Integer; safecall;
    procedure Set_VirtualKey(Value: Integer); safecall;
    procedure Clear; safecall;
    function IsEmpty: WordBool; safecall;
  public
    constructor Create(AContext: TKeymanContext; AHotkeyValue: Integer; ATarget: KeymanHotkeyTarget);
  end;

implementation

uses Windows, ComServ, ErrorControlledRegistry, RegistryKeys, SysUtils, keymanerrorcodes, Variants;

constructor TKeymanHotkey.Create(AContext: TKeymanContext;
  AHotkeyValue: Integer; ATarget: KeymanHotkeyTarget);
begin
  inherited Create(AContext, IKeymanHotkey);
  FValue := AHotkeyValue;
  FTarget := ATarget;
end;

procedure TKeymanHotkey.Clear;
begin
  FValue := 0;
end;

function TKeymanHotkey.Get_Modifiers: TOleEnum;
begin
  Result := FValue and (HK_ALT or HK_CTRL or HK_SHIFT);
end;

function TKeymanHotkey.Get_RawValue: Integer;
begin
  Result := FValue;
end;

function TKeymanHotkey.Get_Target: TOleEnum;
begin
  Result := FTarget;
end;

function TKeymanHotkey.Get_VirtualKey: Integer;
begin
  Result := FValue and $FF;
end;

function TKeymanHotkey.IsEmpty: WordBool;
begin
  Result := FValue = 0;
end;

procedure TKeymanHotkey.Set_Modifiers(Value: TOleEnum);
begin
  FValue := (FValue and $FF) or Integer(Value);
end;

procedure TKeymanHotkey.Set_RawValue(Value: Integer);
begin
  FValue := Value;
end;

procedure TKeymanHotkey.Set_Target(Value: TOleEnum);
begin
  FTarget := Integer(Value);
end;

procedure TKeymanHotkey.Set_VirtualKey(Value: Integer);
begin
  FValue := (FValue and (HK_ALT or HK_CTRL or HK_SHIFT)) or Value;
end;

end.
