(*
  Name:             HotkeysXMLRenderer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    4 Jan 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    04 Jan 2007 - mcdurdin - Encode entities in xml render
*)
unit HotkeysXMLRenderer;

interface

uses
  XMLRenderer,
  Windows;

type
  THotkeysXMLRenderer = class(TXMLRenderer)
  protected
    function XMLData: WideString; override;
  end;

implementation

uses
  InterfaceHotkeys,
  SysUtils,
  KeyNames,
  utilxml;

{ THotkeysXMLRenderer }

const
    HK_ALT   = $00010000;
    HK_CTRL	 = $00020000;
    HK_SHIFT = $00040000;

function HotkeyAsString(hotkey: DWord): string;
var
  s: string;
begin
  s := '';
  if LoByte(hotkey) <> 0 then
  begin
    if (hotkey and HK_SHIFT) = HK_SHIFT then s := s + 'Shift+';
    if (hotkey and HK_CTRL) = HK_CTRL   then s := s + 'Ctrl+';
    if (hotkey and HK_ALT) = HK_ALT     then s := s + 'Alt+';
    s := s + SKeyNames[LoByte(hotkey)];
  end
  else if (hotkey = HK_CTRL or HK_SHIFT) then s := 'Ctrl+Shift'
  else if (hotkey = HK_ALT or HK_SHIFT) then s := 'Left Alt+Shift';

  Result := s;
end;

function THotkeysXMLRenderer.XMLData: WideString;
var
  FValue, i, j: Integer;
begin
  Result := '<Hotkeys>';
  for i := 0 to High(CInterfaceHotkeys) do
  begin
    FValue := 0;
    for j := 0 to kmcom.Hotkeys.Count - 1 do
      if kmcom.Hotkeys[j].Target = CInterfaceHotkeys[i] then
      begin
        FValue := kmcom.Hotkeys[j].RawValue;
        Break;
      end;
    Result := Result + '<Hotkey><index>'+IntToStr(i)+'</index><Target>'+IntToStr(CInterfaceHotkeys[i])+'</Target><Value>'+XMLEncode(HotkeyAsString(FValue))+'</Value></Hotkey>';
  end;
  Result := Result + '</Hotkeys>';
end;

end.

