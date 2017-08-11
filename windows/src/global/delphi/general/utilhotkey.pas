(*
  Name:             utilhotkey
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    16 Jan 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Refactor util functions into multiple units
                    16 Jan 2009 - mcdurdin - I1630 - Hotkey not displayed in menu
*)
unit utilhotkey;

interface

uses
  Classes, SysUtils, keymanapi_TLB, Menus;

function HotkeyToShortcut(hotkey: IKeymanHotkey): TShortCut;
function ShortcutToHotkey(Shortcut: TShortCut): Integer;
function ShortcutToTextEx(Shortcut: TShortCut): string;

implementation

function HotkeyToShortcut(hotkey: IKeymanHotkey): TShortCut;
var
  Shift: TShiftState;
begin
  Shift := [];
  if (hotkey.Modifiers and HK_ALT) = HK_ALT then Include(Shift, ssAlt);
  if (hotkey.Modifiers and HK_CTRL) = HK_CTRL then Include(Shift, ssCtrl);
  if (hotkey.Modifiers and HK_SHIFT) = HK_SHIFT then Include(Shift, ssShift);
  Result := ShortCut(Word(hotkey.VirtualKey), Shift);
end;

function ShortcutToHotkey(Shortcut: TShortCut): Integer;
var
  Shift: TShiftState;
  Key: Word;
begin
  Result := 0;
  ShortCutToKey(Shortcut, Key, Shift);
  if ssShift in Shift then Result := Result or HK_SHIFT;
  if ssCtrl in Shift then Result := Result or HK_CTRL;
  if ssAlt in Shift then Result := Result or HK_ALT;
  Result := Result or Key;
end;

function ShortcutToTextEx(Shortcut: TShortCut): string;
begin
  if Lo(Shortcut) = 19 then
    Result := 'Pause' // I1630
  else
    Result := ShortcutToText(Shortcut); 
end;

end.
