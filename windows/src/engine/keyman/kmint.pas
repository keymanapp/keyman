(*
  Name:             kmint
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Remove Refresh-Keyman function
*)
unit kmint;

interface

uses
  custinterfaces,
  KeymanEngineControl,
  keymanapi_TLB;

var
  kmcom: IKeyman = nil;
  FProductFileName: string = '';
  FRunFolder: string = '';

function EncodingsAsString(encodings: KeymanKeyboardEncodings): WideString;
function HotkeyAsString(hotkey: IKeymanHotkey): WideString;
function KeymanCustomisation: IKeymanCustomisation;
function KeymanEngineControl: IKeymanEngineControl;

const
  KEYMAN_LAYOUT_CUSTOM = $000005FE;

implementation

uses
  Winapi.Windows,

  KeyNames,
  MessageIdentifierConsts,
  MessageIdentifiers;

const
  KR_REQUEST_REFRESH = 0;  // Send this to any window, which will make Keyman post a KR_REFRESH to all top-level windows
  KR_REFRESH = 2;          // Finally this message get sent to all the other top-level windows in the system.
  KR_SETTINGS_CHANGED = 3;

function KeymanCustomisation: IKeymanCustomisation;
begin
  Result := (kmcom.Control as IKeymanCustomisationAccess).KeymanCustomisation;
end;

function KeymanEngineControl: IKeymanEngineControl;
begin
  Result := kmcom.Control as IKeymanEngineControl;
end;

function EncodingsAsString(encodings: KeymanKeyboardEncodings): WideString;
begin
  Result := '';
  if (encodings and keANSI) = keANSI then Result := MsgFromId(skANSIEncoding);
  if (encodings and keUnicode) = keUnicode then
  begin
    if Result <> '' then Result := Result + ', ';
    Result := Result + MsgFromId(skUnicodeEncoding);
  end;
end;

function HotkeyAsString(hotkey: IKeymanHotkey): WideString;
var
  s: string;
begin
  s := '';
  if LoByte(hotkey.VirtualKey) <> 0 then
  begin
    if (hotkey.Modifiers and HK_SHIFT) = HK_SHIFT then s := s + 'Shift + ';
    if (hotkey.Modifiers and HK_CTRL) = HK_CTRL   then s := s + 'Ctrl + ';
    if (hotkey.Modifiers and HK_ALT) = HK_ALT     then s := s + 'Alt + ';
    s := s + SKeyNames[LoByte(hotkey.VirtualKey)];
  end;

  Result := s;
end;

end.

