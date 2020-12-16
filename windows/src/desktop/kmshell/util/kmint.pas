(*
  Name:             kmint
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Remove old Keyman 6 integration
                    01 Aug 2006 - mcdurdin - Look for pxx file in startup folder to determine product to start
                    05 Nov 2007 - mcdurdin - I937, I1128 - Repair COM object if it fails at startup
                    28 Jun 2011 - mcdurdin - I2960 - Repair does repair on 7.1 not 8.0
                    28 Jun 2011 - mcdurdin - I2923 - When Desktop 7.x installed over 8.0, crash occurs, needs repair
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit kmint;  // I3306

interface

uses
  custinterfaces,
  System.UITypes,
  KeymanEngineControl,
  keymanapi_TLB;

var
  kmcom: IKeyman = nil;

function EncodingsAsString(encodings: KeymanKeyboardEncodings): WideString;
function HotkeyAsString(hotkey: IKeymanHotkey): WideString;
function KeymanCustomisation: IKeymanCustomisation;
function KeymanEngineControl: IKeymanEngineControl;

function LoadKMCOM: Boolean;

const
  KEYMAN_LAYOUT_CUSTOM = $000005FE;
  KR_REFRESH = 2;
  KR_SETTINGS_CHANGED = 3;

const
  KMC_REFRESH = 4;

implementation

uses ActiveX, ComObj, Forms, Windows, jwamsi,
  Dialogs, Controls, KeymanVersion,
  MessageIdentifierConsts, MessageIdentifiers, KeyNames, Sysutils;

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

function ReinstallKMCOM(msg: string): Boolean;
var
  err: DWord;
  productcode: array[0..39] of char;
begin
  Result := False;
  if MessageDlg('Keyman Engine appears to be damaged.  Do you want to try and repair Keyman Engine now?'#13#10#13#10'The error message was: '+msg, mtConfirmation, mbOkCancel, 0) = mrCancel then
    Exit;

  err := MsiGetProductCode(SKeymanInstallerComponentCode, productcode);
  if err <> ERROR_SUCCESS then
  begin
    ShowMessage('Keyman Engine could not be repaired.  Please reinstall this product.'#13#10#13#10+
      'Error message was: MsiGetProductCode: '+SysErrorMessage(err));
    Exit;
  end;

  err := MsiInstallMissingComponent(productcode, SKeymanInstallerComponentCode, INSTALLSTATE_LOCAL);
  if err <> ERROR_SUCCESS then
  begin
    ShowMessage('Keyman Engine could not be repaired.  Please reinstall this product.'#13#10#13#10+
      'Error message was: MsiInstallMissingComponent: '+SysErrorMessage(err));
    Exit;
  end;

  err := MsiReinstallProduct(productcode, REINSTALLMODE_MACHINEDATA);
  if err <> ERROR_SUCCESS then
  begin
    ShowMessage('Keyman Engine could not be repaired.  Please reinstall this product.'#13#10#13#10+
      'Error message was: MsiReinstallProduct: '+SysErrorMessage(err));
    Exit;
  end;

  Result := True;
end;

function TestV8Interfaces(var msg: string): Boolean;  // I2923, I2960
begin
  try
    kmcom := CoKeyman.Create;
  except
    on E:EOleSysError do
    begin
      kmcom := nil;
      msg := E.Message;
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

procedure CoFreeUnusedLibrariesEx(dwUnloadDelay, dwReserved: DWORD); stdcall; external 'ole32.dll';
 
function LoadKMCOM: Boolean;
var
  msg: string;
begin
  if not TestV8Interfaces(msg) then  // I2923, I2960
  begin
    //CoFreeAllLibraries;
    CoFreeUnusedLibrariesEx(0, 0);
    CoUninitialize;
    Result := ReinstallKMCOM(msg);
    CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    if Result then kmcom := CoKeyman.Create;
  end
  else
    Result := True;
end;

function KeymanCustomisation: IKeymanCustomisation;
begin
  Result := (kmcom.Control as IKeymanCustomisationAccess).KeymanCustomisation;
end;

function KeymanEngineControl: IKeymanEngineControl;
begin
  Result := kmcom.Control as IKeymanEngineControl;
end;

end.

