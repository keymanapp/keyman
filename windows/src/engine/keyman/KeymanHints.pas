(*
  Name:             KeymanHints
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    28 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - I1256 - Initial version (hint system)
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    07 Nov 2012 - mcdurdin - I3554 - V9.0 - Hint window shows content at top left of screen, outside frame of window
                    28 May 2014 - mcdurdin - I4242 - Crash when OSK closed/reopened without dismissing hint window [CrashID:keyman.exe_9.0.449.0_2C405C5D_EInvalidPointer]
*)
unit KeymanHints;  // I3306

interface

uses
  System.Classes,
  Vcl.Controls,
  HintConsts,
  Math,
  Messages,
  WideStrUtils,
  SysUtils,
  Windows;

function ShowKeymanHint(AHint: TKeymanHint): Boolean;   // I4242

implementation

uses
  Hints,
  keymanapi_TLB,
  KeymanDesktopShell,
  kmint;
//  UfrmKeyman7Main,
//  XMLRenderer;

var
  InHint: Boolean = False;

function ShowKeymanHint(AHint: TKeymanHint): Boolean;
var
//  hwnd: THandle;
  ec: Cardinal;
begin
  if InHint then
    Exit(False);

  Result := True;

  InHint := True;
  try
    if not Assigned(kmcom) then Exit;
  //  hwnd := kmcom.Control.LastFocusWindow;

    if not IsHintEnabled(AHint) then Exit;

    if KeymanHintData[AHint].IsQuestion then
    begin
      //TODO: Make this background with a post message to main to close
      TKeymanDesktopShell.WaitForKeymanConfiguration('-showhint '+GetHintName(AHint), ec);
      if ec = 1 then
        Result := False
    end
    else
      TKeymanDesktopShell.RunKeymanConfiguration('-showhint '+GetHintName(AHint));
  finally
    InHint := False;
  end;
end;

end.
