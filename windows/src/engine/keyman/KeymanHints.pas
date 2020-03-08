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
  Vcl.Forms,
  HintConsts,
  Math,
  Messages,
  WideStrUtils,
  SysUtils,
  Windows;

function ShowKeymanHint(AHint: TKeymanHint): Boolean;   // I4242
procedure ResetHintState;

implementation

uses
  Hints,
  keymanapi_TLB,
  KeymanDesktopShell,
  kmint;

var
  InHint: Boolean = False;

procedure ResetHintState;
begin
  InHint := False;
end;

function ShowKeymanHint(AHint: TKeymanHint): Boolean;
begin
  if InHint then
    Exit(True);

  Result := True;

  if not Assigned(kmcom) then Exit;
  if not IsHintEnabled(AHint) then Exit;

  InHint := True; // will be reset by the hint response

  // We will receive the response and process asynchronously via
  // wm_keyman_control message KMC_HINTRESPONSE
  if not TKeymanDesktopShell.RunKeymanConfiguration('-showhint '+GetHintName(AHint)+' -parentwindow '+IntToStr(Application.MainForm.Handle)) then
    InHint := False;

  Result := not KeymanHintData[AHint].IsQuestion;
end;

end.
