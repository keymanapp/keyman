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
  Classes,
  Consts,
  Controls,
  Dialogs,
  Forms,
  Graphics,
  HintConsts,
  Math,
  Messages,
  WideStrUtils,
  SysUtils,
  Windows;

function ShowKeymanHint(AHint: TKeymanHint): TModalResult;   // I4242
function ShowKeymanHintQuery(AHint: TKeymanHint; AButtons: TMsgDlgButtons; ADefaultResult: TModalResult): TModalResult;

implementation

uses
  Hints,
  keymanapi_TLB,
  kmint,
  UfrmHint,
//  UfrmKeyman7Main,
  XMLRenderer;

function ShowKeymanHint(AHint: TKeymanHint): TModalResult;   // I4242
begin
  Result := ShowKeymanHintQuery(AHint, [mbOk], mrOk);   // I4242
end;

function ShowKeymanHintQuery(AHint: TKeymanHint; AButtons: TMsgDlgButtons; ADefaultResult: TModalResult): TModalResult;
var
  hwnd: THandle;
begin
  Result := ADefaultResult;

  if TfrmHint.Instance <> nil then   // I4242
  begin
    TfrmHint.Instance.BringToFront;
    Exit(mrCancel);
  end;

  if not Assigned(kmcom) then Exit;
  hwnd := kmcom.Control.LastFocusWindow;

  if IsHintEnabled(AHint) then
  begin
    if FileExists(GetXMLTemplatePath('hint.xsl')+'hint.xsl') then
    begin
      with TfrmHint.Create(nil) do   // I4242
      try
        Hint := AHint;
        Buttons := AButtons;
        DefaultResult := ADefaultResult;   // I3554 -> removed fsStayOnStop
        Result := ShowModal;
      finally
        Free;
      end;
    end;
  end;

  if not Assigned(kmcom) then Exit;  // This can happen if Keyman exits while the hint window is visible

  AttachThreadInput(GetCurrentThreadId, GetWindowThreadProcessId(hwnd, nil), TRUE);
  Windows.SetForegroundWindow(kmcom.Control.LastActiveWindow);
  Windows.SetFocus(hwnd);
  AttachThreadInput(GetCurrentThreadId, GetwindowThreadProcessId(hwnd, nil), FALSE);
end;

end.
