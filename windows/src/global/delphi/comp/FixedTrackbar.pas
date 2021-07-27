(*
  Name:             FixedTrackbar
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Base on TTrackBar instead of TSp-TBXTrackbar
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit FixedTrackbar;  // I3306

interface

uses
  Windows, Classes, Messages, Controls, ComCtrls, CommCtrl, Grids;

type
  TTntFixedDrawGrid = class(TDrawGrid)
  private
    HackedMousedown: Boolean;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Keyman', [TTntFixedDrawGrid]);
end;

{
  Stop erase of cells to eliminate flicker on the character map
}

procedure TTntFixedDrawGrid.WMEraseBkgnd(var Message: TMessage);
begin
  ; // do nothing
end;

{
  Stop scroll on mousedown on bottom row of grid when bottom row
  is a partial cell: have to block both initial scroll and timer-
  based scroll.

  This code is pretty dependent on the implementation in Vcl.Grids.pas,
  so it should be checked if we upgrade to new version of Delphi.

  Tested on VER320 (10.2)
  Tested on VER330 (10.3) - 29 Oct 2019 - mcdurdin
}

{$IFNDEF VER340}
{$MESSAGE WARN 'Not yet checked against Delphi 10.4'}
{$IFNDEF VER330}
{$IFNDEF VER320}
{$MESSAGE ERROR 'Check that this fix is still applicable for a new version of Delphi. Checked against Delphi 10.2, 10.3' }
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure TTntFixedDrawGrid.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Call the inherited event, blocking the default MoveCurrent
  // behaviour that scrolls the cell into view
  HackedMouseDown := True;
  try
    inherited;
  finally
    HackedMouseDown := False;
  end;

  // Cancel scrolling timer started by the mousedown event for selecting
  if FGridState = gsSelecting then
    KillTimer(Handle, 1);
end;


function TTntFixedDrawGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := inherited;
  if Result and HackedMousedown then
  begin
    // MoveColRow calls MoveCurrent, which
    // calls SelectCell. If SelectCell returns False, then
    // movement is blocked. But we fake it by re-calling with Show=False
    // to get the behaviour we want
    HackedMouseDown := False;
    try
      MoveColRow(ACol, ARow, True, False);
    finally
      HackedMouseDown := True;
    end;
    Result := False;
  end
end;

end.
