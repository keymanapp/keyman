(*
  Name:             CharMapDropTool_TntCustomEdit
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit CharMapDropTool_TntCustomEdit;  // I3306   // I4796

interface

uses
  Windows,
  CharacterDragObject,
  CharMapDropTool,
  CharMapInsertMode,
  Controls,
  Messages,
  StdCtrls;

type
  TCharMapDropToolControl_TntCustomEdit = class(TCharMapDropToolControl)
  private
    function Edit: TCustomEdit;
    function TntCustomEdit: TCustomEdit;
    function TntCustomMemo: TCustomMemo;
  public
    class function Handles: TControlClass; override;
    procedure Drag(AObject: TCharacterDragObject; X, Y: Integer; var Accept: Boolean); override;
    procedure Drop(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; X, Y: Integer); override;
  end;

implementation

procedure TCharMapDropToolControl_TntCustomEdit.Drag(AObject: TCharacterDragObject; X, Y: Integer; var Accept: Boolean);
begin
  Accept := (X > 0) and (Y > 0) and ((Control is TCustomEdit) or (Control is TCustomMemo));
  if Accept then
  begin
    Edit.SelStart := SendMessage(Edit.Handle, EM_CHARFROMPOS, 0, MAKELONG(X, Y))
  end;
end;

procedure TCharMapDropToolControl_TntCustomEdit.Drop(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; X, Y: Integer);
var
  col, row: Integer;
  FAutoSelect: Boolean;
begin
  col := Edit.SelStart - SendMessage(Edit.Handle, EM_LINEINDEX, -1, 0);

  if Control is TCustomMemo then
  begin
    row := SendMessage(TntCustomMemo.Handle, EM_LINEFROMCHAR, -1, 0);
    TntCustomMemo.SelText := CharMapDropTool_Insert(AInsertType, AObject, TntCustomMemo.Lines[row], col);
  end
  else if Control is TCustomEdit then
  begin
    TntCustomEdit.SelText := CharMapDropTool_Insert(AInsertType, AObject, TntCustomEdit.Text, col)
  end
  else
    Exit;

  FAutoSelect := TEdit(Edit).AutoSelect;
  TEdit(Edit).AutoSelect := False;
  Edit.SetFocus;
  TEdit(Edit).AutoSelect := FAutoSelect;
end;

class function TCharMapDropToolControl_TntCustomEdit.Handles: TControlClass;
begin
  Result := TCustomEdit;
end;

function TCharMapDropToolControl_TntCustomEdit.Edit: TCustomEdit;
begin
  Result := Control as TCustomEdit;
end;

function TCharMapDropToolControl_TntCustomEdit.TntCustomEdit: TCustomEdit;
begin
  Result := Control as TCustomEdit;
end;

function TCharMapDropToolControl_TntCustomEdit.TntCustomMemo: TCustomMemo;
begin
  Result := Control as TCustomMemo;
end;

initialization
  TCharMapDropToolControl_TntCustomEdit.Register;
end.
