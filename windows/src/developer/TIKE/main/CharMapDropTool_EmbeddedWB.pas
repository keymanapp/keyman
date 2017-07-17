(*
  Name:             CharMapDropTool_TntCustomEdit
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    19 Mar 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    06 Mar 2014 - mcdurdin - I4110 - Double-click on Character Map when in Touch Layout Editor crashes TIKE
                    06 Mar 2014 - mcdurdin - I4111 - Double-click in char map when no input element focused in touch layout editor crashes TIKE
                    19 Mar 2014 - mcdurdin - I4144 - V9.0 - When character is dropped or double-clicked to insert into html text field, no modified event is triggered
*)
unit CharMapDropTool_EmbeddedWB;  // I3306

interface

uses
  Windows,
  CharacterDragObject,
  CharMapDropTool,
  CharMapInsertMode,
  Controls,
  Messages,
  EmbeddedWB,
  StdCtrls;

type
  TCharMapDropToolControl_EmbeddedWB = class(TCharMapDropToolControl)
  private
    function EmbeddedWB: TEmbeddedWB;
  public
    class function Handles: TControlClass; override;
    procedure Drag(AObject: TCharacterDragObject; X, Y: Integer; var Accept: Boolean); override;
    procedure Drop(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; X, Y: Integer); override;
  end;

implementation

uses
  System.SysUtils,
  MSHTML_EWB;

procedure TCharMapDropToolControl_EmbeddedWB.Drag(AObject: TCharacterDragObject; X, Y: Integer; var Accept: Boolean);
var
  elem: IHTMLElement;
begin
  Accept := (X > 0) and (Y > 0) and (Control is TEmbeddedWB);
  if Accept then
  begin
    elem := EmbeddedWB.Doc2.elementFromPoint(X, Y);
    if True then

    Accept :=
      Assigned(elem) and elem.isTextEdit;
///      (((LowerCase(elem.tagName) = 'input') and (elem.getAttribute('type') = 'text')) or
   //   (LowerCase(elem.tagName) = 'textarea'));
    // SelStart := SendMessage(Edit.Handle, EM_CHARFROMPOS, 0, MAKELONG(X, Y))
  end;
end;

procedure TCharMapDropToolControl_EmbeddedWB.Drop(AInsertType: TCharMapInsertMode; AObject: TCharacterDragObject; X, Y: Integer);
var
  elem: IHTMLElement;
  inputElem: IHTMLInputElement;
  textRange: IHTMLTxtRange;
begin
  if (X = -1) and (Y = -1) then   // I4110
  begin
    elem := EmbeddedWB.Doc2.activeElement;
  end
  else
    elem := EmbeddedWB.Doc2.elementFromPoint(X, Y);

  if not Assigned(elem) then   // I4111
    Exit;

  if not Supports(elem, IHTMLElement2) or not Supports(elem, IHTMLInputElement) then   // I4111
    Exit;

  EmbeddedWB.SetFocus;

  (elem as IHTMLElement2).focus;

  inputElem := (elem as IHTMLInputElement);
  textRange := inputElem.createTextRange();
//  if textRange. then

//  textRange.moveToPoint(X, Y);
  textRange.text := textRange.text + CharMapDropTool_Insert(AInsertType, AObject, '', 0);
  (elem as IHTMLElement2).blur;   // I4144
  (elem as IHTMLElement2).focus;   // I4144
  textRange.collapse(false);
  textRange.select;
end;

class function TCharMapDropToolControl_EmbeddedWB.Handles: TControlClass;
begin
  Result := TEmbeddedWB;
end;

function TCharMapDropToolControl_EmbeddedWB.EmbeddedWB: TEmbeddedWB;
begin
  Result := Control as TEmbeddedWB;
end;

initialization
  TCharMapDropToolControl_EmbeddedWB.Register;
end.
