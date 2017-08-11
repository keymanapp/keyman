(*
  Name:             UfrmDebugStatus_CallStack
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
*)
unit UfrmDebugStatus_CallStack;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DebugListBox, debugging, UfrmDebugStatus_Child;

type
  TfrmDebugStatus_CallStack = class(TfrmDebugStatus_Child)
    lbCallStack: TDebugListBox;
    procedure lbCallStackDblClick(Sender: TObject);
    procedure lbCallStackKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

  private
    function SetEditorCursorLine(ALine: Integer): Boolean;
    { Call stack functions }

  public
    { Public declarations }
    procedure CallStackPush(rule: TDebugEventRuleData);
    procedure CallStackPop;
    procedure CallStackClear;
  end;

implementation

uses
  UKeyBitmap;

{$R *.dfm}

{ TfrmDebugStatus_CallStack }

procedure TfrmDebugStatus_CallStack.CallStackClear;
begin
  lbCallStack.Clear;
end;

procedure TfrmDebugStatus_CallStack.CallStackPop;
begin
  if lbCallStack.Items.Count = 0 then Exit;
  lbCallStack.Items.Delete(lbCallStack.Items.Count - 1);
end;

procedure TfrmDebugStatus_CallStack.CallStackPush(rule: TDebugEventRuleData);
var
  s: string;
begin
  case rule.ItemType of
    QID_BEGIN_UNICODE:
      lbCallStack.Items.AddObject('begin Unicode', rule);
    QID_BEGIN_ANSI:
      lbCallStack.Items.AddObject('begin ANSI', rule);
    QID_GROUP_ENTER:
      if rule.Group.dpName <> '' then
      begin
        s := 'group('+rule.Group.dpName+')';
        if rule.Group.fUsingKeys then s := s + ' using keys';
        lbCallStack.Items.AddObject(s, rule);
      end
      else
        lbCallStack.Items.AddObject('Unknown group', rule);
    QID_RULE_ENTER:
      lbCallStack.Items.AddObject('rule at line '+IntToStr(rule.line), rule);
    QID_MATCH_ENTER:
      lbCallStack.Items.AddObject('match rule', rule);
    QID_NOMATCH_ENTER:
      lbCallStack.Items.AddObject('nomatch rule', rule);
  end;
end;

procedure TfrmDebugStatus_CallStack.lbCallStackDblClick(Sender: TObject);
var
  rule: TDebugEventRuleData;
begin
  if lbCallStack.ItemIndex < 0 then Exit;
  if not Assigned(lbCallStack.Items.Objects[lbCallStack.ItemIndex]) then Exit;
  rule := lbCallStack.Items.Objects[lbCallStack.ItemIndex] as TDebugEventRuleData;
  if not SetEditorCursorLine(rule.Line)
    then ShowMessage('TIKE could not find the line for this rule.')
    else EditorMemo.SetFocus;
end;

procedure TfrmDebugStatus_CallStack.lbCallStackKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    lbCallStackDblClick(lbCallStack);
    Key := 0;
  end
  else
    Exit;
end;

function TfrmDebugStatus_CallStack.SetEditorCursorLine(ALine: Integer): Boolean;
begin
  Result := False;
  Dec(ALine);
  if (ALine >= EditorMemo.LineCount) or (ALine < 0) then Exit;
  EditorMemo.SelLine := ALine;
  EditorMemo.SelCol := 0;
  EditorMemo.SelLength := Length(EditorMemo.LinesArray[ALine]);
  EditorMemo.ScrollInView;
  Result := True;
end;


end.
