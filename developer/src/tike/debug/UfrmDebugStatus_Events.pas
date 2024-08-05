{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Displays a list of debug events and actions for current keystroke. This is
 * intended for internal use only and would not normally be visible for end
 * users of Keyman Developer.
}
unit UfrmDebugStatus_Events;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DebugListBox, UfrmDebugStatus_Child,
  Keyman.System.Debug.DebugEvent;

type
  TfrmDebugStatus_Events = class(TfrmDebugStatus_Child)
    lbCallStack: TDebugListBox;

  private
    FIndent: Integer;
    procedure AddRuleEvent(rule: TDebugEventRuleData);
    procedure AddActionEvent(action: TDebugEventActionData);
    { Call stack functions }

  protected
    function GetHelpTopic: string; override;

  public
    procedure SetEvents(events: TDebugEventList);
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,
  Keyman.System.KeymanCore,
  Keyman.System.KeymanCoreDebug,

  UKeyBitmap;

{$R *.dfm}

{ TfrmDebugStatus_CallStack }

procedure TfrmDebugStatus_Events.SetEvents(events: TDebugEventList);
var
  event: TDebugEvent;
begin
  lbCallStack.Clear;
  for event in events do
  begin
    case event.EventType of
      etAction: AddActionEvent(event.Action);
      etRuleMatch: AddRuleEvent(event.Rule);
    end;
  end;
end;

procedure TfrmDebugStatus_Events.AddRuleEvent(rule: TDebugEventRuleData);
  procedure AddItem(IsEntry: Boolean; const message: string; data: TObject);
  begin
    if IsEntry then
    begin
      lbCallStack.Items.AddObject(StringOfChar(' ', FIndent)+'Enter '+message, data);
      Inc(FIndent);
    end
    else
    begin
      Dec(FIndent);
      lbCallStack.Items.AddObject(StringOfChar(' ', FIndent)+'Exit '+message, data);
    end;
  end;

var
  s: string;
begin
  case rule.ItemType of
    KM_CORE_DEBUG_BEGIN,
    KM_CORE_DEBUG_END:
        AddItem(rule.ItemType = KM_CORE_DEBUG_BEGIN, 'begin Unicode', rule);
    KM_CORE_DEBUG_GROUP_ENTER,
    KM_CORE_DEBUG_GROUP_EXIT:
      begin
        if rule.Group.dpName <> '' then
        begin
          s := 'group('+rule.Group.dpName+')';
          if rule.Group.fUsingKeys then s := s + ' using keys';
        end
        else
          s := 'Unknown group';
        AddItem(rule.ItemType = KM_CORE_DEBUG_GROUP_ENTER, s, rule);
      end;
    KM_CORE_DEBUG_RULE_ENTER,
    KM_CORE_DEBUG_RULE_EXIT:
      AddItem(rule.ItemType = KM_CORE_DEBUG_RULE_ENTER, 'rule at line '+IntToStr(rule.line), rule);
    KM_CORE_DEBUG_MATCH_ENTER,
    KM_CORE_DEBUG_MATCH_EXIT:
      AddItem(rule.ItemType = KM_CORE_DEBUG_MATCH_ENTER, 'match rule', rule);
    KM_CORE_DEBUG_NOMATCH_ENTER,
    KM_CORE_DEBUG_NOMATCH_EXIT:
      AddItem(rule.ItemType = KM_CORE_DEBUG_NOMATCH_ENTER, 'nomatch rule', rule);
    KM_CORE_DEBUG_SET_OPTION:
      AddItem(True, 'set option', rule);
    else
      lbCallStack.Items.AddObject('???', rule);
  end;
end;

procedure TfrmDebugStatus_Events.AddActionEvent(action: TDebugEventActionData);
  procedure AddItem(const message: string; data: TObject);
  begin
    lbCallStack.Items.AddObject(StringOfChar(' ', FIndent)+message, data);
  end;
begin
  case action.ActionType of
    KM_CORE_IT_EMIT_KEYSTROKE: AddItem('emit_keystroke', action);
    KM_CORE_IT_CHAR:        AddItem('char', action);
    KM_CORE_IT_MARKER:     AddItem('marker', action);
    KM_CORE_IT_ALERT:        AddItem('alert', action);
    KM_CORE_IT_BACK:        AddItem('back', action);
    KM_CORE_IT_PERSIST_OPT: AddItem('persist_opt', action);
    KM_CORE_IT_INVALIDATE_CONTEXT:  AddItem('invalidate_context', action);
    KM_CORE_IT_CAPSLOCK:    AddItem('capslock', action);
    else             AddItem('Unknown action ???', action);
  end;
end;

function TfrmDebugStatus_Events.GetHelpTopic: string;
begin
//  Result := SHelpTopic_Context_DebugStatus_Event;
end;

end.
