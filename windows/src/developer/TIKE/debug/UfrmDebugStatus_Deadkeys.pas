(*
  Name:             UfrmDebugStatus_Deadkeys
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    16 Jan 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
                    16 Jan 2009 - mcdurdin - I1699 - Fix crash in debugger when debugging deadkeys
*)
unit UfrmDebugStatus_Deadkeys;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DebugListBox, debugging, debugdeadkeys,
  UfrmDebugStatus_Child;

type
  TfrmDebugStatus_DeadKeys = class(TfrmDebugStatus_Child)
    lbDeadkeys: TDebugListBox;
    procedure lbDeadkeysClick(Sender: TObject);
  private
    { Deadkey functions }
    procedure ClearDeadKeys;

  protected
    function GetHelpTopic: string; override;

  public
    procedure UpdateDeadkeyDisplay(deadkeys: TList);
    procedure DeselectDeadkeys;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  UfrmDebugStatus;

{$R *.dfm}

procedure TfrmDebugStatus_DeadKeys.ClearDeadKeys;
begin
  lbDeadkeys.Clear;
end;

procedure TfrmDebugStatus_DeadKeys.DeselectDeadkeys;
begin
  lbDeadkeys.ItemIndex := -1;
  lbDeadkeysClick(lbDeadkeys);
end;

function TfrmDebugStatus_DeadKeys.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_DebugStatus_DeadKeys;
end;

procedure TfrmDebugStatus_DeadKeys.lbDeadkeysClick(Sender: TObject);
begin
  if lbDeadKeys.ItemIndex < 0
    then DebugForm.SelectDeadKey(nil)
    else DebugForm.SelectDeadKey(lbDeadKeys.Items.Objects[lbDeadKeys.ItemIndex] as TDeadKeyInfo);
end;

procedure TfrmDebugStatus_DeadKeys.UpdateDeadkeyDisplay(deadkeys: TList);
var
  i: Integer;
begin
  ClearDeadKeys;
  for i := 0 to deadkeys.Count - 1 do
    with TDeadKeyInfo(deadkeys[i]) do
      lbDeadkeys.Items.AddObject('deadkey('+Deadkey.Name+') ('+IntToStr(Position)+')', TDeadKeyInfo(deadkeys[i]));
end;

end.
