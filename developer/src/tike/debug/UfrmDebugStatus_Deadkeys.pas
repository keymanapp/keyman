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
  Dialogs, StdCtrls, DebugListBox, debugdeadkeys,
  UfrmDebugStatus_Child;

type
  TDebugStatus_SelectDeadkeyEvent = procedure(DeadKey: TDeadKeyInfo) of object;

  TfrmDebugStatus_DeadKeys = class(TfrmDebugStatus_Child)
    lbDeadkeys: TDebugListBox;
    procedure lbDeadkeysClick(Sender: TObject);
  private
    FOnSelectDeadkey: TDebugStatus_SelectDeadkeyEvent;
    FDeadkeys: TDebugDeadkeyInfoList;
    { Deadkey functions }
    procedure ClearDeadKeys;
    procedure SetDeadkeys(const Value: TDebugDeadkeyInfoList);

  protected
    function GetHelpTopic: string; override;

  public
    procedure UpdateDeadkeyDisplay;
    procedure DeselectDeadkeys;

    property Deadkeys: TDebugDeadkeyInfoList read FDeadkeys write SetDeadkeys;
    property OnSelectDeadkey: TDebugStatus_SelectDeadkeyEvent read FOnSelectDeadkey write FOnSelectDeadkey;
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
  if not Assigned(FOnSelectDeadkey) then
    Exit;
  if lbDeadKeys.ItemIndex < 0
    then FOnSelectDeadKey(nil)
    else FOnSelectDeadKey(lbDeadKeys.Items.Objects[lbDeadKeys.ItemIndex] as TDeadKeyInfo);
end;

procedure TfrmDebugStatus_DeadKeys.SetDeadkeys(
  const Value: TDebugDeadkeyInfoList);
begin
  FDeadkeys := Value;
  UpdateDeadkeyDisplay;
end;

procedure TfrmDebugStatus_DeadKeys.UpdateDeadkeyDisplay;
var
  dk: TDeadkeyInfo;
begin
  ClearDeadKeys;
  if Assigned(FDeadkeys) then
    for dk in FDeadkeys do
      lbDeadkeys.Items.AddObject('deadkey('+dk.Deadkey.Name+') ('+IntToStr(dk.Position)+')', dk);
end;

end.
