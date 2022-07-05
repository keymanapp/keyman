(*
  Name:             UfrmSelectKey
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
  History:          23 Aug 2006 - mcdurdin - Unicode controls
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit UfrmSelectKey;   // I4796

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtShiftState, UfrmTike;
type
  TfrmSelectKey = class(TTIKEForm)
    lblTitle: TLabel;
    cmdCancel: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FVKey: Word;
    FShiftState: TExtShiftState;
    FDistinguishLeftRight: Boolean;
  protected
    function GetHelpTopic: string; override;
  public
    property VKey: Word read FVKey;
    property ShiftState: TExtShiftState read FShiftState;
    property DistinguishLeftRight: Boolean read FDistinguishLeftRight write FDistinguishLeftRight;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics;

{$R *.DFM}

procedure TfrmSelectKey.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_SHIFT, VK_CONTROL, VK_MENU, VK_CAPITAL, VK_ESCAPE] then Exit;
  FVKey := Key;

  FShiftState := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(FShiftState, essShift);
  if FDistinguishLeftRight then
  begin
    if GetKeyState(VK_LCONTROL) < 0 then Include(FShiftState, essLCtrl);
    if GetKeyState(VK_RCONTROL) < 0 then Include(FShiftState, essRCtrl);
    if GetKeyState(VK_LMENU) < 0  then Include(FShiftState, essLAlt);
    if GetKeyState(VK_RMENU) < 0  then Include(FShiftState, essRAlt);
  end
  else
  begin
    if GetKeyState(VK_CONTROL) < 0 then Include(FShiftState, essCtrl);
    if GetKeyState(VK_MENU) < 0  then Include(FShiftState, essAlt);
  end;

  Key := 0;
  ModalResult := mrOk;
end;

function TfrmSelectKey.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_SelectKey;
end;

end.

