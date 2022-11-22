(*
  Name:             UfrmInternalDebug
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
unit UfrmInternalDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, UfrmTike;

type
  TfrmInternalDebug = class(TTIKEForm)
    PageControl1: TPageControl;
    TabSheet4: TTabSheet;
    lbDEBUG: TListBox;
  private
  public
    procedure AddDEBUG(s: string);
  end;

var
  frmInternalDebug: TfrmInternalDebug;

implementation

{$R *.dfm}

{
  if (di.Flags and QID_FLAG_NOMATCH) = QID_FLAG_NOMATCH then s := ' NoMatch in Group' else s := '';
  s := Format('RULEMATCH %s%s', [qid[di.ItemType],  s]);
  //Context:%s Output:%s, HexString(di.Context), HexString(di.Output),
  if Assigned(di.Rule) then
  begin
    s := s + Format(' Line:%d', [di.Rule.Line]);
  end;

  AddDEBUG(s);
}

procedure TfrmInternalDebug.AddDEBUG(s: string);
begin
//  lbDEBUG.Clear;
  if panDebug.Visible then
  begin
    lbDEBUG.Items.Add(s);
    if (lbDEBUG.Items.Count - lbDEBUG.TopIndex) * lbDEBUG.ItemHeight >= lbDEBUG.ClientHeight then
      lbDEBUG.TopIndex := lbDEBUG.Items.Count - (lbDEBUG.ClientHeight div lbDEBUG.ItemHeight);
  end;
end;

end.
