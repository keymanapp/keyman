(*
  Name:             StringGridEditControlled
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      8 Jun 2012

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          08 Jun 2012 - mcdurdin - I3309 - Use TStringGrid as base class, not TWideStringGrid
*)
unit StringGridEditControlled;

interface

uses Classes, Grids;

type
  TStringGridEditControlled = class;

  TCanEditShowEvent = procedure(Sender: TStringGridEditControlled; ACol, ARow: Integer; var CanShow: Boolean) of object;

  TStringGridEditControlled = class(TStringGrid)  // I3309
  private
    FOnCanEditShow: TCanEditShowEvent;
  protected
    function CanEditShow: Boolean; override;
  published
    property OnCanEditShow: TCanEditShowEvent read FOnCanEditShow write FOnCanEditShow;
  end;

procedure Register;

implementation

{ TStringGridEditControlled }

function TStringGridEditControlled.CanEditShow: Boolean;
begin
  Result := inherited CanEditShow;
  if Result then if Assigned(FOnCanEditShow) then FOnCanEditShow(Self, Col, Row, Result);
end;

procedure Register;
begin
  RegisterComponents('Keyman', [TStringGridEditControlled]);
end;

end.
