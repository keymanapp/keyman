(*
  Name:             ExtShiftState
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2006

  Modified Date:    15 Jan 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2006 - mcdurdin - Initial version
                    28 Sep 2006 - mcdurdin - Added ext shift state validation
                    15 Jan 2007 - mcdurdin - Add ExtShiftStateToString function
*)
unit ExtShiftState;

interface

type
  TExtShiftStateValue = (essShift, essCtrl, essAlt, essLCtrl, essRCtrl, essLAlt, essRAlt);
  TExtShiftState = set of TExtShiftStateValue;

const
  ValidExtShiftStates: array[0..13] of TExtShiftState = (
    [],
    [essShift],
    [essCtrl],
    [essShift, essCtrl],
    [essAlt],
    [essShift, essAlt],
    [essCtrl, essAlt],
    [essShift, essCtrl, essAlt],
    [essRCtrl],
    [essShift, essRCtrl],
    [essRAlt],
    [essShift, essRAlt],
    [essRCtrl, essRAlt],
    [essShift, essRCtrl, essRAlt]);

function CleanExtShiftState(Value: TExtShiftState): TExtShiftState;
function ValidExtShiftStateIndex(Value: TExtShiftState): Integer;
function ExtShiftStateToString(Value: TExtShiftState): string;

implementation

uses
  System.TypInfo;

function CleanExtShiftState(Value: TExtShiftState): TExtShiftState;
begin
  if essRCtrl in Value then
  begin
    Exclude(Value, essLCtrl);
    Exclude(Value, essCtrl);
    if essLAlt in Value then Value := Value + [essRAlt] - [essLAlt];
    if essAlt in Value then Value := Value + [essRAlt] - [essAlt];
  end;
  if essRAlt in Value then
  begin
    Exclude(Value, essLAlt);
    Exclude(Value, essAlt);
    if essLCtrl in Value then Value := Value + [essRCtrl] - [essLCtrl];
    if essCtrl in Value then Value := Value + [essRCtrl] - [essCtrl];
  end;
  Result := Value;
end;

function ValidExtShiftStateIndex(Value: TExtShiftState): Integer;
var
  i: Integer;
begin
  Value := CleanExtShiftState(Value);
  for i := Low(ValidExtShiftStates) to High(ValidExtShiftStates) do
    if ValidExtShiftStates[i] = Value then
    begin
      Result := i;
      Exit;
    end;
  Result := 0;
end;

function ExtShiftStateToString(Value: TExtShiftState): string;
var
  i: TExtShiftStateValue;
begin
  Result := '';
  for I := Low(TExtShiftStateValue) to High(TExtShiftStateValue) do
    if I in Value then
    begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + GetEnumName(TypeInfo(TExtShiftStateValue), Ord(I));
    end;
end;

end.
