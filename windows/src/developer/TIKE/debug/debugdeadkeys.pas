(*
  Name:             debugdeadkeys
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      18 May 2012

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
*)
unit debugdeadkeys;

interface

uses Classes, debugkeyboard, SysUtils, KeymanDeveloperMemo
{$IFDEF USE_PLUSMEMO}
  ,
  PMSupport
{$ENDIF}
  ;

type
  TDeadKeyInfo = class // Kept as a list of active deadkeys
  private
{$IFDEF USE_PLUSMEMO}
    FNavStart, FNavStop: TPlusNavigator;  // I3323
{$ENDIF}
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    function GetDeleted: Boolean;
    function GetPos2: Integer;
  public
    //Position: Integer;
    memo: TKeymanDeveloperMemo;  // I3323
    Deadkey: TDebugDeadkey;
    destructor Destroy; override;
    property Position: Integer read GetPosition write SetPosition;
    property Pos2: Integer read GetPos2;
    property Deleted: Boolean read GetDeleted;
  end;

implementation

{ TDeadKeyInfo }

destructor TDeadKeyInfo.Destroy;
begin
  inherited;
{$IFDEF USE_PLUSMEMO}
  if Assigned(FNavStart) then FNavStart.Free;
  if Assigned(FNavStop) then FNavStop.Free;
{$ENDIF}
end;

function TDeadKeyInfo.GetDeleted: Boolean;
begin
{$IFDEF USE_PLUSMEMO}
  Result := True;
  if not Assigned(FNavStart) then Exit; //or not Assigned(FNavStop) then Exit;
  if FNavStop.Pos - FNavStart.Pos <= 0 then Exit;
{$ENDIF}
  Result := False;
end;

function TDeadKeyInfo.GetPosition: Integer;
begin
{$IFDEF USE_PLUSMEMO}
  if not Assigned(FNavStart)
    then Result := 0
    else Result := FNavStart.Pos;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TDeadKeyInfo.GetPos2: Integer;
begin
{$IFDEF USE_PLUSMEMO}
  if not Assigned(FNavStop)
    then Result := 0
    else Result := FNavStop.Pos;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

procedure TDeadKeyInfo.SetPosition(const Value: Integer);
begin
{$IFDEF USE_PLUSMEMO}
  if Assigned(FNavStart) then FNavStart.Free;
  if Assigned(FNavStop) then FNavStop.Free;
  FNavStart := TPlusNavigator.Create(memo);  // I3323
  FNavStart.Pos := Value;
  FNavStop := TPlusNavigator.Create(memo);  // I3323
  FNavStop.Pos := Value + 1;
{$ENDIF}
end;

end.
