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

uses Classes, debugkeyboard, SysUtils, KeymanDeveloperDebuggerMemo;

type
  TDeadKeyInfo = class // Kept as a list of active deadkeys
  private
    function GetPosition: Integer;
    procedure SetPosition(const Value: Integer);
    function GetDeleted: Boolean;
    function GetPos2: Integer;
  public
    //Position: Integer;
    memo: TKeymanDeveloperDebuggerMemo;  // I3323
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
end;

function TDeadKeyInfo.GetDeleted: Boolean;
begin
  // TODO
  Result := False;
end;

function TDeadKeyInfo.GetPosition: Integer;
begin
  // TODO
  Result := 0;
end;

function TDeadKeyInfo.GetPos2: Integer;
begin
  // TODO
  Result := 0;
end;

procedure TDeadKeyInfo.SetPosition(const Value: Integer);
begin
  // TODO
end;

end.
