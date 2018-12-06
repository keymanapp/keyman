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
    FDeleted: Boolean;
    FPosition: Integer;
  public
    //Position: Integer;
    memo: TKeymanDeveloperDebuggerMemo;  // I3323
    Deadkey: TDebugDeadkey;
    procedure Delete;
    property Position: Integer read FPosition write FPosition;
    property Deleted: Boolean read FDeleted;
  end;

implementation

{ TDeadKeyInfo }

procedure TDeadKeyInfo.Delete;
begin
  FDeleted := True;
end;

end.
