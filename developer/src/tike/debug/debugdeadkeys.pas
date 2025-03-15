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

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  debugkeyboard,
  KeymanDeveloperDebuggerMemo;

type
  TDeadKeyInfo = class // Kept as a list of active deadkeys
  private
    FDeleted: Boolean;
    FPosition: Integer; // in UTF-16 code units
    FSavedPosition: Integer;
  public
    //Position: Integer;
    memo: TKeymanDeveloperDebuggerMemo;  // I3323
    Deadkey: TDebugDeadkey;
    procedure Delete;
    property Position: Integer read FPosition write FPosition;
    property SavedPosition: Integer read FSavedPosition write FSavedPosition;
    property Deleted: Boolean read FDeleted;
  end;

  TDebugDeadkeyInfoList = class(TObjectList<TDeadkeyInfo>)
  public
    function GetFromPosition(pos: Integer): TDeadkeyInfo;
    procedure FillDeadkeys(startpos: Integer; var s: WideString);
  end;

const
  // This is not an ideal marker -- we were using U+FFFC previously, but
  // richedit silently converts it to U+0020.
  DeadKey_Marker = #$0001;

implementation

uses
  kmxfileconsts,
  utilstr;

{ TDeadKeyInfo }

procedure TDeadKeyInfo.Delete;
begin
  FDeleted := True;
end;

{ TDebugDeadkeyInfoList }

function TDebugDeadkeyInfoList.GetFromPosition(pos: Integer): TDeadkeyInfo;
begin
  for Result in Self do
    if Result.Position = pos then
      Exit;
  Result := nil;
end;

procedure TDebugDeadkeyInfoList.FillDeadkeys(startpos: Integer; var s: WideString);
var
  i: Integer;
  dk: TDeadKeyInfo;
begin
//  Dec(startpos);
  i := 1;
  while i <= Length(s) do
  begin
    if s[i] = DeadKey_Marker then
    begin
      for dk in Self do
      begin
        if dk.Position = startpos then
        begin
          s[i] := WChr(UC_SENTINEL);
          s := Copy(s, 1, i) + WChr(CODE_DEADKEY) + WChr(dk.Deadkey.Value) + Copy(s, i+1, Length(s));
          Inc(i, 2);
          Break;
        end;
      end;
    end;
    Inc(startpos);
    Inc(i);
  end;
end;


end.
