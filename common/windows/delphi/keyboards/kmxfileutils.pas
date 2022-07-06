(*
  Name:             kmxfileutils
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      27 Mar 2008

  Modified Date:    27 Aug 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          27 Mar 2008 - mcdurdin - Initial version
                    22 Mar 2010 - mcdurdin - I2243 - Support for nul in LHS of rule
                    27 Aug 2012 - mcdurdin - I3442 - V9.0 - Update xstring utils in Delphi to support v8 and v9
*)
unit kmxfileutils;

interface

uses
  Winapi.Windows,

  kmxfile;

// Sentinel code is similar to that found in CompileKeymanWeb.pas, but here is working
// with a compiled keyboard rather than with the intermediate obj that CompileKeymanWeb
// version works with.
//
// TODO: A refactor in the future may be worthwhile to consolidate these two sentinel
//       processors. I have not done it at present because it introduces risk in the
//       web compiler for minimal direct benefit.

type
  TSentinelRecordAny = record
    StoreIndex: Integer;
    Store: PKeyboardFileStore;
  end;

  TSentinelRecordIndex = record
    StoreIndex: Integer;
    Store: PKeyboardFileStore;
    Index: Integer;
  end;

  TSentinelRecordContextEx = record
    Index: Integer;
  end;

  TSentinelRecordDeadkey = record
    DeadKey: Integer;
  end;

  TSentinelRecordUse = record
    GroupIndex: Integer;
    Group: PKeyboardFileGroup;
  end;

  TSentinelRecordCall = record
    StoreIndex: Integer;
    Store: PKeyboardFileStore;
  end;

  TSentinelRecordIfOpt = record
    StoreIndex1: Integer;
    Store1: PKeyboardFileStore;
    StoreIndex2: Integer;
    Store2: PKeyboardFileStore;
    IsNot: Integer;
  end;

  TSentinelRecordSetOpt = record
    StoreIndex1: Integer;
    Store1: PKeyboardFileStore;
    StoreIndex2: Integer;
    Store2: PKeyboardFileStore;
  end;

  TSentinelRecordResetOpt = record
    StoreIndex: Integer;
    Store: PKeyboardFileStore;
  end;

  TSentinelRecordSaveOpt = record
    StoreIndex: Integer;
    Store: PKeyboardFileStore;
  end;

  TSentinelRecordIfSystemStore = record
    dwSystemID: DWORD;
    SystemStore: PKeyboardFileStore;
    StoreIndex: Integer;
    Store: PKeyboardFileStore;
    IsNot: Integer;
  end;

  TSentinelRecordSetSystemStore = record
    dwSystemID: DWORD;
    SystemStore: PKeyboardFileStore;
    StoreIndex: Integer;
    Store: PKeyboardFileStore;
  end;

  TSentinelRecord = record
    IsSentinel: Boolean;
    Code: Integer;
    Any: TSentinelRecordAny;
    Index: TSentinelRecordIndex;
    Deadkey: TSentinelRecordDeadkey;
    Use: TSentinelRecordUse;
    Call: TSentinelRecordCall;
    ContextEx: TSentinelRecordContextEx;
    IfOpt: TSentinelRecordIfOpt;
    IfSystemStore: TSentinelRecordIfSystemStore;
    SetOpt: TSentinelRecordSetOpt;
    SetSystemStore: TSentinelRecordSetSystemStore;
    ResetOpt: TSentinelRecordResetOpt;
    SaveOpt: TSentinelRecordSaveOpt;
    ChrVal: DWord;
  end;

function incxstr(p: PWideChar): PWideChar;
function xstrlen(p: PWideChar): Integer;
function xstrlen_printing(p: PWideChar): Integer;
function GetSuppChar(p: PWideChar): DWord;
function ExpandSentinel(
  pwsz: PWideChar; fk: PKeyboardFileHeader): TSentinelRecord;

implementation

uses
  kmxfileconsts,
  Unicode;

function incxstr(p: PWideChar): PWideChar;
begin
  Result := p;
  if Result^ = #0 then Exit;

  if PWord(Result)^ <> UC_SENTINEL then
  begin
		if (PWord(Result)^ >= $D800) and (PWord(Result)^ <= $DBFF) then
    begin
      Inc(Result);
      if (PWord(Result)^ >= $DC00) and (PWord(Result)^ <= $DFFF) then Inc(Result);
    end
    else Inc(Result);
		Exit;
  end;

  Inc(Result);

  case PWord(Result)^ of
    CODE_ANY:      Inc(Result, 2);
    CODE_INDEX:    Inc(Result, 3);
    CODE_USE:      Inc(Result, 2);
    CODE_DEADKEY:  Inc(Result, 2);
    CODE_EXTENDED: begin Inc(Result, 3); while (PWord(Result)^ <> UC_SENTINEL_EXTENDEDEND) and (Result^ <> #0) do Inc(Result); Inc(Result); end;
    CODE_CALL:     Inc(Result, 2);
    CODE_CONTEXTEX: Inc(Result, 2);
    CODE_NOTANY:   Inc(Result, 2);

    CODE_CLEARCONTEXT: Inc(Result, 2);  // I3442
    CODE_IFOPT:    Inc(Result, 4);  // I3442
    CODE_IFSYSTEMSTORE: Inc(Result, 4);  // I3442
    CODE_SETOPT:   Inc(Result, 3);  // I3442
    CODE_SETSYSTEMSTORE: Inc(Result, 3);  // I3442
    CODE_RESETOPT: Inc(Result, 2);  // I3442
    CODE_SAVEOPT:  Inc(Result, 2);  // I3442

    else Inc(Result);
  end;
end;

function xstrlen(p: PWideChar): Integer;
begin
  Result := 0;
  while p^ <> #0 do
  begin
    p := incxstr(p);
    Inc(Result);
  end;
end;

function xstrlen_printing(p: PWideChar): Integer;
var
  q: PWideChar;
begin
  Result := 0;
  while p^ <> #0 do
  begin
    if PWord(p)^ = UC_SENTINEL then
    begin
      q := p;
      Inc(q);
      case PWord(q)^ of  // I3442
        CODE_DEADKEY,
        CODE_NUL,
        CODE_IFOPT,
        CODE_IFSYSTEMSTORE:
          Dec(Result);
      end;
    end;
    p := incxstr(p);
    Inc(Result);
  end;
end;

function GetSuppChar(p: PWideChar): DWord;
var
  ch1: WideChar;
begin
  Result := Ord(p^);
  if Uni_IsSurrogate1(p^) then
  begin
    ch1 := p^;
    Inc(p);
    if Uni_IsSurrogate2(p^) then
      Result := Uni_SurrogateToUTF32(ch1, p^);
  end;
end;

function ExpandSentinel(
  pwsz: PWideChar; fk: PKeyboardFileHeader): TSentinelRecord;
var
  i: Integer;
  Found: Boolean;
  function BaseMem(dp: DWord): PByte;
  begin
    Result := PByte(fk);
    Inc(Result, dp);
  end;
  function BaseStore(dp: DWord; n: DWord): PKeyboardFileStore;
  begin
    Result := PKeyboardFileStore(BaseMem(dp));
    Inc(Result, n);
  end;
  function BaseGroup(dp: DWord; n: DWord): PKeyboardFileGroup;
  begin
    Result := PKeyboardFileGroup(BaseMem(dp));
    Inc(Result, n);
  end;

begin
  FillChar(Result, SizeOf(Result), 0);
  if Ord(pwsz^) = UC_SENTINEL then
  begin
    Result.IsSentinel := True;
    Inc(pwsz);
    Result.Code := Ord(pwsz^);
    Inc(pwsz);
    case Result.Code of
      CODE_ANY, CODE_NOTANY:      // I3981
        begin
          Result.Any.StoreIndex := Ord(pwsz^) - 1;
          Result.Any.Store := BaseStore(fk.dpStoreArray, Result.Any.StoreIndex);
        end;
      CODE_INDEX:
        begin
          Result.Index.StoreIndex := Ord(pwsz^) - 1;
          Result.Index.Store := BaseStore(fk.dpStoreArray, Result.Index.StoreIndex);
          Inc(pwsz);
          Result.Index.Index := Ord(pwsz^);
        end;
      CODE_DEADKEY:
        Result.DeadKey.DeadKey := Ord(pwsz^) - 1;
      CODE_USE:
        begin
          Result.Use.GroupIndex := Ord(pwsz^) - 1;
          Result.Use.Group := BaseGroup(fk.dpGroupArray, Result.Use.GroupIndex);
        end;
      CODE_CALL:
        begin
          Result.Call.StoreIndex := Ord(pwsz^) - 1;
          Result.Call.Store := BaseStore(fk.dpStoreArray, Result.Call.StoreIndex);
        end;
      CODE_CONTEXTEX:
        Result.ContextEx.Index := Ord(pwsz^);
      CODE_SETOPT:    // I3429
        begin
          Result.SetOpt.StoreIndex1 := Ord(pwsz^) - 1;
          Result.SetOpt.Store1 := BaseStore(fk.dpStoreArray, Result.SetOpt.StoreIndex1);
          Inc(pwsz);
          Result.SetOpt.StoreIndex2 := Ord(pwsz^) - 1;
          Result.SetOpt.Store2 := BaseStore(fk.dpStoreArray, Result.SetOpt.StoreIndex2);
        end;
      CODE_SETSYSTEMSTORE:  // I3437
        begin
          Result.SetSystemStore.dwSystemID := Ord(pwsz^) - 1;
          Result.SetSystemStore.SystemStore := BaseStore(fk.dpStoreArray, 0);
          Found := False;

          for i := 0 to fk.cxStoreArray - 1 do
          begin
            if Result.SetSystemStore.SystemStore.dwSystemID = Result.SetSystemStore.dwSystemID then
            begin
              Found := True;
              Break;
            end;
            Inc(Result.SetSystemStore.SystemStore);
          end;

          if not Found then Result.SetSystemStore.SystemStore := nil;

          Inc(pwsz);
          Result.SetSystemStore.StoreIndex := Ord(pwsz^) - 1;
          Result.SetSystemStore.Store := BaseStore(fk.dpStoreArray, Result.SetSystemStore.StoreIndex);
        end;
      CODE_RESETOPT:  // I3429
        begin
          Result.ResetOpt.StoreIndex := Ord(pwsz^) - 1;
          Result.ResetOpt.Store := BaseStore(fk.dpStoreArray, Result.ResetOpt.StoreIndex);
        end;
      CODE_SAVEOPT:  // I3429
        begin
          Result.SaveOpt.StoreIndex := Ord(pwsz^) - 1;
          Result.SaveOpt.Store := BaseStore(fk.dpStoreArray, Result.SaveOpt.StoreIndex);
        end;
      CODE_IFOPT:  // I3429
        begin
          Result.IfOpt.StoreIndex1 := Ord(pwsz^) - 1;
          Result.IfOpt.Store1 := BaseStore(fk.dpStoreArray, Result.IfOpt.StoreIndex1);
          Inc(pwsz);
          Result.IfOpt.IsNot := Ord(pwsz^) - 1;  // I3429
          Inc(pwsz);
          Result.IfOpt.StoreIndex2 := Ord(pwsz^) - 1;
          Result.IfOpt.Store2 := BaseStore(fk.dpStoreArray, Result.IfOpt.StoreIndex2);
        end;
      CODE_IFSYSTEMSTORE:  // I3430
        begin
          Result.IfSystemStore.dwSystemID := Ord(pwsz^) - 1;
          Result.IfSystemStore.SystemStore := BaseStore(fk.dpStoreArray, 0);

          Found := False;

          for i := 0 to fk.cxStoreArray - 1 do
          begin
            if Result.IfSystemStore.SystemStore.dwSystemID = Result.IfSystemStore.dwSystemID then
            begin
              Found := True;
              Break;
            end;
            Inc(Result.IfSystemStore.SystemStore);
          end;

          if not Found then Result.IfSystemStore.SystemStore := nil;
          Inc(pwsz);
          Result.IfSystemStore.IsNot := Ord(pwsz^) - 1;  // I3430
          Inc(pwsz);
          Result.IfSystemStore.StoreIndex := Ord(pwsz^) - 1;
          Result.IfSystemStore.Store := BaseStore(fk.dpStoreArray, Result.IfSystemStore.StoreIndex);
        end;
    end;
  end
  else
    Result.ChrVal := GetSuppChar(pwsz);
end;

end.
