(*
  Name:             debugkeyboard
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Nov 2007

  Modified Date:    17 Aug 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    17 Aug 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit debugkeyboard;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  kmxfile;

{ Load line number details from compiled keyboard }

type
  TDebugKeyList = class;

  TDebugKey = class
    Key: WORD;
    Line: Cardinal;
    ShiftFlags: Cardinal;
    dpOutput: string;
    dpContext: string;
  end;

  TDebugGroup = class
    Name: string;
    Line, MatchLine, NomatchLine: Integer;
    Base: PKeyboardFileGroup;
    Keys: TDebugKeyList;
    Match: string;
    Nomatch: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TDebugStore = class
    SystemID: Integer;
    Name, AString: string;
    Base: PKeyboardFileStore;
  end;

  TDebugDeadkey = class
    Name: string;
    Value: Integer;
  end;

  TDebugGroupList = class(TObjectList<TDebugGroup>);
  TDebugStoreList = class(TObjectList<TDebugStore>);
  TDebugDeadkeyList = class(TObjectList<TDebugDeadkey>);
  TDebugKeyList = class(TObjectList<TDebugKey>);

  TDebugKeyboard = class
    BeginUnicodeLine, BeginANSILine: Integer;
    Groups: TDebugGroupList;
    Deadkeys: TDebugDeadkeyList;
    Stores: TDebugStoreList;
    Memory: TMemoryStream;
    constructor Create(Filename: string);
    destructor Destroy; override;
  end;

  { KMX file structures mapped into memory }

  TKeymanStoreEx = record
    Store: TKeyboardFileStore;
    MatchPosition: Integer;
  end;

  PKeymanStoreEx = ^TKeymanStoreEx;

  TKeymanKey = packed record
    Key: WideChar; packing: Word;
    Line: Cardinal;
    ShiftFlags: Cardinal;
    dpOutput: PWideChar;
    dpContext: PWideChar;
  end;

  PKeymanKey = ^TKeymanKey;

  TKeymanKeyEx = record
    Key: WideCHAR;
    Line: Cardinal;
    ShiftFlags: Cardinal;
    dpOutput: string;
    dpContext: string;
  end;

  TKeymanGroup = packed record
    dpName: PWideChar;
    dpKeyArray: PKeymanKey;
    dpMatch: PWideChar;
    dpNoMatch: PWideChar;
    cxKeyArray: Cardinal;
    fUsingKeys: LongBool;
  end;

  PKeymanGroup = ^TKeymanGroup;

  TKeymanGroupEx = record
    dpName: string;
    dpMatch: string;
    dpNoMatch: string;
    fUsingKeys: LongBool;
  end;

  TKeymanStore = packed record
    dwSystemID: Cardinal;
    dpName: PWideChar;
    dpValue: PWideChar;
  end;

  PKeymanStore = ^TKeymanStore;

implementation

uses
  kmxfileconsts;

{ TDebugKeyboard }

constructor TDebugKeyboard.Create(Filename: string);
var
  ki: TKeyboardInfo;
  pch, pch2, pchg: PByte;  // I3310
  kfs: PKeyboardFileStore;
  kfg: PKeyboardFileGroup;
  kfh: PKeyboardFileHeader;
  grp, ln, i, j, n: Integer;
  dk: TDebugDeadKey;
  ch: WideChar;
  store: TDebugStore;
  group: TDebugGroup;
  s: string;
  kfk: PKeyboardFileKey;
  key: TDebugKey;

  function StringFromMemory(dp: Cardinal): string;
  var
    pch: PByte;
  begin
    if dp = 0 then
      Exit('');
    pch := ki.MemoryDump.Memory;
    Inc(pch, dp);
    Result := PChar(pch);
  end;

begin
  inherited Create;

  BeginUnicodeLine := -1;
  BeginANSILine := -1;

  Groups := TDebugGroupList.Create;
  Deadkeys := TDebugDeadkeyList.Create;
  Stores := TDebugStoreList.Create;

  try
    GetKeyboardInfo(FileName, True, ki)
  except
    Memory := nil;
    Exit;
  end;

  begin
    Memory := ki.MemoryDump;
    with ki.MemoryDump do
    begin
      pch := PByte(Memory);  // I3310
      kfh := PKeyboardFileHeader(Memory);

      Inc(pch, kfh.dpGroupArray);
      kfg := PKeyboardFileGroup(pch);

      for i := 0 to Integer(kfh.cxGroupArray) - 1 do
      begin
        group := TDebugGroup.Create;
        group.Base := kfg;
        group.Match := StringFromMemory(kfg.dpMatch);
        group.Nomatch := StringFromMemory(kfg.dpNoMatch);
        Groups.Add(group);

        pch := PByte(Memory);
        Inc(pch, kfg.dpKeyArray);
        kfk := PKeyboardFileKey(pch);
        for j := 0 to Integer(kfg.cxKeyArray) - 1 do
        begin
          key := TDebugKey.Create;
          key.Key := kfk.Key;
          key.Line := kfk.Line;
          key.ShiftFlags := kfk.ShiftFlags;
          key.dpOutput := StringFromMemory(kfk.dpOutput);
          key.dpContext := StringFromMemory(kfk.dpContext);
          group.Keys.Add(key);
          Inc(kfk);
        end;
      end;

      pch := PByte(Memory);  // I3310
      Inc(pch, kfh.dpStoreArray);
      kfs := PKeyboardFileStore(pch);

      for i := 0 to Integer(kfh.cxStoreArray) - 1 do
      begin
        store := TDebugStore.Create;
        store.Base := kfs;
        store.SystemID := kfs.dwSystemID;
        store.Name := StringFromMemory(kfs.dpName);
        store.AString := StringFromMemory(kfs.dpString);
        Stores.Add(store);

        if kfs.dwSystemID = TSS_DEBUG_LINE then
        begin
          ln := StrToIntDef(store.AString, 0);

          s := store.Name;
          if s <> '' then
          begin
            ch := s[1]; Delete(s,1,1);
            if ch = 'B' then
              if s[1] = 'U' then BeginUnicodeLine := ln else BeginANSILine := ln
            else if ch = 'D' then
            begin
              dk := TDebugDeadKey.Create;
              DeadKeys.Add(dk);
              n := Pos(' ', s); if n = 0 then n := Length(s)+1;
              // The deadkey value stored in the debug data is zero-based but
              // we need to work with a 1-based value (0 conflates with
              // end-of-string in extended strings)
              dk.Value := StrToIntDef(Copy(s,1,n-1), -2) + 1;
              dk.Name := Copy(s,n+1, 128);
            end
            else
            begin
              n := Pos(' ', s); if n = 0 then n := Length(s)+1;
              grp := StrToIntDef(Copy(s,1,n-1), -1);
              if grp > -1 then
              begin
                while grp >= Groups.Count do Groups.Add(TDebugGroup.Create);
                case ch of
                  'G': begin Groups[grp].Line := ln; Groups[grp].Name := Copy(s, n+1, 128); end;
                  'M': Groups[grp].MatchLine := ln;
                  'N': Groups[grp].NomatchLine := ln;
                end;
              end;
            end;
          end;
        end;
        Inc(kfs);
      end;
    end;
  end;
end;

destructor TDebugKeyboard.Destroy;
begin
  Groups.Free;
  Deadkeys.Free;
  Stores.Free;
  Memory.Free;
  inherited Destroy;
end;

{ TDebugGroup }

constructor TDebugGroup.Create;
begin
  inherited Create;
  Keys := TDebugKeyList.Create;
end;

destructor TDebugGroup.Destroy;
begin
  Keys.Free;
  inherited Destroy;
end;

end.
