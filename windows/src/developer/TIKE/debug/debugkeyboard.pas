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

uses Classes, SysUtils;

{ Load line number details from compiled keyboard }

type
  TDebugGroup = class
    Name: WideString;
    Line, MatchLine, NomatchLine: Integer;
  end;

  TDebugStore = class
    SystemID: Integer;
    Name, AString: WideString;
  end;

  TDebugDeadkey = class
    Name: WideString;
    Value: Integer;
  end;

  TDebugGroupList = class(TList)
  protected
    function Get(Index: Integer): TDebugGroup;
    procedure Put(Index: Integer; Item: TDebugGroup);
  public
    property Items[Index: Integer]: TDebugGroup read Get write Put; default;
    function Add(Item: TDebugGroup): Integer;
  end;

  TDebugStoreList = class(TList)
  protected
    function Get(Index: Integer): TDebugStore;
    procedure Put(Index: Integer; Item: TDebugStore);
  public
    property Items[Index: Integer]: TDebugStore read Get write Put; default;
    function Add(Item: TDebugStore): Integer;
  end;

  TDebugDeadkeyList = class(TList)
  protected
    function Get(Index: Integer): TDebugDeadkey;
    procedure Put(Index: Integer; Item: TDebugDeadkey);
  public
    property Items[Index: Integer]: TDebugDeadkey read Get write Put; default;
    function Add(Item: TDebugDeadkey): Integer;
  end;

  TDebugKeyboard = class
    BeginUnicodeLine, BeginANSILine: Integer;
    Groups: TDebugGroupList;
    Deadkeys: TDebugDeadkeyList;
    Stores: TDebugStoreList;
    Memory: TMemoryStream;
    constructor Create(Filename: string);
    destructor Destroy; override;
  end;

implementation

uses kmxfile;

{ TDebugGroupList }

function TDebugGroupList.Get(Index: Integer): TDebugGroup;        begin Result := TDebugGroup(inherited Get(Index)); end;
procedure TDebugGroupList.Put(Index: Integer; Item: TDebugGroup); begin inherited Put(Index, Pointer(Item)); end;
function TDebugGroupList.Add(Item: TDebugGroup): Integer;         begin Result := inherited Add(Pointer(Item)); end;

{ TDebugDeadkeyList }

function TDebugDeadkeyList.Get(Index: Integer): TDebugDeadkey;        begin Result := TDebugDeadkey(inherited Get(Index)); end;
procedure TDebugDeadkeyList.Put(Index: Integer; Item: TDebugDeadkey); begin inherited Put(Index, Pointer(Item)); end;
function TDebugDeadkeyList.Add(Item: TDebugDeadkey): Integer;         begin Result := inherited Add(Pointer(Item)); end;

{ TDebugStoreList }

function TDebugStoreList.Get(Index: Integer): TDebugStore;          begin Result := TDebugStore(inherited Get(Index)); end;
procedure TDebugStoreList.Put(Index: Integer; Item: TDebugStore);   begin inherited Put(Index, Pointer(Item)); end;
function TDebugStoreList.Add(Item: TDebugStore): Integer;           begin Result := inherited Add(Pointer(Item)); end;

{ TDebugKeyboard }

constructor TDebugKeyboard.Create(Filename: string);
var
  ki: TKeyboardInfo;
  pch, pch2: PByte;  // I3310
  kfs: PKeyboardFileStore;
  kfh: PKeyboardFileHeader;
  grp, ln, i, n: Integer;
  dk: TDebugDeadKey;
  ch: WideChar;
  store: TDebugStore;
  s: WideString;
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
      Inc(pch, kfh.dpStoreArray);

      for i := 0 to kfh.cxStoreArray - 1 do
      begin
        kfs := PKeyboardFileStore(pch);

        store := TDebugStore.Create;
        store.SystemID := kfs.dwSystemID;

        pch2 := PByte(Memory);  // I3310
        Inc(pch2, kfs.dpName);
        store.Name := PWideChar(pch2);

        pch2 := PByte(Memory);  // I3310
        Inc(pch2, kfs.dpString);
        store.AString := PWideChar(pch2);

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
              dk.Value := StrToIntDef(Copy(s,1,n-1), -1);
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
        Inc(pch, SizeOf(TKeyboardFileStore));
      end;
    end;
  end;
//  else
  //  Memory := nil;
end;

destructor TDebugKeyboard.Destroy;
var
  i: Integer;
begin
  for i := 0 to Groups.Count - 1 do Groups[i].Free;
  for i := 0 to Deadkeys.Count - 1 do Deadkeys[i].Free;
  for i := 0 to Stores.Count - 1 do Stores[i].Free;
  Groups.Free;
  Deadkeys.Free;
  Stores.Free;
  Memory.Free;
  inherited Destroy;
end;

end.
