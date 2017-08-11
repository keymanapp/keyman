(*
  Name:             SortedKeyboardList
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      11 Jan 2011

  Modified Date:    1 Dec 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          11 Jan 2011 - mcdurdin - I1867 - Sort keyboards alphabetically in OSK, menus
                    08 Jun 2012 - mcdurdin - I3311 - V9.0 - Change 'published' to 'public' on classes that don't need RTTI
                    01 Dec 2012 - mcdurdin - I3614 - V9.0 - Start removal of keyboard selection list from UI
*)
deleted unit SortedKeyboardList;  // I1867   // I3614

interface

uses
  Classes,
  keymanapi_TLB;

type
  TSortedKeysboardList = class(TList)  // I3311
  private
    function Get(Index: Integer): IKeymanKeyboardInstalled;
    procedure Put(Index: Integer; const Value: IKeymanKeyboardInstalled);
  public
    procedure Clear; override;
    procedure Sort; reintroduce;
    function Add(kbd: IKeymanKeyboardInstalled): Integer;
    property Items[Index: Integer]: IKeymanKeyboardInstalled read Get write Put; default;
  end;

implementation

uses
  SysUtils;

function SortKeyboards(Item1, Item2: Pointer): Integer;
var
  k1, k2: IKeymanKeyboardInstalled;
begin
  k1 := IKeymanKeyboardInstalled(Item1);
  k2 := IKeymanKeyboardInstalled(Item2);
  Result := WideCompareText(k1.KeyboardName, k2.KeyboardName);
end;

{ TSortedKeyboardList }

function TSortedKeyboardList.Add(kbd: IKeymanKeyboardInstalled): Integer;
begin
  Result := inherited Add(nil);
  IKeymanKeyboardInstalled(List[Result]) := kbd;
end;

procedure TSortedKeyboardList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    IInterface(List[I]) := nil;

  inherited Clear;
end;

function TSortedKeyboardList.Get(Index: Integer): IKeymanKeyboardInstalled;
begin
  Result := IKeymanKeyboardInstalled(List[Index]);
end;

procedure TSortedKeyboardList.Put(Index: Integer; const Value: IKeymanKeyboardInstalled);
begin
  IKeymanKeyboardInstalled(List[Index]) := Value;
end;

procedure TSortedKeyboardList.Sort;
begin
  inherited Sort(SortKeyboards);
end;


end.
