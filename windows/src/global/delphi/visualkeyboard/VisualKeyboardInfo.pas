(*
  Name:             VisualKeyboardInfo
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      3 May 2011

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit VisualKeyboardInfo;

interface

uses VisualKeyboard, Contnrs;

type
  TVisualKeyboardInfo = class
  public
    Keyboard: TVisualKeyboard;
    FileName: string;
    KeymanID: Integer;
    KeymanName: string;
    destructor Destroy; override;
  end;

  TVisualKeyboardInfoList = class(TObjectList)
  protected
    function Get(Index: Integer): TVisualKeyboardInfo;
    procedure Put(Index: Integer; Item: TVisualKeyboardInfo);
  public
    property Items[Index: Integer]: TVisualKeyboardInfo read Get write Put; default;
    function Add(Item: TVisualKeyboardInfo): Integer;
    procedure Load;
  end;

implementation

uses
  Classes,
  klog,
  ErrorControlledRegistry, 
  RegistryKeys,
  SysUtils,
  Windows;

{ TVisualKeyboardInfoList }

function TVisualKeyboardInfoList.Add(Item: TVisualKeyboardInfo): Integer;
begin
  Result := inherited Add(Item);
end;

function TVisualKeyboardInfoList.Get(Index: Integer): TVisualKeyboardInfo;
begin
  Result := TVisualKeyboardInfo(inherited Get(Index));
end;

procedure TVisualKeyboardInfoList.Load;
  procedure LoadExt(hkey: HKEY);
  var
    i: Integer;
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      RootKey := hkey;
      for i := 0 to Count - 1 do
        if OpenKeyReadOnly('\'+SRegKey_InstalledKeyboards+'\'+Items[i].KeymanName) then
          if ValueExists(SRegValue_VisualKeyboard) then Items[i].FileName := ReadString(SRegValue_VisualKeyboard);
    finally
      Free;
    end;
  end;

var
  str: TStringList;
  vki: TVisualKeyboardInfo;
  i: Integer;
begin
  KL.Log('Loading visual keyboards');
  Clear;
  str := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(SRegKey_ActiveKeyboards) then
    begin
      GetValueNames(str);
      for i := 0 to str.Count - 1 do
      begin
        vki := TVisualKeyboardInfo.Create;
        vki.KeymanID := StrToIntDef(str[i], 10000);
        vki.KeymanName := ReadString(str[i]);
        vki.Keyboard := nil;
        vki.FileName := '';
        Add(vki);
      end;
    end;
    LoadExt(HKEY_CURRENT_USER);
    LoadExt(HKEY_LOCAL_MACHINE);
    for i := 0 to Count - 1 do
      KL.Log('Visual Keyboard '+Items[i].KeymanName+'='+Items[i].FileName);
   finally
    Free;
    str.Free;
  end;
end;

procedure TVisualKeyboardInfoList.Put(Index: Integer; Item: TVisualKeyboardInfo);
begin
  inherited Put(Index, Item);
end;

{ TVisualKeyboardInfo }

destructor TVisualKeyboardInfo.Destroy;
begin
  FreeAndNil(Keyboard);
  inherited Destroy;
end;

end.
 
