(*
  Name:             Hints
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - I1248 - Initial version
                    10 Dec 2010 - mcdurdin - I2558 - Remove LazyWrite
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit Hints;

interface

uses
  HintConsts,
  keymanapi_TLB;

function IsHintEnabled(Hint: TKeymanHint): Boolean;
procedure DisableHint(Hint: TKeymanHint);
procedure EnableHint(Hint: TKeymanHint);
procedure DisableAllHints;
procedure EnableAllHints;
procedure ResetAllHints;

function GetHintName(const Hint: TKeymanHint): string;

implementation

uses
  Windows, ErrorControlledRegistry, RegistryKeys, TypInfo;

function GetHintName(const Hint: TKeymanHint): string;
begin
  Result := GetEnumName(TypeInfo(TKeymanHint), Ord(Hint));
end;

function IsHintEnabled(Hint: TKeymanHint): Boolean;
var
  HintName: string;
begin
  HintName := GetHintName(Hint);
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanEngine) then
    begin
      if ValueExists(SRegValue_EnableHints) and not ReadBool(SRegValue_EnableHints)
        then Result := False // Hint is globally disabled
        else Result :=
          not OpenKeyReadOnly('\'+SRegKey_KeymanDesktop) or
          not OpenKeyReadOnly(SRegKey_Hints) or
          not ValueExists(HintName) or
          ReadBool(HintName);
    end
    else
      Result := True;
  finally
    Free;
  end;
end;

procedure DisableHint(Hint: TKeymanHint);
var
  HintName: string;
begin
  HintName := GetHintName(Hint);
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanDesktop, True) and OpenKey(SRegKey_Hints, True) then
      WriteBool(HintName, False);
  finally
    Free;
  end;
end;

procedure EnableHint(Hint: TKeymanHint);
var
  HintName: string;
begin
  HintName := GetHintName(Hint);
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanDesktop, True) and OpenKey(SRegKey_Hints, True) then
      WriteBool(HintName, True);
  finally
    Free;
  end;
end;

procedure DisableAllHints;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanDesktop, True) then
      WriteBool(SRegValue_EnableHints, False);
  finally
    Free;
  end;
end;

procedure EnableAllHints;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanDesktop, True) then
      WriteBool(SRegValue_EnableHints, True);
  finally
    Free;
  end;
end;

procedure ResetAllHints;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_KeymanDesktop, True) and KeyExists(SRegKey_Hints) then
      DeleteKey(SRegKey_Hints);
  finally
    Free;
  end;
end;

end.
