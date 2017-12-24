(*
  Name:             keymanoption
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    28 Aug 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add Serialize
                    04 Dec 2006 - mcdurdin - Remove description from options (localizable)
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    28 Aug 2014 - mcdurdin - I4390 - V9.0 - Free vs Pro
*)
unit keymankeyboardoption;

interface

uses
  System.Classes,
  Winapi.ActiveX,
  keymanapi_TLB,
  keymanautoobject,
  KeymanContext;

type
  TKeymanKeyboardOption = class(TKeymanAutoObject, IKeymanKeyboardOption)
  private
    FKeyboardID, FName, FValue: string;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString;
      References: TStrings): WideString; override;

    { IKeymanKeyboardOption }
    function Get_Name: WideString; safecall;
    function Get_Value: WideString; safecall;
    procedure Set_Value(const Value: WideString); safecall;
  public
    constructor Create(AContext: TKeymanContext; const AKeyboardID, AName, AValue: string);
  end;

implementation

uses
//  ComServ,
  System.Win.Registry,
  RegistryKeys,
  utilxml;

procedure TKeymanKeyboardOption.Set_Value(const Value: WideString);
begin
  FValue := Value;
  with TRegistry.Create do  // I2890
  try
    if OpenKey(BuildKeyboardOptionKey_CU(FKeyboardID), True) then
    begin
      WriteString(FName, Value);
      if OpenKey(SRegSubKey_SharedOptions, False) then
        WriteString(FName, Value);
    end;
  finally
    Free;
  end;
end;

constructor TKeymanKeyboardOption.Create(AContext: TKeymanContext; const AKeyboardID, AName, AValue: string);
begin
  inherited Create(AContext, IKeymanKeyboardOption);
  FKeyboardID := AKeyboardID;
  FName := AName;
  FValue := AValue;
end;

function TKeymanKeyboardOption.Get_Name: WideString;
begin
  Result := FName;
end;

function TKeymanKeyboardOption.Get_Value: WideString;
begin
  Result := FValue;
end;

function TKeymanKeyboardOption.Serialize(Flags: TOleEnum; const ImagePath: WideString;
  References: TStrings): WideString;
begin
  Result := XMLFormat([
    'name', Get_Name,
    'value', Get_Value]);
end;

end.
