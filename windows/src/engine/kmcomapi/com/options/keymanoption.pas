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
unit keymanoption;

interface

uses
  ComObj, ActiveX, keymanapi_TLB, keymanautoobject, KeymanContext, utilkeymanoption, StdVcl, Classes;

type
  TKeymanOption = class(TKeymanAutoObject, IKeymanOption)
  private
    Fopt: TUtilKeymanOptionEntry;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString;
      References: TStrings): WideString; override;

    { IKeymanOption }
    function Get_DefaultValue: OleVariant; safecall;
    function Get_Enabled: WordBool; safecall;
    function Get_Group: WideString; safecall;
    function Get_ID: WideString; safecall;
    function Get_OptionType: TOleEnum; safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(Value: OleVariant); safecall;
  public
    constructor Create(AContext: TKeymanContext; const Aopt: TUtilKeymanOptionEntry);
  end;

implementation

uses
  System.Variants,
  utilxml;

constructor TKeymanOption.Create(AContext: TKeymanContext; const Aopt: TUtilKeymanOptionEntry);
begin
  inherited Create(AContext, IKeymanOption);
  Fopt := Aopt;
end;

function TKeymanOption.Get_ID: WideString;
begin
  Result := Fopt.ID;
end;

function TKeymanOption.Get_OptionType: TOleEnum;
begin
  Result := Fopt.OptionType;
end;

function TKeymanOption.Get_Value: OleVariant;
begin
  case FOpt.OptionType of
    kotBool: Result := Fopt.BoolValue;
    kotLong: Result := Fopt.IntValue;
    kotString: Result := Fopt.StringValue;
    else Result := Null;
  end;
end;

function TKeymanOption.Serialize(Flags: TOleEnum; const ImagePath: WideString;
  References: TStrings): WideString;
begin
  Result := XMLFormat([
    'id', Get_ID,
    'value', Get_Value,
    'optiontype', Get_OptionType,
    'defaultvalue', Get_DefaultValue,
    'enabled', Get_Enabled,
    'group', Get_Group]);
end;

procedure TKeymanOption.Set_Value(Value: OleVariant);
begin
  case FOpt.OptionType of
    kotBool:   Fopt.BoolValue := Value;
    kotLong:   Fopt.IntValue := Value;
    kotString: Fopt.StringValue := Value;
  end;
end;

function TKeymanOption.Get_DefaultValue: OleVariant;
begin
  case FOpt.OptionType of
    kotBool: Result := Fopt.DefaultBoolValue;
    kotLong: Result := Fopt.DefaultIntValue;
    kotString: Result := Fopt.DefaultStringValue;
    else Result := Null;
  end;
end;

function TKeymanOption.Get_Enabled: WordBool;
begin
  Result := Fopt.Enabled;
end;

function TKeymanOption.Get_Group: WideString;
begin
  Result := Fopt.Group;
end;

end.
