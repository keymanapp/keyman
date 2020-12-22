(*
  Name:             keymanoptions
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    6 Feb 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add AutoRefershKeyman call
                    12 Aug 2008 - mcdurdin - Avoid crash with missing options
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    01 Jan 2013 - mcdurdin - I3717 - V9.0 - Need ability to select base keyboard in Keyman Configuration
                    17 Jan 2013 - mcdurdin - I3759 - V9.0 - UnderlyingLayout stores a HKL in the registry but expects a KLID in the code
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
*)
unit keymankeyboardoptions;

interface

uses
  Winapi.ActiveX,
  System.Win.ComObj,

  keymanapi_TLB,
  keymanautoobject,
  KeymanContext,
  keymankeyboardoption,
  internalinterfaces;

type
  TKeymanKeyboardOptions = class(TKeymanAutoCollectionObject, IKeymanKeyboardOptions)
  private
    type TKeyboardOptionList = TAutoObjectList;
  private
    FKeyboardID: string;
    FKeyboardOptions: TKeyboardOptionList;
  protected
    procedure DoRefresh; override;

    { IKeymanKeyboardOptions }
    function Get_Items(Index: OleVariant): IKeymanKeyboardOption; safecall;
    function IndexOf(const Name: WideString): Integer; safecall;

  public
    constructor Create(AContext: TKeymanContext; const AKeyboardID: string);
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  ErrorControlledRegistry,
  RegistryKeys,
  keymanerrorcodes;

constructor TKeymanKeyboardOptions.Create(AContext: TKeymanContext; const AKeyboardID: string);
begin
  FKeyboardOptions := TKeyboardOptionList.Create;
  FKeyboardID := AKeyboardID;
  inherited Create(AContext, IKeymanKeyboardOptions, FKeyboardOptions);
  Refresh;
end;

destructor TKeymanKeyboardOptions.Destroy;
begin
  FKeyboardOptions.Free;
  inherited;
end;

function TKeymanKeyboardOptions.Get_Items(Index: OleVariant): IKeymanKeyboardOption;
var
  i: Integer;
begin
  Result := nil;

  if VarType(Index) = varOleStr then
  begin
    i := IndexOf(Index);
    if i < 0 then
      i := FKeyboardOptions.Add(TKeymanKeyboardOption.Create(Context, FKeyboardID, Index, ''));
  end
  else
  begin
    i := Index;
    if (i < 0) or (i >= Get_Count) then
    begin
      ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([VarToStr(Index)]));
      Exit;
    end;
  end;

  Result := FKeyboardOptions[i] as IKeymanKeyboardOption;
end;

function TKeymanKeyboardOptions.IndexOf(const Name: WideString): Integer;
var
  i: Integer;
begin
  for i := 0 to Get_Count - 1 do
    if AnsiCompareText(Name, (FKeyboardOptions[i] as IKeymanKeyboardOption).Name) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TKeymanKeyboardOptions.DoRefresh;
var
  i: Integer;
  s: TStringList;
begin
  FKeyboardOptions.Clear;

  s := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(BuildKeyboardOptionKey_CU(FKeyboardID)) then
    begin
      GetValueNames(s);
      for i := 0 to s.Count - 1 do
        FKeyboardOptions.Add(TKeymanKeyboardOption.Create(Context, FKeyboardID, s[i], ReadString(s[i])));
    end;
  finally
    Free;
    s.Free;
  end;
end;

end.
