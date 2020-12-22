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
unit keymanoptions;

interface

uses
  ComObj,
  ActiveX,
  keymanapi_TLB,
  StdVcl,
  keymanautoobject,
  KeymanContext,
  keymanoption,
  internalinterfaces,
  utilkeymanoption;

type
  TKeymanOptionList = TAutoObjectList;

  TKeymanOptions = class(TKeymanAutoCollectionObject, IKeymanOptions, IIntKeymanOptions)
  private
    FKeymanOptions: TKeymanOptionList;
    FInternalOptions: TUtilKeymanOptions;
  protected
    procedure DoRefresh; override;

    { IKeymanOptions }
    function Get_Items(Index: OleVariant): IKeymanOption; safecall;

    procedure Apply; safecall;
    function IndexOf(const ID: WideString): Integer; safecall;
  public
    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  ComServ,
  ErrorControlledRegistry,
  RegistryKeys,
  SysUtils,
  Glossary,
  KeymanOptionNames,
  keymanerrorcodes,
  Variants,
  utilkeyman;

constructor TKeymanOptions.Create(AContext: TKeymanContext);
begin
  FKeymanOptions := TKeymanOptionList.Create;
  inherited Create(AContext, IKeymanOptions, FKeymanOptions);
  FInternalOptions := TUtilKeymanOptions.Create;
  Refresh;
end;

destructor TKeymanOptions.Destroy;
begin
  FKeymanOptions.Free;
  FInternalOptions.Free;
  inherited;
end;

function TKeymanOptions.Get_Items(Index: OleVariant): IKeymanOption;
var
  i: Integer;
begin
  Result := nil;
  if VarType(Index) = varOleStr
    then i := IndexOf(Index)
    else i := Index;

  if (i < Get_Count) and (i >= 0)
    then Result := FKeymanOptions[i] as IKeymanOption
    else ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([VarToStr(Index)]));
end;

function TKeymanOptions.IndexOf(const ID: WideString): Integer;
var
  i: Integer;
begin
  for i := 0 to Get_Count - 1 do
    if AnsiCompareText(ID, (FKeymanOptions[i] as IKeymanOption).ID) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TKeymanOptions.Apply;
var
  I, FOldBaseLayout: Integer;
begin
  with TRegistryErrorControlled.Create do   // I3717
  try
    if OpenKey(SRegKey_KeymanEngine_CU, True) then
    begin
      if ValueExists(SRegValue_UnderlyingLayout)
        then FOldBaseLayout := StrToIntDef('$'+ReadString(SRegValue_UnderlyingLayout),0)   // I3759
        else FOldBaseLayout := GetDefaultKeyboardID;
    end
    else
      FOldBaseLayout := GetDefaultKeyboardID;
  finally
    Free;
  end;

  FInternalOptions.Save(Context);

  if FOldBaseLayout <> Get_Items('koBaseLayout').Value then
    for I := 0 to Context.Keyboards.Count - 1 do   // I4169
      (Context.Keyboards.Items[I] as IIntKeymanKeyboardInstalled).UpdateBaseLayout;

  Context.Control.AutoApplyKeyman;
end;

procedure TKeymanOptions.DoRefresh;
var
  i: TUtilKeymanOption;
  v: IKeymanOption;
begin
  FKeymanOptions.Clear;
  FInternalOptions.Load;
  for i := Low(TUtilKeymanOption) to High(TUtilKeymanOption) do
    if FInternalOptions.Option[i] <> nil then
      FKeymanOptions.Add(TKeymanOption.Create(Context, FInternalOptions.Option[i]));

  v := Get_Items('koBaseLayout');
  if v.Value = v.DefaultValue then
    v.Value := GetDefaultKeyboardID;
end;

end.
