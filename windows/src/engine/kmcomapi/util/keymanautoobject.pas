(*
  Name:             keymanautoobject
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    25 Mar 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add SerializeXML and UniqueIndex functionality
                    04 Dec 2006 - mcdurdin - Objects are contained by TAutoIntfObject
                    19 Mar 2007 - mcdurdin - I701 - fix leaked COM objects
                    23 Aug 2007 - mcdurdin - Add ClearErrors function
                    28 Jul 2008 - mcdurdin - I1574 - remove SafecallException exception reporting and use new global reporting
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
*)
unit keymanautoobject;

interface

uses
  Windows, ComObj, ActiveX, keymanapi_TLB, Classes, Contnrs,
  internalinterfaces, keymancontext;

type
  IEnumVariant = interface(IUnknown)
    ['{00020404-0000-0000-C000-000000000046}']
    function Next(celt: LongWord; out elt; pceltFetched: PLongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  end;

  TKeymanAutoObject = class;
  TKeymanAutoCollectionObject = class;
  TAutoObjectList = class;

  TKeymanEnumerator = class(TInterfacedObject, IEnumVariant)
  private
    FEnumPos: Integer;
    FCollection: TKeymanAutoCollectionObject;
  protected
    function Next(celt: LongWord; out elt;
      pceltFetched: PLongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  public
    constructor Create(ACollection: TKeymanAutoCollectionObject);
  end;

  TKeymanAutoObject = class(TAutoIntfObject, IKeymanObject, IIntKeymanInterface)
  private
    FContext: TKeymanContext;
  protected
    procedure Error(ErrorCode: Cardinal);
    procedure ErrorFmt(ErrorCode: Cardinal; Args: OleVariant);
    procedure ClearErrors;
    property Context: TKeymanContext read FContext;
    procedure _SetContext(AContext: TKeymanContext);

    { IKeymanObject }
    function SerializeXML(Flags: TOleEnum; const ImagePath: WideString; out References: OleVariant): WideString; safecall;

    { IIntKeymanInterface }
    function XMLClassName: WideString; virtual;
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString; virtual;
    function DoSerialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;

  public
    constructor Create(AContext: TKeymanContext; intf: TGUID);
    destructor Destroy; override;
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  TKeymanAutoCollectionObject = class(TKeymanAutoObject, IIntKeymanCollection, IKeymanCollection)
  private
    FList: TAutoObjectList;
    function GetItem(Index: Integer): IIntKeymanInterface;
    function NewEnum: IUnknown;
  protected
    procedure DoRefresh; virtual;
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString; override;

    { IKeymanCollection }
    procedure Refresh; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;

  public
    constructor Create(AContext: TKeymanContext; intf: TGUID; AList: TAutoObjectList);

    { IIntKeymanCollection }
    function Count: Integer;
    function GetIntItem(Index: Integer): IIntKeymanInterface;
    property Items[Index: Integer]: IIntKeymanInterface read GetItem;
  end;

  TAutoObjectList = class(TInterfaceList)
  private

  protected
    function GetItem(Index: Integer): IIntKeymanInterface;
    procedure SetItem(Index: Integer; AObject: IIntKeymanInterface);

  public

    function Add(AObject: IIntKeymanInterface): Integer;
    function Remove(AObject: IIntKeymanInterface): Integer;
    function IndexOf(AObject: IIntKeymanInterface): Integer;

    procedure Insert(Index: Integer; AObject: IIntKeymanInterface);

    property Items[Index: Integer]: IIntKeymanInterface read GetItem write SetItem; default;
  end;

implementation

uses
  comserv,
  exception_keyman,
  KLog,
  SysUtils,
  utilhandleexception,
  Variants;

procedure TKeymanAutoObject.ClearErrors;
begin
  FContext.Errors.DoClear;
end;

constructor TKeymanAutoObject.Create(AContext: TKeymanContext; intf: TGUID);
begin
  KL.Log( IntToHex(Integer(Self), 8) +': '+ClassName+'.Create');
  Assert(AContext is TKeymanContext);
  FContext := AContext;
  inherited Create(ComServer.TypeLib, intf);
end;

destructor TKeymanAutoObject.Destroy;
begin
  KL.Log(IntToHex(Integer(Self), 8) +': '+ClassName+'.Destroy');
  inherited Destroy;
end;

function TKeymanAutoObject.DoSerialize(Flags: TOleEnum;
  const ImagePath: WideString; References: TStrings): WideString;
begin
  Result := '<'+XMLClassName+'>'+Serialize(Flags, ImagePath, References)+'</'+XMLClassName+'>';
end;

procedure TKeymanAutoObject.Error(ErrorCode: Cardinal);
begin
  (FContext as TKeymanContext).Errors.Add(ErrorCode, kesError);
end;

procedure TKeymanAutoObject.ErrorFmt(ErrorCode: Cardinal; Args: OleVariant);
begin
  (FContext as TKeymanContext).Errors.AddFmt(ErrorCode, Args, kesError);
end;

function TKeymanAutoObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
{.$IFDEF KLOG}
  if (ExceptObject is Exception) then //and not (ExceptObject is EOleException) { for some reason, we lose the class name EKeyman? } then
    LogException(ClassName, ExceptObject as Exception, ExceptAddr);
{.$ENDIF}
  Result := inherited SafeCallException(ExceptObject, ExceptAddr);
end;

function TKeymanAutoObject.Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString;
begin
  // virtual base method
  Result := '';
end;

function TKeymanAutoObject.SerializeXML(Flags: TOleEnum;
  const ImagePath: WideString; out References: OleVariant): WideString;

      function StringListToVariantArray(str: TStrings): OleVariant;
      var
        i: Integer;
        p: POleVariant;
      begin
        Result := VarArrayCreate([1,str.Count], varVariant);
        p := POleVariant(VarArrayLock(Result));
        for i := 0 to str.Count - 1 do
        begin
          p^ := str[i];
          Inc(p);
        end;
        VarArrayUnlock(Result);
      end;

var
  FStrings: TStrings;
begin
  try
    if Flags and ksfExportImages = ksfExportImages then
    begin
      FStrings := TStringList.Create;
      try
        Result := DoSerialize(Flags, ImagePath, FStrings);
        References := StringListToVariantArray(FStrings);
      finally
        FStrings.Free;
      end;
    end
    else
    begin
      Result := DoSerialize(Flags, '', nil);
    end;
  except
    on E:Exception do
    begin
      LogException(E);
    end;
  end;
end;

function TKeymanAutoObject.XMLClassName: WideString;
begin
  Result := ClassName;
  Delete(Result,1,1);
end;

procedure TKeymanAutoObject._SetContext(AContext: TKeymanContext);
begin
  FContext := AContext;
end;

{ TKeymanAutoCollectionObject }

function TKeymanAutoCollectionObject.Get_Count: Integer;
begin
  Result := FList.Count;
end;

function TKeymanAutoCollectionObject.GetIntItem(Index: Integer): IIntKeymanInterface;
begin
  Result := FList[Index];
end;

function TKeymanAutoCollectionObject.GetItem(Index: Integer): IIntKeymanInterface;
begin
  Result := FList[Index];
end;

function TKeymanAutoCollectionObject.Get__NewEnum: IUnknown;
begin
  Result := NewEnum;
end;

function TKeymanAutoCollectionObject.NewEnum: IUnknown;
begin
  Result := TKeymanEnumerator.Create(Self);
end;

procedure TKeymanAutoCollectionObject.Refresh;
begin
  DoRefresh;
end;

function TKeymanAutoCollectionObject.Serialize(Flags: TOleEnum;
  const ImagePath: WideString; References: TStrings): WideString;
var
  i: Integer;
  kao: IIntKeymanInterface;
begin
  Result := '';
  for i := 0 to Count-1 do
  begin
    kao := Items[i] as IIntKeymanInterface;
    Result := Result +
      '<'+kao.XMLClassName+'>'+kao.Serialize(Flags, ImagePath, References)+'</'+kao.XMLClassName+'>';
    {<index>'+IntToStr((kao as IKeymanObject).Get_UniqueIndex)+'</index>'+}

  end
end;

function TKeymanAutoCollectionObject.Count: Integer;
begin
  Result := Get_Count;
end;

constructor TKeymanAutoCollectionObject.Create(AContext: TKeymanContext; intf: TGUID; AList: TAutoObjectList);
begin
  inherited Create(AContext, intf);
  FList := AList;
end;

procedure TKeymanAutoCollectionObject.DoRefresh;
begin
end;

{ TAutoObjectList }

function TAutoObjectList.Add(AObject: IIntKeymanInterface): Integer;
begin
  Result := inherited Add(AObject);
end;

function TAutoObjectList.GetItem(Index: Integer): IIntKeymanInterface;
begin
  Result := inherited Items[Index] as IIntKeymanInterface;
end;

function TAutoObjectList.IndexOf(AObject: IIntKeymanInterface): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

procedure TAutoObjectList.Insert(Index: Integer; AObject: IIntKeymanInterface);
begin
  inherited Insert(Index, AObject);
end;

function TAutoObjectList.Remove(AObject: IIntKeymanInterface): Integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TAutoObjectList.SetItem(Index: Integer; AObject: IIntKeymanInterface);
begin
  inherited Items[Index] := AObject;
end;

{ TKeymanEnumerator }

constructor TKeymanEnumerator.Create(ACollection: TKeymanAutoCollectionObject);
begin
  inherited Create;
  FCollection := ACollection;
end;

function TKeymanEnumerator.Clone(out Enum: IEnumVariant): HResult;
var
  Enumerator: TKeymanEnumerator;
begin
  Result := S_OK;
  try
    Enumerator := TKeymanEnumerator.Create(FCollection);
    Enumerator.FEnumPos := FEnumPos;
    Enum := Enumerator;
  except
    Enum := nil;
    Result := E_OUTOFMEMORY;
  end;
end;


function TKeymanEnumerator.Next(celt: LongWord; out elt; pceltFetched: PLongWord): HResult;
var
  I: Integer;
  disp: IDispatch;
begin
  Result := S_FALSE;
  try
    if Assigned(pceltFetched) then pceltFetched^ := 0;
    for I := 0 to celt - 1 do
    begin
      if FEnumPos >= FCollection.Count then Exit;
      disp := FCollection.FList[FEnumPos] as IDispatch;
      disp._AddRef;
      TVariantArgList(elt)[I].vt := varDispatch;
      TVariantArgList(elt)[I].dispVal := Pointer(disp);
      Inc(FEnumPos);
      if Assigned(pceltFetched) then Inc(pceltFetched^);
    end;
  except
    // hide any errors
  end;
  if (not Assigned(pceltFetched)) or (Assigned(pceltFetched) and (pceltFetched^ = celt)) then
    Result := S_OK;
end;

function TKeymanEnumerator.Reset: HResult;
begin
  FEnumPos := 0;
  Result := S_OK;
end;

function TKeymanEnumerator.Skip(celt: LongWord): HResult;
begin
  if FEnumPos + Integer(celt) > FCollection.Count - 1 then
  begin
    FEnumPos := FCollection.Count - 1;
    Result := S_FALSE;
  end
  else
  begin
    Inc(FEnumPos, celt);
    Result := S_OK;
  end;
end;

//initialization
//  FGlobalObjects := TList.Create;
//finalization
//  FreeAndNil(FGlobalObjects);
end.
