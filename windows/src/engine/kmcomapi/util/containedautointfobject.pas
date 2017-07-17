(*
  Name:             containedautointfobject
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    4 Dec 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Initial version
*)
unit containedautointfobject;

interface

uses
  ActiveX, ComObj;

type
  TContainedAutoIntfObject = class(TContainedObject, IDispatch, ISupportErrorInfo)
  private
    FDispTypeInfo: ITypeInfo;
    FDispIntfEntry: PInterfaceEntry;
    FDispIID: TGUID;
  protected
    { IDispatch }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    { ISupportErrorInfo }
    function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
  public
    constructor Create(const Controller: IInterface; const TypeLib: ITypeLib; const DispIntf: TGUID);
{$IFDEF MSWINDOWS}
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; override;
{$ENDIF}
    property DispIntfEntry: PInterfaceEntry read FDispIntfEntry;
    property DispTypeInfo: ITypeInfo read FDispTypeInfo;
    property DispIID: TGUID read FDispIID;
  end;
implementation

uses Windows;

{ TContainedAutoIntfObject }

constructor TContainedAutoIntfObject.Create(const Controller: IInterface; const TypeLib: ITypeLib; const DispIntf: TGUID);
begin
  inherited Create(Controller);
  OleCheck(TypeLib.GetTypeInfoOfGuid(DispIntf, FDispTypeInfo));
  FDispIntfEntry := GetInterfaceEntry(DispIntf);
end;

{ TContainedAutoIntfObject.IDispatch }

function TContainedAutoIntfObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
{$IFDEF MSWINDOWS}
  Result := DispGetIDsOfNames(FDispTypeInfo, Names, NameCount, DispIDs);
{$ENDIF}
{$IFDEF LINUX}
  Result := E_NOTIMPL;
{$ENDIF}
end;

function TContainedAutoIntfObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  if Index <> 0 then
  begin
    Result := DISP_E_BADINDEX;
    Exit;
  end;
  ITypeInfo(TypeInfo) := FDispTypeInfo;
  Result := S_OK;
end;

function TContainedAutoIntfObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 1;
  Result := S_OK;
end;

function TContainedAutoIntfObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
const
  INVOKE_PROPERTYSET = INVOKE_PROPERTYPUT or INVOKE_PROPERTYPUTREF;
begin
  if Flags and INVOKE_PROPERTYSET <> 0 then Flags := INVOKE_PROPERTYSET;
  Result := FDispTypeInfo.Invoke(Pointer(Integer(Self) +
    FDispIntfEntry.IOffset), DispID, Flags, TDispParams(Params), VarResult,
    ExcepInfo, ArgErr);
end;

function TContainedAutoIntfObject.InterfaceSupportsErrorInfo(const iid: TIID): HResult;
begin
  if IsEqualGUID(DispIID, iid) then
    Result := S_OK else
    Result := S_FALSE;
end;

{$IFDEF MSWINDOWS}
function TContainedAutoIntfObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, DispIID, '', '');
end;
{$ENDIF}

end.
