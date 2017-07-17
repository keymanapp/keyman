(*
  Name:             keymanerrors
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Aug 2007

  Modified Date:    4 Nov 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Aug 2007 - mcdurdin - Initial version
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    02 Feb 2012 - mcdurdin - I3183 - Debug KMCOMAPI errors with extra Windows error info
                    04 Nov 2012 - mcdurdin - I3530 - V9.0 - Merge of I3183 - Report clearer error when failure to register controller window
*)
unit keymanerrors;

interface

uses
  ComObj, ActiveX, keymanapi_TLB, keymanautoobject, keymancontext, keymanerror, StdVcl, internalinterfaces;

type
  TErrorList = class(TAutoObjectList)
  private
    function GetItem(Index: Integer): IIntKeymanError;
    procedure SetItem(Index: Integer; const Value: IIntKeymanError);
  public
    property Items[Index: Integer]: IIntKeymanError read GetItem write SetItem; default;
  end;

  TKeymanErrors = class(TKeymanAutoCollectionObject, IIntKeymanErrors, IKeymanErrors)
  private
    FErrors: TErrorList;
  protected
    { IKeymanErrors }
    function Get_Items(Index: Integer): IKeymanError; safecall;
    procedure Clear; safecall;

    { IIntKeymanErrors  }
    procedure DoClear;
    procedure Add(ErrorCode: Cardinal; Severity: KeymanErrorSeverity); safecall;
    procedure AddFmt(ErrorCode: Cardinal; Parameters: OleVariant; Severity: KeymanErrorSeverity); safecall;
  public
    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  System.Win.ComServ,
  Winapi.Windows,

  exception_keyman,
  keymanerrorcodes,
  klog,
  utilvararray;

{ TKeymanErrors }

constructor TKeymanErrors.Create(AContext: TKeymanContext);
begin
  FErrors := TErrorList.Create;
  inherited Create(AContext, IKeymanErrors, FErrors);
end;

destructor TKeymanErrors.Destroy;
begin
  FErrors.Free;
  inherited Destroy;
end;

procedure TKeymanErrors.DoClear;
begin
  Clear;
end;

function TKeymanErrors.Get_Items(Index: Integer): IKeymanError;
begin
  if (Index < 0) or (Index >= FErrors.Count) then
    ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([VarToStr(Index)]));
  Result := FErrors[Index] as IKeymanError;
end;

procedure TKeymanErrors.Add(ErrorCode: Cardinal; Severity: KeymanErrorSeverity);
var
  FLastError: Cardinal;
  FWindowsMessage: string;
begin
  FLastError := GetLastError;  // I3183   // I3530
  if FLastError = ERROR_SUCCESS then FWindowsMessage := ''  // I3183   // I3530
  else FWindowsMessage := SysErrorMessage(FLastError)+', '+IntToStr(FLastError);  // I3183   // I3530

  FErrors.Add(TKeymanError.Create(Context, ErrorCode, Severity));
  with GetKeymanErrorMessage('(unknown)', ErrorCode) do
    kl.Log('TKeymanErrors.Add: '+Message);
  if Severity in [kesFatal, kesError] then raise EKeyman.Create('(unknown)', FWindowsMessage, ErrorCode);  // I3183   // I3530
end;

procedure TKeymanErrors.AddFmt(ErrorCode: Cardinal; Parameters: OleVariant; Severity: KeymanErrorSeverity);
var
  FLastError: Cardinal;
  FWindowsMessage: string;
begin
  FLastError := GetLastError;  // I3183   // I3530
  if FLastError = ERROR_SUCCESS then FWindowsMessage := ''  // I3183   // I3530
  else FWindowsMessage := SysErrorMessage(FLastError)+', '+IntToStr(FLastError);  // I3183   // I3530

  FErrors.Add(TKeymanError.Create(Context, ErrorCode, Parameters, Severity));
  with GetKeymanErrorMessage('(unknown)', ErrorCode) do
    kl.Log('TKeymanErrors.AddFmt: '+FormatVariant(Message, Parameters));
  if Severity in [kesFatal, kesError] then raise EKeyman.CreateFmt('(unknown)', FWindowsMessage, ErrorCode, Parameters);  // I3183   // I3530
end;

{ TErrorList }

function TErrorList.GetItem(Index: Integer): IIntKeymanError;
begin
  Result := inherited GetItem(Index) as IIntKeymanError;
end;

procedure TErrorList.SetItem(Index: Integer; const Value: IIntKeymanError);
begin
  inherited SetItem(Index, Value);
end;

procedure TKeymanErrors.Clear;
begin
  FErrors.Clear;  
end;

end.
