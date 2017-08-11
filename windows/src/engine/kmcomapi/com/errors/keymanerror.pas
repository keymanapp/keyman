(*
  Name:             keymanerror
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    12 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add Serialize
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
*)
unit keymanerror;

interface

uses
  SysUtils, Contnrs, ComObj, ActiveX, keymanapi_TLB, internalinterfaces,
  keymancontext, keymanautoobject, keymanerrorcodes, StdVcl, Classes;

type
  TKeymanError = class(TKeymanAutoObject, IIntKeymanError, IKeymanError)
  private
    FErrorCode: Cardinal;
    FSeverity: KeymanErrorSeverity;
    FParameterVariant: OleVariant;
    FKeymanErrorInfo: TKeymanErrorInfo;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString;
      References: TStrings): WideString; override;
    { IKeymanError }
    function Get_Description: WideString; safecall;
    function Get_ErrorCode: Integer; safecall;
    function Get_Severity: Integer; safecall;
  public
    constructor Create(AContext: TKeymanContext; Value: Cardinal; Severity: KeymanErrorSeverity); reintroduce; overload;
    constructor Create(AContext: TKeymanContext; Value: Cardinal; Parameters: OleVariant; Severity: KeymanErrorSeverity); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

uses ComServ, Variants, exception_keyman, utilunicode, utilvararray, utilxml, klog;

constructor TKeymanError.Create(AContext: TKeymanContext; Value: Cardinal; Severity: KeymanErrorSeverity);
begin
  inherited Create(AContext, IKeymanError);
  FErrorCode := Value;
  FKeymanErrorInfo := GetKeymanErrorMessage('(unknown)', Value);
  FSeverity := Severity;

  if VarIsEmpty(FParameterVariant)
    then KL.LogError(FKeymanErrorInfo.Message)
    else KL.LogError(FormatVariant(FKeymanErrorInfo.Message, FParameterVariant));
end;

constructor TKeymanError.Create(AContext: TKeymanContext; Value: Cardinal; Parameters: OleVariant; Severity: KeymanErrorSeverity);
begin
  FParameterVariant := Parameters;
  Create(AContext, Value, Severity);
end;

destructor TKeymanError.Destroy;
begin
  inherited Destroy;
end;

function TKeymanError.Get_Description: WideString;
begin
  if VarIsEmpty(FParameterVariant)
    then Result := FKeymanErrorInfo.Message
    else Result := FormatVariant(FKeymanErrorInfo.Message, FParameterVariant);
end;

function TKeymanError.Get_ErrorCode: Integer;
begin
  Result := FErrorCode;
end;

function TKeymanError.Get_Severity: Integer;
begin
  Result := Ord(FSeverity);
end;

function TKeymanError.Serialize(Flags: TOleEnum; const ImagePath: WideString;
  References: TStrings): WideString;
begin
  Result := XMLFormat([
    'description', Get_Description,
    'errorcode', Get_ErrorCode,
    'severity', Get_Severity]);
end;

end.
