//*************************************************************
//                         EwbClasses                         *
//                                                            *
//                      Freeware unit                         *
//                     For Delphi 5 to XE                     *
//                            by                              *
//                     Serge Voloshenyuk                      *
//      Developing Team:                                      *
//          Serge Voloshenyuk (SergeV@bsalsa.com)             *
//          Eran Bodankin (bsalsa) -(bsalsa@gmail.com)        *
//                                                            *
//       Documentation and updated versions:                  *
//                                                            *
//               http://www.bsalsa.com                        *
//*************************************************************
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use/ change/ modify the component under 3 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit  for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}
//$Id: EwbClasses.pas,v 1.1.2.1 2006/11/29 22:13:01 sergev Exp $

unit EwbClasses;

interface

{$I EWB.inc}

uses
  Windows;

type
  IInterfaceObjectReference = interface
    ['{C0D28425-8410-45F6-BD60-8AE66782E545}']
    function GetObject: TObject;
  end;

  TInterfacedDispatchObject = class(TObject, IUnknown,
      IInterfaceObjectReference, IDispatch)
  protected
    {IInterface}
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult;
{$IFDEF RESEARCH_MODE} virtual; {$ENDIF} stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {IInterfaceObjectReference}
    function GetObject: TObject;
    {IDispatch}
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; virtual; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
      virtual; stdcall;
  public
    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

implementation

{ TInterfacedDispatchObject }

class function TInterfacedDispatchObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedDispatchObject(Result).FRefCount := 1;
end;

procedure TInterfacedDispatchObject.AfterConstruction;
begin
  InterlockedDecrement(FRefCount);
end;

function TInterfacedDispatchObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedDispatchObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TInterfacedDispatchObject.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedDispatchObject.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := DISP_E_UNKNOWNNAME;
end;

function TInterfacedDispatchObject.GetObject: TObject;
begin
  Result := Self;
end;

function TInterfacedDispatchObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := DISP_E_BADINDEX;
end;

function TInterfacedDispatchObject.GetTypeInfoCount(
  out Count: Integer): HResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TInterfacedDispatchObject.Invoke(DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult,
  ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := S_OK;
end;

end.
