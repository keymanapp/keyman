(*
  Name:             KeymanEmbeddedWB
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      18 Feb 2011

  Modified Date:    8 Jul 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    18 Mar 2011 - mcdurdin - I2784 - Integrated browser has leak of IInternetSecurityManager
                    08 Jul 2011 - mcdurdin - I2984 - Disable floating point exceptions to avoid throwing fp errors from mshtml.dll
*)
unit KeymanEmbeddedWB;  // I2721

interface

uses
  Classes,
  EmbeddedWB,
  SecurityManager,
  URLMon;

type
  TKeymanEmbeddedWB = class(TEmbeddedWB)
  private
    FSecurityManager: TSecurityManager; // I2784
  protected
    function DoQueryService(const rsid, iid: TGUID; var Obj): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override; // I2784
  end;

procedure Register;

implementation

uses
  SysUtils;

procedure Register;
begin
  RegisterComponents('Keyman', [TKeymanEmbeddedWB]);
end;

{ TKeymanEmbeddedWB }

constructor TKeymanEmbeddedWB.Create(AOwner: TComponent);
begin
  FSecurityManager := TSecurityManager.Create(nil);
  inherited Create(AOwner);
  DisableErrors.fpExceptions := False;  // I2984
end;

destructor TKeymanEmbeddedWB.Destroy;
begin
  FreeAndNil(FSecurityManager);  // I2784
  inherited Destroy;
end;

function TKeymanEmbeddedWB.DoQueryService(const rsid, iid: TGUID;
  var Obj): Boolean;
begin
  if Assigned(FSecurityManager) and IsEqualGuid(rsid, IID_IInternetSecurityManager) then  // I2784
  begin
    Result := FSecurityManager.QueryInterface(iid, Obj) = S_OK;
  end
  else
    Result := inherited DoQueryService(rsid, iid, Obj);
end;

initialization
  RegisterClasses([TEmbeddedWB]);
end.
