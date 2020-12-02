(*
  Name:             keymansysteminfo
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    26 Dec 2013
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Add Serialize, EngineInstallPath, EngineVersion
                    04 Dec 2006 - mcdurdin - Add RunDiagnostics function
                    15 Jan 2007 - mcdurdin - Fix call to tsysinfo.exe (quoted string)
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    26 Dec 2013 - mcdurdin - I4013 - V9.0 - Merge of I3911 - Keyman Desktop Support page shows wrong Engine version
*)
unit keymansysteminfo;

interface

uses
  ComObj, ActiveX, keymanapi_TLB, internalinterfaces, keymanautoobject, KeymanContext, StdVcl, Classes, utilxml;

type
  TKeymanSystemInfo = class(TkeymanAutoObject, IIntKeymanSystemInfo, IKeymanSystemInfo)
  private
    FWantReboot: Boolean;
    function ModuleFileName: string;
  protected
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString; override;

    { IKeymanSystemInfo }
    function Get_EngineInstallPath: WideString; safecall;
    function Get_EngineVersion: WideString; safecall;
    function Get_IsAdministrator: WordBool; safecall;
    function Get_RebootRequired: WordBool; safecall;

    procedure SetReboot; safecall;
  public
    constructor Create(AContext: TKeymanContext);
  end;

implementation

uses
  SysUtils,
  ComServ,
  isadmin,
  KeymanPaths,
  KLog,
  utilsystem,
  VersionInfo,
  Windows;

constructor TKeymanSystemInfo.Create(AContext: TKeymanContext);
begin
  inherited Create(AContext, IKeymanSystemInfo);
  FWantReboot := False;
end;

function TKeymanSystemInfo.Get_IsAdministrator: WordBool;
begin
  Result := isadmin.IsAdministrator;
end;

function TKeymanSystemInfo.ModuleFileName: string;   // I4013
var
  buf: array[0..260] of char;
begin
  GetModuleFileName(hInstance, buf, 260);
  buf[259] := #0;
  Result := buf;
end;

function TKeymanSystemInfo.Serialize(Flags: TOleEnum; const ImagePath: WideString; 
  References: TStrings): WideString;
begin
  Result := XMLFormat([
    'isadministrator', Get_IsAdministrator,
    'engineversion', Get_EngineVersion,
    'engineinstallpath', Get_EngineInstallPath
  ]);
end;

function TKeymanSystemInfo.Get_EngineInstallPath: WideString;
begin
  Result := TKeymanPaths.KeymanEngineInstallPath;
end;

function TKeymanSystemInfo.Get_EngineVersion: WideString;
begin
  Result := GetFileVersionString(ModuleFileName);   // I4013
end;

procedure TKeymanSystemInfo.SetReboot;
begin
  KL.Log('TKeymanErrors.SetReboot is no longer supported.');
end;

function TKeymanSystemInfo.Get_RebootRequired: WordBool;
begin
  KL.Log('TKeymanErrors.RebootRequired always returns False.');
  Result := False;
end;

end.
