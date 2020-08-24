(*
  Name:             utiltsf
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    3 Jun 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Refactor util functions into multiple units
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    13 Dec 2012 - mcdurdin - I3663 - V9.0 - Uninstall of keyboard does not remove KMTIP entries
                    03 Jun 2014 - mcdurdin - I4248 - V9.0 - Refactor of kmtip
*)
unit utiltsf;

interface

function TSFInstalled: Boolean;
function IsTIPInstalledForCurrentUser(BCP47Tag: string; LangID: Integer; guidProfile: TGUID): Boolean;

const c_clsidKMTipTextService: TGUID = '{FE0420F1-38D1-4B4C-96BF-E7E20A74CFB7}';  // version 10.0
const c_clsidKMTipTextService_90: TGUID = '{487EB753-DB93-48C5-9E6A-4398E777C61D}';   // I3663   // I4248
const c_clsidTextServicesFramework: TGUID = '{529A9E6B-6587-4F23-AB9E-9C7D683E3C50}';

implementation

uses
  System.SysUtils,
  Winapi.Windows,

  ErrorControlledRegistry,
  RegistryKeys;

function TSFInstalled: Boolean;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CLASSES_ROOT;
    Result := OpenKeyReadOnly('CLSID\'+GUIDToString(c_clsidTextServicesFramework));
  finally
    Free;
  end;
end;

function IsTIPInstalledForCurrentUser(BCP47Tag: string; LangID: Integer; guidProfile: TGUID): Boolean;
var
  FLayoutInstallString: string;
  reg: TRegistryErrorControlled;
begin
  reg := TRegistryErrorControlled.Create(KEY_READ);
  try
    FLayoutInstallString := Format('%04.4x:%s%s', [LangID, GuidToString(c_clsidKMTipTextService),   // I4244
      GuidToString(guidProfile)]);

    Result := reg.OpenKeyReadOnly(SRegKey_ControlPanelInternationalUserProfile + '\' + BCP47Tag) and
      reg.ValueExists(FLayoutInstallString) and ((reg.ReadInteger(FLayoutInstallString) and 1) <> 0);
  finally
    reg.Free;
  end;
end;

end.
