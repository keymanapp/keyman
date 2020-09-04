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
function GetBCP47ForTransientTIP(LangID: Integer; guidProfile: TGUID): string;
function IsTransientLanguageID(LangID: Integer): Boolean;
function GetLayoutInstallString(LangID: Integer; guidProfile: TGUID): string;

const c_clsidKMTipTextService: TGUID = '{FE0420F1-38D1-4B4C-96BF-E7E20A74CFB7}';  // version 10.0
const c_clsidKMTipTextService_90: TGUID = '{487EB753-DB93-48C5-9E6A-4398E777C61D}';   // I3663   // I4248
const c_clsidTextServicesFramework: TGUID = '{529A9E6B-6587-4F23-AB9E-9C7D683E3C50}';

implementation

uses
  System.Classes,
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

function GetLayoutInstallString(LangID: Integer; guidProfile: TGUID): string;
begin
  Result := Format('%04.4x:%s%s', [
    LangID,
    GuidToString(c_clsidKMTipTextService),   // I4244
    GuidToString(guidProfile)
  ]);
end;

function IsTIPInstalledForCurrentUser(BCP47Tag: string; LangID: Integer; guidProfile: TGUID): Boolean;
begin
  Result := (LangID <> 0) and (GetBCP47ForTransientTIP(LangID, guidProfile) <> '');
end;

function GetBCP47ForTransientTIP(LangID: Integer; guidProfile: TGUID): string;
var
  tag, FLayoutInstallString: string;
  reg: TRegistryErrorControlled;
  tags: TStringList;
begin
  Result := '';

  // TODO: read this data once only at start of load of languages rather than
  // once every language possibility
  reg := TRegistryErrorControlled.Create(KEY_READ);
  try
    FLayoutInstallString := GetLayoutInstallString(LangID, guidProfile);

    // We'll search all the installed tags because it's probably a transient
    // language for which we rely on the Windows language association because
    // it can change on us
    if reg.OpenKeyReadOnly(SRegKey_ControlPanelInternationalUserProfile) then
    begin
      tags := TStringList.Create;
      try
        reg.GetKeyNames(tags);
        for tag in tags do
        begin
          if reg.OpenKeyReadOnly('\' + SRegKey_ControlPanelInternationalUserProfile + '\' + tag) and
              reg.ValueExists(FLayoutInstallString) then
            Exit(tag);
        end;
      finally
        tags.Free;
      end;
    end;
  finally
    reg.Free;
  end;
end;

function IsTransientLanguageID(LangID: Integer): Boolean;
begin
  Result :=
    (LangID = $2000) or
    (LangID = $2400) or
    (LangID = $2800) or
    (LangID = $2C00);
end;

end.
