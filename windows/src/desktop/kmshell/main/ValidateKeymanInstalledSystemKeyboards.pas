(*
  Name:             ValidateKeymanInstalledSystemKeyboards
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      13 Jul 2007

  Modified Date:    1 Dec 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 Jul 2007 - mcdurdin - I838 - Initial version
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
                    01 Dec 2012 - mcdurdin - I3613 - V9.0 - System shadow keyboards obsolete, strip out remaining code
*)
unit ValidateKeymanInstalledSystemKeyboards;

interface

procedure DeleteLegacyKeymanInstalledSystemKeyboards;   // I3613

implementation

uses
  Windows,
  Classes,
  ErrorControlledRegistry,
  KeymanVersion,
  RegistryKeys,
  SysUtils;

procedure DeleteLegacyKeymanInstalledSystemKeyboards;   // I3613
var
  Keys: TStringList;
  I: Integer;
begin
  Keys := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKey(SRegKey_KeyboardLayouts_LM, true) then Exit;

    GetKeyNames(Keys);

    for I := 0 to Keys.Count - 1 do
      if SameText(Copy(Keys[i], 6, 3), '5FE') and
        OpenKey('\'+SRegKey_KeyboardLayouts_LM+'\'+Keys[i], True) and
        ValueExists(SRegValue_Legacy_KeyboardKeymanInstall) then
      begin
        if OpenKey('\'+SRegKey_KeyboardLayouts_LM, True) then  // I2890
          DeleteKey(Keys[i]);
      end;

  finally
    Free;
    Keys.Free;
  end;
end;

end.
