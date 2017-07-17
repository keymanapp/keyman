(*
  Name:             main_km_yim
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      15 Jan 2007

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          15 Jan 2007 - mcdurdin - Initial version
                    30 Apr 2007 - mcdurdin - Support Keyman Desktop 7.0
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit main_km_yim;

interface

procedure Run;

implementation

uses
  ErrorControlledRegistry,
  SysUtils,
  Windows;

resourcestring
  SAppTitle = 'Keyman Desktop Yahoo Messenger addin';
  SAppTitle60 = 'Keyman Yahoo Messenger addin';
  SAppTitle70 = 'Keyman Desktop Yahoo Messenger addin';
  SKeymanMissing = 'Unable to install the Keyman/Keyman Desktop Yahoo Messenger addin - Keyman 6.0 or Keyman Desktop 7.0, and the Keyman/Keyman Desktop RichEdit addin, are required but at least one is missing.';
  SRegistryError = 'Error %d accessing registry: %s';

  SInstalledOK60 = 'The Keyman Yahoo Messenger addin was successfully installed.  Don''t forget to restart Keyman before trying to use it!';
  SInstalledOK70 = 'The Keyman Desktop Yahoo Messenger addin was successfully installed.  Don''t forget to restart Keyman Desktop before trying to use it!';

procedure Error(const msg: string);
begin
  MessageBox(0, PChar(SKeymanMissing), PChar(SAppTitle), MB_ICONHAND or MB_OK);
  Halt(1);
end;

procedure Run;
type
  TAppFound = (afKeyman, afKeymanDesktop, afNone);
var
  szPath: string;
  AppFound: TAppFound;
begin
  AppFound := afNone;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey(SRegKey_Keyman60+'\add-ins\richedit', False) and ValueExists('addin file name') then
    begin
      szPath := ReadString('addin file name');
      if not OpenKey(SRegKey_Keyman60+'\add-ins', True) then
        Error(Format(SRegistryError, [GetLastError, SysErrorMessage(GetLastError)]));

      if (ParamStr(1) = '-c') and (ParamStr(2) <> '') then
        WriteString(ParamStr(2), szPath)
      else
        WriteString('YIMInputWindow', szPath);
      AppFound := afKeyman;
    end;

    if OpenKey(SRegKey_KeymanEngine80+'\add-ins\richedit', False) and ValueExists('addin file name') then
    begin
      szPath := ReadString('addin file name');
      if not OpenKey(SRegKey_KeymanEngine80+'\add-ins', False) then
        Error(Format(SRegistryError, [GetLastError, SysErrorMessage(GetLastError)]));

      if (ParamStr(1) = '-c') and (ParamStr(2) <> '') then
        WriteString(ParamStr(2), szPath)
      else
        WriteString('YIMInputWindow', szPath);
      AppFound := afKeymanDesktop;
    end;

    if AppFound = afNone then
      Error(SKeymanMissing);

  finally
    Free;
  end;

  if AppFound = afKeyman
    then MessageBox(0, PChar(SInstalledOK60), PChar(SAppTitle60), MB_ICONINFORMATION or MB_OK)
    else MessageBox(0, PChar(SInstalledOK70), PChar(SAppTitle70), MB_ICONINFORMATION or MB_OK);

  ExitCode := 0;
end;

end.
