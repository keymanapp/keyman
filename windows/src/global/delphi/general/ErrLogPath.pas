(*
  Name:             ErrLogPath
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      28 Jul 2008

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          28 Jul 2008 - mcdurdin - I1574 - Initial version
                    18 Mar 2011 - mcdurdin - I2768 - Fix crash in installer when Diag folder does not exist
                    18 Mar 2011 - mcdurdin - I2824 - Consolidate logging of diagnostics to Diag folder in appdata
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
*)
unit ErrLogPath;

interface

function GetErrLogPath: string;
function GetErrLogFileName(app: string): string;

implementation

uses
  Winapi.ShlObj,
  Winapi.Windows,
  System.SysUtils,
  RegistryKeys,
  utilsystem;

function GetErrLogPath: string;
begin
  Result := GetFolderPath(CSIDL_LOCAL_APPDATA) + SFolderKeymanEngineDiag + '\';
  ForceDirectories(Result);  // I2768
end;

function GetErrLogFileName(app: string): string;  // I2824
begin
  Result := GetErrLogPath + app + '-' + IntToStr(GetCurrentProcessId) + '.log';
end;

end.
