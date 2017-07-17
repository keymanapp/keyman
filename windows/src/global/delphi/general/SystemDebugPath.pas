(*
  Name:             SystemDebugPath
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Initial version
                    19 Jun 2007 - mcdurdin - Widestrings
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
*)
unit SystemDebugPath;  // I3306

interface

function GetSystemDebugPath: WideString;

implementation

uses
  ActiveX, ShellApi, ShlObj, ErrorControlledRegistry, SysUtils, Windows;

function GetFolderPath(csidl: Integer): WideString;
var
  buf: array[0..260] of WideChar;
  idl: PItemIDList;
  mm: IMalloc;
begin
  Result := '';
  if SHGetMalloc(mm) = NOERROR then
  begin
    if SHGetSpecialFolderLocation(0, csidl, idl) = NOERROR then
    begin
      if SHGetPathFromIDList(idl, buf) then
      begin
        Result := Buf;
      end;
      mm.Free(idl);
    end;
    mm._Release;
  end;

  if (Result = '') and (csidl = CSIDL_PROGRAM_FILES) then
    with TRegistryErrorControlled.Create do  // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if not OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion') then  // I2890
        RaiseLastRegistryError;
      Result := ReadString('ProgramFilesDir');
    finally
      Free;
    end;
  if Result <> '' then Result := IncludeTrailingPathDelimiter(Result);
    if Result[Length(Result)] <> '\' then Result := Result + '\';
end;

function GetSystemDebugPath: WideString;
begin
  Result := GetFolderPath(CSIDL_DESKTOPDIRECTORY) + 'keymanlog';
  if not DirectoryExists(Result) then CreateDir(Result); // assumes write permissions!
  Result := Result + '\';
end;

end.
