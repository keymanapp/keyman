(*
  Name:             RedistFiles
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    9 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add UnicodeDataSourcePath and GetXMLTemplatePath functions
                    14 Sep 2006 - mcdurdin - Add GetRedistDesktopPath, GetRedistUIPath and GetReditAddinsPath
                    28 Sep 2006 - mcdurdin - Refactor wtih GetDebugPath function
                    30 May 2007 - mcdurdin - I817 - Added Debug Redist Setup Path
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    08 Oct 2012 - mcdurdin - I3463 - V9.0 - Look up charmap root path from registry when starting Developer
                    11 Aug 2013 - mcdurdin - I3885 - V9.0 - Touch Layout Editor
                    09 Aug 2015 - mcdurdin - I4841 - Restructure version 9 developer help
*)
unit RedistFiles;

interface

type
  TRedistFile = record
    FileName: string;
    Description: string;
    IsEULAFile: boolean;
  end;

const
  CRuntimeFiles: array[0..0] of TRedistFile = (
    (FileName: 'keymandesktop.msi'; Description: 'Keyman Installer')
  );

  CRedistFiles: array[0..11] of TRedistFile = (
    (FileName: 'KeymanDesktop.chm'; Description: 'Keyman Help Files'),
    (FileName: 'keyman.exe'; Description: 'Keyman Program'),
    (FileName: 'keymanx64.exe'; Description: 'Keymanx64 Program'),
    (FileName: 'keyman32.dll'; Description: 'Keyman32 Library'),
    (FileName: 'keyman64.dll'; Description: 'keyman64 Library'),
    (FileName: 'kmcomapi.dll'; Description: 'Keyman COM API Library'),
    (FileName: 'kmtip.dll'; Description: 'kmtip Library'),
    (FileName: 'kmtip64.dll'; Description: 'kmtip64 Library'),
    (FileName: 'mcompile.dll'; Description: 'mcompile Program'),
    (FileName: 'tsysinfo.exe'; Description: 'System Information Executable'),

    (FileName: 'kmshell.exe'; Description: 'Keyman Configuration Application'),
    (FileName: 'license.rtf'; Description: 'Keyman EULA'; IsEULAFile: True)
  );

function GetRedistDesktopPath: string;
function GetHelpURL: string;   // I4841
function GetRedistUIPath: string;
function GetRedistAddinsPath: string;
function GetRedistSetupPath: string;
function GetRedistProjectTemplatePath: string;
function GetWixPath: string;
function GetStockKCTPath: string;
function GetUnicodeDataSourcePath(DefaultPath: string = ''): string;  // I3463
function GetXMLTemplatePath: string;
function GetDeveloperRootPath: string;
function GetLayoutBuilderPath: string;   // I3885

implementation

uses
  System.SysUtils,
  Winapi.Windows,

  DebugPaths,
  KeymanPaths,
  Upload_Settings,
  ErrorControlledRegistry,
  RegistryKeys;

function GetUnicodeDataSourcePath(DefaultPath: string): string;  // I3463
begin
  if DefaultPath = '' then  // I3463
    DefaultPath := ExtractFilePath(ParamStr(0));
  Result := GetDebugPath('Debug_UnicodeDataSourcePath', DefaultPath);
end;

function GetHelpURL: string;   // I4841
begin
  Result := GetDebugPath('Debug_HelpURL', MakeKeymanURL(URLPath_KeymanDeveloperDocumentation), False);
end;

function GetStockKCTPath: string;
begin
  Result := GetDebugPath('Debug_StockPath', ExtractFilePath(ParamStr(0)));
end;

function GetRedistUIPath: string;
begin
  Result := GetDebugPath('Debug_RedistUIPath', ExtractFilePath(ParamStr(0))+'redist\ui\');
end;

function GetRedistAddinsPath: string;
begin
  Result := GetDebugPath('Debug_RedistAddinsPath', ExtractFilePath(ParamStr(0))+'redist\addins\');
end;

function GetRedistDesktopPath: string;
begin
  Result := GetDebugPath('Debug_RedistDesktopPath', ExtractFilePath(ParamStr(0))+'redist\desktop\');
end;

function GetRedistSetupPath: string;
begin
  Result := GetDebugPath('Debug_RedistSetupPath', ExtractFilePath(ParamStr(0))+'redist\setup\');
end;

function GetWixPath: string;
begin
  Result := GetDebugPath('Debug_WixPath', ExtractFilePath(ParamStr(0))+'wix\');
end;

function GetRedistProjectTemplatePath: string;
var
  root: string;
const
  DevProjectTemplatePath = 'developer\src\kmconvert\data\';
begin
  if TKeymanPaths.RunningFromSource(root)
    then Result := root + DevProjectTemplatePath
    else Result := ExtractFilePath(ParamStr(0))+'projects\templates\';
  Result := GetDebugPath('Debug_RedistTemplatePath', Result);
end;

function GetXMLTemplatePath: string;
var
  root: string;
const
  DevTemplatePath = 'developer\src\tike\xml\';
begin
  if TKeymanPaths.RunningFromSource(root)
    then Result := root + DevTemplatePath
    else Result := ExtractFilePath(ParamStr(0))+'xml\';
  Result := GetDebugPath('Debug_XMLTemplatePath', Result);
end;


function GetLayoutBuilderPath: string;   // I3885
begin
  Result := GetXMLTemplatePath + 'layoutbuilder\';
end;

function GetDeveloperRootPath: string;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if not OpenKeyReadOnly(SRegKey_KeymanDeveloper_LM) then  // I2890
      RaiseLastRegistryError;
    Result := ReadString(SRegValue_RootPath);
    if Result = '' then
      raise Exception.Create('Unable to find the Keyman Developer directory.  '+
        'You should reinstall Keyman Developer.');
    if Result[Length(Result)] <> '\' then Result := Result + '\';
  finally
    Free;
  end;
end;

end.

