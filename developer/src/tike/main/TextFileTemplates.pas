(*
  Name:             TextFileTemplates
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Jun 2008

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Jun 2008 - mcdurdin - I1423 - Initial version
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    26 Jun 2012 - mcdurdin - I3379 - KM9 - Remove old Winapi references now in Delphi libraries
*)
unit TextFileTemplates;  // I3306

interface

function GetXMLTemplate: WideString;
function GetHTMLTemplate: WideString;

implementation

uses
  Winapi.ShlObj,
  Classes,
  RegistryKeys,
  SysUtils,
  utilsystem;

const
  SXMLTemplate: WideString =
    '<?xml version="1.0" encoding="utf-8"?>'#13#10#13#10;

  SHTMLTemplate: WideString =
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'#13#10+
    '<html xmlns="http://www.w3.org/1999/xhtml" >'#13#10+
    '<head>'#13#10+
    '    <title>Untitled Page</title>'#13#10+
    '</head>'#13#10+
    '<body>'#13#10+
    ''#13#10+
    '</body>'#13#10+
    '</html>';

function GetTextFileTemplatePath: WideString;
begin
  Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloperTemplates + '\';
end;

function GetTextFileTemplate(const BaseFileName, DefaultTemplate: WideString): WideString;
var
  FFileName: WideString;
begin
  try
    FFileName := GetTextFileTemplatePath + BaseFileName;
    if not FileExists(FFileName) then
    begin
      ForceDirectories(GetTextFileTemplatePath);
      with TStringList.Create do
      try
        Text := DefaultTemplate;
        SaveToFile(FFileName, TEncoding.UTF8);  // I3337
      finally
        Free;
      end;
      Result := DefaultTemplate;
    end
    else
    begin
      with TStringList.Create do
      try
        LoadFromFile(FFileName);  // prolog determines encoding
        Result := Text;
      finally
        Free;
      end;
    end;
  except
    Result := DefaultTemplate;
  end;
end;

function GetXMLTemplate: WideString;
begin
  Result := GetTextFileTemplate('default.xml', SXMLTemplate);
end;

function GetHTMLTemplate: WideString;
begin
  Result := GetTextFileTemplate('default.html', SHTMLTemplate);
end;

end.
