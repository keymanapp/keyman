(*
  Name:             XMLTemplatePath
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      27 Mar 2008

  Modified Date:    27 Mar 2008
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          27 Mar 2008 - mcdurdin - Initial version
*)
unit XMLTemplatePath;

interface

function GetXMLTemplatePath(const FileName: WideString): WideString;

implementation

uses
  kmint, custinterfaces, TntSysUtils;

function OldXMLTemplatePath: WideString;
begin
  Result := WideExtractFilePath(ParamStr(0));// + 'xml\';
  if WideDirectoryExists(Result + 'xml') then Result := Result + 'xml\';
end;

function GetXMLTemplatePath(const FileName: WideString): WideString;
begin
  with ((FActiveProduct as IKeymanCustomisation).CustMessages as IKeymanCustomisationMessages2) do
  begin
    Result := GetLocalePathForLocale(LanguageCode);

    if (WideExtractFileName(Result) = 'strings.xml') and WideFileExists(WideExtractFilePath(Result) + '\'+FileName)
      then Result := WideExtractFilePath(Result) + '\'
      else Result := OldXMLTemplatePath;
  end;
end;

end.
