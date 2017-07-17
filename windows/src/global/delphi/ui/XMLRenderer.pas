(*
  Name:             XMLRenderer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Oct 2006

  Modified Date:    1 May 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    06 Oct 2006 - mcdurdin - Move TemplatePath into separate function
                    04 Dec 2006 - mcdurdin - Read xml from locale folder
                    05 Dec 2006 - mcdurdin - Refactor rendering process into TXMLRenderers
                    12 Dec 2006 - mcdurdin - Clean up locale references
                    04 Jan 2007 - mcdurdin - Encode entities in XML render
                    19 Jun 2007 - mcdurdin - I899 - Translate XSLT within Keyman Desktop rather than relying on IE
                    13 Jul 2007 - mcdurdin - I902 - Resolve externals when loading XML files
                    23 Aug 2007 - mcdurdin - I956 - support locale install from packages
                    27 Mar 2008 - mcdurdin - Relocated
                    28 Aug 2008 - mcdurdin - I1607 - Fixup missing files
                    16 Jan 2009 - mcdurdin - I1668, I1594 - Fix crash when attempting to render XML when two copies of kmshell start simultaneously
                    16 Jan 2009 - mcdurdin - I1793 - Add shield icon to Keyman Configuration
                    20 Jan 2009 - mcdurdin - I1803 - Fix bug loading xml files when no debug path is provided
                    25 May 2010 - mcdurdin - I1694 - Select Keyman UI language rework
                    25 Jan 2011 - mcdurdin - I2612 - Language menu displays incorrectly in Splash, Config
                    18 Feb 2011 - mcdurdin - I2712 - SMP support for xml renderer
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    23 Mar 2012 - mcdurdin - I3269 - Add Debug_XMLRenderer DebugPath to allow writing temp .xml file
                    04 Nov 2012 - mcdurdin - I3545 - V9.0 - Merge of I3269 - Add Debug_XMLRenderer DebugPath to allow writing temp .xml file
                    01 Dec 2012 - mcdurdin - I3612 - V9.0 - Keyboard install should run as Admin only
                    02 Dec 2012 - mcdurdin - I3626 - V9.0 - Keyman Desktop Help window crashes
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
*)
unit XMLRenderer;  // I3306

interface

uses
  Windows,
  Classes,
  keymanapi_TLB,
  custinterfaces,
  Contnrs,
  SysUtils,
  TempFileManager;

type
  EXMLRenderer = class(Exception)
  end;

  TXMLRenderer = class
  private
    FXMLFileName: WideString;
  protected
    function XMLData(FRefreshKeyman: Boolean): WideString; virtual; abstract;
    function Stylesheet: WideString; virtual;
  public
    function Render: Boolean;
    function GetXMLData(FRefreshKeyman: Boolean): WideString;
    property XMLFileName: WideString read FXMLFileName;
  end;

  TXMLRenderers = class(TObjectList)
  private
    FRenderTemplate: WideString;
    function GetItem(Index: Integer): TXMLRenderer;
    procedure SetItem(Index: Integer; const Value: TXMLRenderer);
    function GetUILanguages: WideString;
  public
    constructor Create;
    function RenderToFile(FRefreshKeyman: Boolean = False; const AdditionalData: WideString = ''): TTempFile;   // I4181
    function TemplateExists: Boolean;
    property Items[Index: Integer]: TXMLRenderer read GetItem write SetItem; default;
    property RenderTemplate: WideString read FRenderTemplate write FRenderTemplate;
  end;

function GetXMLTemplatePath(const FileName: WideString): WideString;

implementation

uses
  DebugPaths,
  GetOSVersion,
  KeymanPaths,
  KLog,
  kmint,
  WideStrUtils,
  StrUtils,
  TypInfo,
  UFixupMissingFile,
  UILanguages,
  Unicode,
  utildir,
  utilhttp,
  utilsystem,
  utiluac,
  utilxml,
  xmldoc,
  xmlintf;

{ TXMLRendererRenderer }

function TXMLRenderer.GetXMLData(FRefreshKeyman: Boolean): WideString;
begin
  Result := XMLData(FRefreshKeyman);
end;

function TXMLRenderer.Render: Boolean;
begin
  Result := True;
end;

function TXMLRenderer.Stylesheet: WideString;
var
  s: string;
  n: Integer;
begin
  s := LowerCase(Copy(ClassName,2,Length(ClassName)));
  n := Pos('xmlrenderer', s);
  if n > 0 then
    Delete(s, n, Length('xmlrenderer'));
  Result := s + '.xsl';
end;



{ TXMLRenderers }

constructor TXMLRenderers.Create;
begin
  inherited Create;
end;

function TXMLRenderers.GetItem(Index: Integer): TXMLRenderer;
begin
  Result := inherited GetItem(Index) as TXMLRenderer;
end;

function TXMLRenderers.GetUILanguages: WideString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to UILanguageList.Count - 1 do
  begin
    Result := Result + '<uilanguage code="'+XMLEncode(UILanguageList.LanguageCode[i])+'" name="'+XMLEncode(UILanguageList.LanguageName[i])+'" />';
  end;
end;

function TXMLRenderers.RenderToFile(FRefreshKeyman: Boolean; const AdditionalData: WideString): TTempFile;   // I4181
var
  i: Integer;
  FOutput: TStrings;
  FTemplatePath: WideString;
  s: WideString;
  FXMLFileName: TTempFile;
  doc: IXMLDocument;
  xml: IXMLDocument;

    function URLPath(s: WideString): WideString;
    begin
      Result := 'file:///'+URLEncode(WideReplaceText(s, '\', '/'));  // I2612
    end;

    function ScriptURLPath(s: WideString): WideString;
    begin
      Result := URLPath(s);
    end;

begin
  FXMLFileName := TTempFileManager.Get('.htm');   // I4181

  FTemplatePath := GetXMLTemplatePath(FRenderTemplate);

  FOutput := TStringList.Create;
  try
    with kmint.KeymanCustomisation.CustMessages do
    begin
      s := GetLocalePathForLocale(LanguageCode); // + '/locale.xml';
      if not FileExists(s) then
        s := FTemplatePath + 'locale.xml';
    end;

    FOutput.Add(
      '<?xml version="1.0" encoding="utf-8"?>'+

      '<?xml-stylesheet type="text/xsl" href="'+XMLEncode(FTemplatePath+FRenderTemplate)+'"?>'+

      '<Keyman>'+
        AdditionalData+
        '<osversion id="'+GetEnumName(TypeInfo(TOS), Ord(GetOs))+'" index="'+IntToStr(Ord(GetOs))+'" />'+
        IfThen(CanElevate, '<canelevate />')+
        IfThen(Assigned(kmcom) and kmcom.SystemInfo.IsAdministrator, '<isadmin />')+   // I3612   // I3626
        '<templatepath>'+XMLEncode(URLPath(FTemplatePath))+'</templatepath>'+
        '<scripttemplatepath>'+XMLEncode(ScriptUrlPath(FTemplatePath))+'</scripttemplatepath>'+
        '<uilanguages>'+GetUILanguages+'</uilanguages>'+
        '<defaultlocalepath>'+XMLEncode(FTemplatePath)+'locale.xml</defaultlocalepath>'+
        '<localepath>'+XMLEncode(s)+'</localepath>');

    for i := 0 to Count - 1 do
      FOutput.Add(Items[i].GetXMLData(FRefreshKeyman));
    FOutput.Add('</Keyman>');

    doc := TXMLDocument.Create(nil);
    doc.ParseOptions := [poResolveExternals];  // I902 - resolve externals when loading XML files so locale.xml parses
    doc.LoadFromXML(FOutput.Text);

    xml := TXMLDocument.Create(nil);
    xml.ParseOptions := [poResolveExternals];  // I902 - resolve externals when loading XML files so locale.xml parses

    if not FileExists(FTemplatePath + FRenderTemplate) then
    begin
      try
        FixupMissingFile(FTemplatePath + FRenderTemplate);
      except
        on E:EFixupMissingFile do
          raise EXMLRenderer.Create(E.Message);
      end;
    end;

    xml.LoadFromFile(FTemplatePath + FRenderTemplate);

    doc.Node.transformNode(xml.Node, s);
    FOutput.Text := s;

    if KLEnabled or (GetDebugPath('Debug_XMLRenderer', '', False) <> '') then  // I3269   // I3545
      doc.SaveToFile(KGetTempFileName('.'+ChangeFileExt(FRenderTemplate,'.xml')));

    FOutput.SaveToFile(FXMLFileName.Name, TEncoding.UTF8);  // I3337   // I4181
  finally
    FOutput.Free;
  end;

  Result := FXMLFileName;
end;

procedure TXMLRenderers.SetItem(Index: Integer; const Value: TXMLRenderer);
begin
  inherited SetItem(Index, Value);
end;

function TXMLRenderers.TemplateExists: Boolean;
var
  FTemplatePath: WideString;
begin
  FTemplatePath := GetXMLTemplatePath(FRenderTemplate);
  Result := FileExists(FTemplatePath + FRenderTemplate);
end;

function OldXMLTemplatePath: WideString;
begin
  Result := GetDebugPath('xmltemplate desktop_pro.kxx',''); // I1803
  if Result = '' then
    Result := TKeymanPaths.KeymanDesktopInstallPath;
  if DirectoryExists(Result + 'xml') then Result := Result + 'xml\';
end;

function GetXMLTemplatePath(const FileName: WideString): WideString;
var
  Customisation: IKeymanCustomisation;
  CustMessages: IKeymanCustomisationMessages;
begin
  Customisation := kmint.KeymanCustomisation;
  CustMessages := Customisation.CustMessages;
  with CustMessages do
  begin
    Result := GetLocalePathForLocale(LanguageCode);

    if (ExtractFileName(Result) = 'locale.xml') and FileExists(ExtractFilePath(Result) + '\'+FileName)
      then Result := ExtractFilePath(Result) + '\'
      else Result := OldXMLTemplatePath;
  end;
end;

end.
