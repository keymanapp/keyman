(*
  Name:             CustomisationMessages
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    06 Oct 2006 - mcdurdin - Allow embedding of messages within messages
                    04 Dec 2006 - mcdurdin - Add localization
                    12 Dec 2006 - mcdurdin - Add GetDialogParameters
                    12 Dec 2006 - mcdurdin - Require locale.xml for all localization
                    12 Dec 2006 - mcdurdin - Manage active LanguageCode here
                    04 Jan 2007 - mcdurdin - Fix locale.xml validation (it won't...)
                    23 Aug 2007 - mcdurdin - I956 - Add support for locales in kmp files
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support

                    24 Jan 2012 - mcdurdin - I3217 - Avoid error loading messages.txt due to sections property
                    24 Jan 2012 - mcdurdin - I3192 - Crash parsing invalid locale XML file
*)
unit CustomisationMessages;  // I3306

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.TypInfo,
  Winapi.msxml,
  Winapi.Windows;

type
  TCustomisationLocale = class
    ID: string;
    Path: string;
    Name: string;
    NameWithEnglish: string;
  end;

  TCustomisationLocaleList = class(TObjectDictionary<string, TCustomisationLocale>)
  end;

  TCustomisationMessageManager = class(TComponent)
  private
    FLanguageCode: string;
    FLocaleDoc: IXMLDOMDocument;
    FDefaultLocaleDoc: IXMLDOMDocument;
    FLanguages: TCustomisationLocaleList;
    FCustStorageFilename: string;
    procedure SetLanguageCode(Value: string);
    function GetAvailableLanguages: string;
    procedure LoadLocale;
  public
    constructor Create(ACustStorageFilename: string; ALoadLocale: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Load(ALoadLocale: Boolean = True);
    function MessageFromID(ID: WideString): WideString; overload;
    function MessageFromID(ID, LanguageCode: WideString): WideString; overload;
    function GetLocalePathForLocale(LocaleName: WideString): WideString;
    procedure GetDialogParameters(DialogName: WideString; var FWidth: Integer; var FHeight: Integer);
  published
    property AvailableLanguages: string read GetAvailableLanguages;
    property LanguageCode: string read FLanguageCode write SetLanguageCode;
  end;

implementation

uses
  DebugPaths,
  Keyman.System.AndroidStringToKeymanLocaleString,
  KLog,
  ErrorControlledRegistry,
  MessageIdentifierConsts,
  RegistryKeys;

{ TCustomisationMessageManager }

constructor TCustomisationMessageManager.Create(ACustStorageFilename: string; ALoadLocale: Boolean = True);
begin
  inherited Create(nil);
  FLanguages := TCustomisationLocaleList.Create;
  FCustStorageFilename := ACustStorageFilename;
  Load(ALoadLocale);
end;

destructor TCustomisationMessageManager.Destroy;
begin
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

function TCustomisationMessageManager.GetAvailableLanguages: string;
var
  FLocaleIndexDoc: IXmlDomDocument;
  node: IXMLDOMNode;
  locale: TCustomisationLocale;
  FLocalePath: WideString;

  function LoadLocaleData(node: IXMLDOMNode; locale: TCustomisationLocale): Boolean;
  var
    v: IXMLDOMNode;
  begin
    v := node.attributes.getNamedItem('SKLanguageCode');
    if v = nil then Exit(False);
    locale.ID := v.nodeValue;
    locale.Path := FLocalePath + locale.ID + '\strings.xml';

    v := node.attributes.getNamedItem('SKUILanguageName');
    if v = nil then Exit(False);
    locale.Name := v.nodeValue;

    v := node.attributes.getNamedItem('SKUILanguageNameWithEnglish');
    if v = nil then Exit(False);
    locale.NameWithEnglish := v.nodeValue;

    Result := True;
  end;

begin
  Result := '';
  FLocalePath := ExtractFilePath(FCustStorageFileName)+'locale\';
  FLanguages.Clear;

  try
    FLocaleIndexDoc := CoDomDocument.Create;
    FLocaleIndexDoc.preserveWhiteSpace := True;
    FLocaleIndexDoc.async := False;
    FLocaleIndexDoc.validateOnParse := False;
    FLocaleIndexDoc.load(FLocalePath + 'index.xml');
    if (FLocaleIndexDoc.documentElement = nil) or (FLocaleIndexDoc.documentElement.tagName <> 'locales') then
      Exit;

    node := FLocaleIndexDoc.documentElement.firstChild;
    if not Assigned(node) then
      Exit;

    while node <> nil do
    begin
      if node.nodeName = 'locale' then
      begin
        locale := TCustomisationLocale.Create;
        if not LoadLocaleData(node, locale) then
          locale.Free
        else
        begin
          FLanguages.Add(locale.ID, locale);
          Result := Result + locale.ID + #13#10;
        end;
      end;
      node := node.nextSibling;
      end;
  except
    // If any locale items are invalid, we will have only English
    // Currently, we cannot report errors to Sentry from kmcomapi
    Result := '';
  end;
end;

procedure TCustomisationMessageManager.GetDialogParameters(
  DialogName: WideString; var FWidth, FHeight: Integer);
var
  node, attr: IXMLDOMNode;
begin
  FWidth := 0; FHeight := 0;
  node := FLocaleDoc.selectSingleNode('/Locale/Dialog[@Id="'+DialogName+'"]');
  if not Assigned(node) then
    node := FDefaultLocaleDoc.selectSingleNode('/Locale/Dialog[@Id="'+DialogName+'"]');

  if Assigned(node) then
  begin
    attr := node.attributes.getNamedItem('Width');
    try
      if Assigned(attr) then FWidth := attr.nodeValue;
    except
      FWidth := 0;
    end;
    attr := node.attributes.getNamedItem('Height');
    try
      if Assigned(attr) then FHeight := attr.nodeValue;
    except
      FHeight := 0;
    end;
  end;
end;

function TCustomisationMessageManager.GetLocalePathForLocale(
  LocaleName: WideString): WideString;
var
  locale: TCustomisationLocale;
begin
  if LocaleName = '' then
    Result := ExtractFilePath(FCustStorageFileName)+'locale\'
  else if not FLanguages.TryGetValue(LocaleName, locale) then
    Result := ''
  else
    Result := locale.Path;
end;

procedure TCustomisationMessageManager.Load(ALoadLocale: Boolean = True);
begin
  GetAvailableLanguages;
  if ALoadLocale then
    LoadLocale;
end;

procedure TCustomisationMessageManager.LoadLocale;
var
  FPath: WideString;
begin
  FLocaleDoc := CoDomDocument.Create;
  FLocaleDoc.preserveWhiteSpace := True;
  FLocaleDoc.async := False;
  FLocaleDoc.validateOnParse := False;
  if FileExists(GetLocalePathForLocale(LanguageCode)) then
    FLocaleDoc.load(GetLocalePathForLocale(LanguageCode));

  FDefaultLocaleDoc := CoDomDocument.Create;
  FDefaultLocaleDoc.preserveWhiteSpace := True;
  FDefaultLocaleDoc.async := False;
  FDefaultLocaleDoc.validateOnParse := False;

  FPath := GetDebugPath('KeymanConfigStaticHttpFilesPath', ExtractFilePath(FCustStorageFileName));
  if FileExists(FPath + 'strings.xml') then
    FDefaultLocaleDoc.load(FPath + 'strings.xml')
  else if FileExists(ExtractFilePath(FCustStorageFileName) + 'xml\strings.xml') then
    FDefaultLocaleDoc.load(ExtractFilePath(FCustStorageFileName) + 'xml\strings.xml');
end;

(**
  * Look up a localized string for the currently active localization. If
  * not found, looks up the string from the default (i.e. en)
  * localization, and finally falls back to the ID itself on failure.
  *
  * @param ID            ID of string to lookup.
  * @return Localized string value.
  *)
function TCustomisationMessageManager.MessageFromID(ID: WideString): WideString;
var
  node: IXMLDOMNode;
begin
  node := Flocaledoc.selectSingleNode('/resources/string[@name="'+ID+'"]');
  if Assigned(node) then
    Result := TAndroidStringToKeymanLocaleString.Transform(node.text)
  else
  begin
    node := Fdefaultlocaledoc.selectSingleNode('/resources/string[@name="'+ID+'"]');
    if Assigned(node)
      then Result := TAndroidStringToKeymanLocaleString.Transform(node.text)
      else Result := ID;
  end;
end;

(**
  * Look up a localized string for a specific localization. This function
  * can return values for only `SKLanguageCode`, `SKUILanguageName`
  * and `SKUILanguageNameWithEnglish`. Other strings will be returned
  * from the currently active localization.
  *
  * @param ID            ID of string to lookup.
  * @param LanguageCode  The BCP-47 language tag.
  * @return Localized string value.
  *)
function TCustomisationMessageManager.MessageFromID(ID,
  LanguageCode: WideString): WideString;
var
  locale: TCustomisationLocale;
begin
  if not FLanguages.TryGetValue(LanguageCode, locale) then
    Exit(MessageFromID(ID));

  if ID = StringFromMsgId(SKUILanguageName) then
  begin
    Result := locale.Name;
  end
  else if ID = StringFromMsgId(SKUILanguageNameWithEnglish) then
  begin
    Result := locale.NameWithEnglish;
  end
  else if ID = StringFromMsgId(SKLanguageCode) then
  begin
    Result := locale.ID;
  end
  else
  begin
    // Other tags are no longer supported for performance reasons
    Result := MessageFromID(ID);
  end;
end;

procedure TCustomisationMessageManager.SetLanguageCode(Value: string);
begin
  FLanguageCode := Value;
  LoadLocale;
end;

end.

