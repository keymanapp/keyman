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
  Windows,
  SysUtils,
  Classes,
  TypInfo,
//  CustomisationStorage,
//  StockFileNames,
//  StockMessages,
  msxml;

type
(*  TCustomisationMessage = class(TCollectionItem)
  private
    FValue: WideString;
    FID: string;
//  public
//    function TranslatedValue(const localedoc, defaultlocaledoc: IXMLDOMDocument): WideString;
  published
    property ID: string read FID write FID;
    property Value: WideString read FValue write FValue;
  end;

  TCustomisationMessages = class(TCollection)
  private
    function GetItem(Index: Integer): TCustomisationMessage;
    procedure SetItem(Index: Integer; const Value: TCustomisationMessage);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Add: TCustomisationMessage;
    property Items[Index: Integer]: TCustomisationMessage read GetItem write SetItem; default;
  end;
*)

  TCustomisationMessageManager = class(TComponent)
  private
    FLanguageCode: string;
//    FCustStorage: TCustomisationStorage;
//    FCustFile: TCustFile;
//    FMessages: TCustomisationMessages;
    FLocaleDoc: IXMLDOMDocument;
    FDefaultLocaleDoc: IXMLDOMDocument;
    FLanguages: TStringList;
    FCustStorageFilename: string;
//    FSections: TStockMessageSections;  // I3217
//    procedure SetSections(const Value: TStockMessageSections);  // I3217
//    procedure SetMessages(const Value: TCustomisationMessages);
    procedure SetLanguageCode(Value: string);
    function GetAvailableLanguages: string;
    procedure LoadLocale;
  public
    constructor Create(ACustStorageFilename: string; ALoadLocale: Boolean = True); reintroduce;
    destructor Destroy; override;
    procedure Load(ALoadLocale: Boolean = True);
//    procedure Save;
    function MessageFromID(ID: WideString): WideString; overload;
    function MessageFromID(ID, LanguageCode: WideString): WideString; overload;
//    function MessageObjectFromID(ID: string): TCustomisationMessage;
    function GetLocalePathForLocale(LocaleName: WideString): WideString;
    procedure GetDialogParameters(DialogName: WideString; var FWidth: Integer; var FHeight: Integer);
  published
    property AvailableLanguages: string read GetAvailableLanguages;
    property LanguageCode: string read FLanguageCode write SetLanguageCode;
//    property Messages: TCustomisationMessages read FMessages write SetMessages;
//    property Sections: TStockMessageSections read FSections write SetSections;  // I3217
  end;

implementation

uses
  DebugPaths,
  MessageDefaults,
  KLog,
  ErrorControlledRegistry,
  RegistryKeys;

{ TCustomisationMessages }

(*
function TCustomisationMessages.Add: TCustomisationMessage;
begin
  Result := inherited Add as TCustomisationMessage;
end;

constructor TCustomisationMessages.Create;
begin
  inherited Create(TCustomisationMessage);
end;

destructor TCustomisationMessages.Destroy;
begin
  inherited Destroy;
end;

function TCustomisationMessages.GetItem(Index: Integer): TCustomisationMessage;
begin
  Result := inherited GetItem(Index) as TCustomisationMessage;
end;

procedure TCustomisationMessages.SetItem(Index: Integer; const Value: TCustomisationMessage);
begin
  inherited SetItem(Index, Value);
end;
*)

{ TCustomisationMessageManager }

constructor TCustomisationMessageManager.Create(ACustStorageFilename: string; ALoadLocale: Boolean = True);
begin
  inherited Create(nil);
  FLanguages := TStringList.Create;
  FCustStorageFilename := ACustStorageFilename;
//  FCustStorage := ACustStorage;
//  FMessages := TCustomisationMessages.Create;
//  FSections := TStockMessageSections.Create;  // I3217
  //FDomain := WideChangeFileExt(WideExtractFileName(FCustStorage.FileName), '');
  //DefaultInstance.bindtextdomain(FDomain, LocalePath); //ExtractFilePath(FCustStorage.FileName)+'locale\');
  Load(ALoadLocale);
end;

destructor TCustomisationMessageManager.Destroy;
begin
//  FreeAndNil(FSections);  // I3217
//  FreeAndNil(FMessages);
  FreeAndNil(FLanguages);
  inherited Destroy;
end;

function TCustomisationMessageManager.GetAvailableLanguages: string;

  function IsValidLocaleFile(const path: string): Boolean;  // I3192
  var
    FLocaleDoc: IXmlDomDocument;
  begin
    try
      FLocaleDoc := CoDomDocument.Create;
      FLocaleDoc.preserveWhiteSpace := True;
      FLocaleDoc.async := False;
      FLocaleDoc.validateOnParse := False;
      FLocaleDoc.load(path); //GetLocalePath + LanguageCode + '\locale.xml');
      if FLocaleDoc.documentElement <> nil
        then Result := (FLocaleDoc.documentElement.tagName = 'Locale')
        else Result := False;
    except
      Result := False;
    end;
  end;

  procedure ReadPackageLanguages(IsLM: Boolean);
  var
    i: Integer;
    s, t: string;
    f: TSearchRec;
    keys: TStringList;
    Path: string;
  begin
    keys := TStringList.Create;
    with TRegistryErrorControlled.Create do  // I2890
    try
      if IsLM then
      begin
        RootKey := HKEY_LOCAL_MACHINE;
        Path := SRegKey_InstalledPackages_LM;
      end
      else
      begin
        RootKey := HKEY_CURRENT_USER;
        Path := SRegKey_InstalledPackages_CU;
      end;
      if OpenKeyReadOnly('\'+Path) then
      begin
        GetKeyNames(keys);
        for i := 0 to keys.Count - 1 do
        begin
          if OpenKeyReadOnly('\'+Path+'\'+keys[i]) then
          begin
            if ValueExists(SRegValue_PackageFile) then
            begin
              s := ReadString(SRegValue_PackageFile);
              if FindFirst(ExtractFilePath(s)+'locale-*.xml', 0, f) = 0 then
              begin
                repeat
                  t := ChangeFileExt(Copy(f.Name, 8, Length(f.Name)), '');
                  if FLanguages.IndexOfName(t) < 0 then
                  begin
                    if IsValidLocaleFile(ExtractFilePath(s)+f.Name) then  // I3192
                      FLanguages.Add(t+'='+ExtractFilePath(s)+f.Name);
                    if Result <> '' then Result := Result + #13#10;
                    Result := Result + t;
                  end;
                until FindNext(f) <> 0;
                FindClose(f);
              end;
            end;
          end;
        end;
      end;
    finally
      keys.Free;
      Free;
    end;
  end;

var
  f: TSearchRec;
  FLocalePath: WideString;
begin
  Result := '';
  FLocalePath := ExtractFilePath(FCustStorageFileName)+'locale\';
  FLanguages.Clear;
  if FindFirst(FLocalePath + '*', faDirectory, f) = 0 then
  begin
    repeat
      if ((f.Attr and faDirectory) = faDirectory) and (f.Name <> '.') and (f.Name <> '..') then
      begin
        if IsValidLocaleFile(FLocalePath+f.Name+'\locale.xml') then  // I3192
          FLanguages.Add(f.Name+'='+FLocalePath+f.Name+'\locale.xml');
        if Result <> '' then Result := Result + #13#10;
        Result := Result + f.Name;
      end;
    until FindNext(f) <> 0;
    FindClose(f);
  end;

  { Find all locale*.xml files in the user's installed keyboards }

  ReadPackageLanguages(True);
  ReadPackageLanguages(False);
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
begin
  if LocaleName = ''
    then Result := ExtractFilePath(FCustStorageFileName)+'locale\'
    else Result := FLanguages.Values[LocaleName];
end;

procedure TCustomisationMessageManager.Load(ALoadLocale: Boolean = True);
//var
//  n: Integer;
//  ms: TMemoryStream;
begin
  GetAvailableLanguages;

(*
  n := FCustStorage.CustFiles.IndexOfFileName(StockFileName_Messages);
  if n < 0 then
  begin
    with FCustStorage.CustFiles.AddCustFile do
    begin
      FileName := StockFileName_Messages;
    end;
    n := FCustStorage.CustFiles.IndexOfFileName(StockFileName_Messages);
  end;
  FCustFile := FCustStorage.CustFiles[n];

  FCustFile.Stream.Position := 0;
  if FCustFile.Stream.Size > 0 then
  try
    ms := TMemoryStream.Create;
    try
      ObjectTextToBinary(FCustFile.Stream, ms);
      ms.Position := 0;
      ms.ReadComponent(Self);
    finally
      ms.Free;
    end;
  except
    on E:Exception do
    begin
      //ExceptionHook.LogException;
      KL.LogError('Exception %s loading messages: %s', [E.ClassName, E.Message]);
    end;
  end;
*)

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
  if FileExists(GetLocalePathForLocale(LanguageCode)) then // + LanguageCode + '\locale.xml') then
    FLocaleDoc.load(GetLocalePathForLocale(LanguageCode)); //GetLocalePath + LanguageCode + '\locale.xml');

  FDefaultLocaleDoc := CoDomDocument.Create;
  FDefaultLocaleDoc.preserveWhiteSpace := True;
  FDefaultLocaleDoc.async := False;
  FDefaultLocaleDoc.validateOnParse := False;

  FPath := GetDebugPath('xmltemplate '+ExtractFileName(FCustStorageFileName), ExtractFilePath(FCustStorageFileName));
  if FileExists(FPath + 'locale.xml') then
    FDefaultLocaleDoc.load(FPath + 'locale.xml')
  else if FileExists(ExtractFilePath(FCustStorageFileName) + 'xml\locale.xml') then
    FDefaultLocaleDoc.load(ExtractFilePath(FCustStorageFileName) + 'xml\locale.xml');
end;

function TCustomisationMessageManager.MessageFromID(ID: WideString): WideString;
var
  node: IXMLDOMNode;
begin
  node := Flocaledoc.selectSingleNode('/Locale/String[@Id="'+ID+'"]');
  if Assigned(node) then
    Result := node.text
  else
  begin
    node := Fdefaultlocaledoc.selectSingleNode('/Locale/String[@Id="'+ID+'"]');
    if Assigned(node)
      then Result := node.text
      else Result := ID;
  end;
end;

function TCustomisationMessageManager.MessageFromID(ID,
  LanguageCode: WideString): WideString;
var
  mm: TCustomisationMessageManager;
begin
  if LanguageCode = Self.FLanguageCode then
    Result := MessageFromID(ID)
  else
  begin
    mm := TCustomisationMessageManager.Create(FCustStorageFilename, False);
    try
      mm.LanguageCode := LanguageCode;
      Result := mm.MessageFromID(ID);
    finally
      mm.Free;
    end;
  end;
end;

(*function TCustomisationMessageManager.MessageObjectFromID(ID: string): TCustomisationMessage;
var
  i: Integer;
begin
  for i := 0 to FMessages.Count - 1 do
    if AnsiCompareText(FMessages[i].ID, ID) = 0 then
    begin
      Result := FMessages[i];
      Exit;
    end;
  Result := nil;
end;*)

//procedure TCustomisationMessageManager.Save;
//begin
//  FCustFile.Stream.Size := 0;
//  FCustFile.Stream.WriteComponent(Self);
//end;

procedure TCustomisationMessageManager.SetLanguageCode(Value: string);
begin
  FLanguageCode := Value;
  LoadLocale;
end;

{procedure TCustomisationMessageManager.SetMessages(const Value: TCustomisationMessages);
begin
  FMessages.Assign(Value);
end;

procedure TCustomisationMessageManager.SetSections(
  const Value: TStockMessageSections);  // I3217
begin
  FSections.Assign(Value);
end;}

{ TCustomisationMessage }

(*function TCustomisationMessage.TranslatedValue(const localedoc, defaultlocaledoc: IXMLDOMDocument): WideString;
var
  node: IXMLDOMNode;
begin
  node := localedoc.selectSingleNode('/Locale/String[@Id="'+ID+'"]');
  if Assigned(node) then
    Result := node.text
  else
  begin
    node := defaultlocaledoc.selectSingleNode('/Locale/String[@Id="'+ID+'"]');
    if Assigned(node)
      then Result := node.text
      else Result := Value;
  end;
  //Result := dgettext(domain, ID);
  //if Result = ID then Result := Value; { No default found, so use the value from the .pxx file }
end;*)

end.

