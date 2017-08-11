//****************************************************
//                  IEMultiDownload                  *
//                     For Delphi 5 to XE            *
//                Freeware Component                 *
//                       by                          *
//                                                   *
//              Eran Bodankin (bsalsa)               *
//                 bsalsa@gmail.com                  *
//                                                   *
// Documentation and updated versions:               *
//               http://www.bsalsa.com               *
//****************************************************

{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SystemS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SystemS. VSOFT SPECIFICALLY
DISCLAIMS ANY EXPRES OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a Link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please consider donation in our web site!
{*******************************************************************************}

unit IEMultiDownload;

{$I EWB.inc}

interface

uses
  WinInet, MSHTML_EWB, Windows, SysUtils, Classes, Forms, ExtCtrls,
  IEDownload, IEParser, EwbUrl;

type
  TIEMultiDownload = class;

  TMultiState = (msBusy, msReady, msStopped);
  TMultiDownloadOptions = (doAll, doImages, doPages, doVideo, doMusic);

  TDownloadItem = class(TCollectionItem)
  private
    FFileName: WideString;
    FPassword: WideString;
    FPath: WideString;
    FPort: integer;
    FProtocol: WideString;
    FRef: WideString;
    FRoot: WideString;
    FUser: WideString;
    procedure Set_Ref(const Value: WideString);
    procedure SetFileName(const Value: WideString);
    procedure SetPassword(const Value: WideString);
    procedure SetPath(const Value: WideString);
    procedure SetPort(const Value: integer);
    procedure SetProtocol(const Value: WideString);
    procedure SetRoot(const Value: WideString);
    procedure SetUser(const Value: WideString);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FileName: WideString read FFileName write SetFileName;
    property Ref: WideString read FRef write Set_Ref;
    property Password: WideString read FPassword write SetPassword;
    property Path: WideString read FPath write SetPath;
    property Port: integer read FPort write SetPort;
    property Protocol: WideString read FProtocol write SetProtocol;
    property Root: WideString read FRoot write SetRoot;
    property User: WideString read FUser write SetUser;
  end;

  TDownloadList = class(TCollection)
  private
    FIEMD: TIEMultiDownload;
    function GetItem(Index: Integer): TDownloadItem;
    procedure SetItem(Index: Integer; Value: TDownloadItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(IEMD: TIEMultiDownload);
    function Add: TDownloadItem;
    function Insert(Index: Integer): TDownloadItem;
    function IsListed(const aRef: WideString): Boolean;
    procedure DeleteItem(Index: Integer);
    procedure ClearItems;
  public
    property Items[index: Integer]: TDownloadItem read GetItem write SetItem; default;
  end;

  TOnMultiBeforeDownloadEvent = procedure(Sender: TObject; const hRef: WideString; const Item: TDownloadItem; var Cancel: boolean) of object;
  TOnMultiCompleteEvent = procedure(Sender: TObject; const DownloadedList: TStrings) of object;
  TOnMultiGetDocInfoEvent = procedure(Sender: TObject; const Text: string) of object;
  TOnMultiGetImageEvent = procedure(Sender: TObject; const ImgName: string; var Cancel: Boolean) of object;
  TOnMultiGetLinkEvent = procedure(Sender: TObject; const hRef, Host, HostName, PathName, Port, Protocol, MimeType, NameProp: string; var Cancel: Boolean) of object;
  TOnMultiGetQueryInfoEvent = procedure(const MimeType, Encoding, Disposition: string) of object;
  TOnMultiItemAddedEvent = procedure(Sender: TObject; const hRef, hProtocol, hRoot, hPath, hFileName, hUser, hPassword: WideString; const hPort: integer) of object;
  TOnMultiParseCompleteEvent = procedure(Sender: TObject; Doc: IhtmlDocument2; All: IHtmlElementCollection) of object;
  TOnMultiParseDocumentEvent = procedure(Sender: TObject; const Res: HRESULT; stMessage: string) of object;
  TOnMultiParseErrorEvent = procedure(Sender: TObject; const ErrorCode: integer; const Url, stError: string) of object;
  TOnMultiParseProgressEvent = procedure(Sender: TObject; const ulProgress, ulProgressMax: integer) of object;
  TOnMultiStateChangeEvent = procedure(Sender: TObject; const State: TMultiState) of object;
  TOnMultiStartParsingEvent = procedure(Sender: TObject; const aUrl: WideString) of object;

  TIEMultiDownload = class(TCustomIEDownload)
  private
    FAbout: string;
    FBaseFolder: WideString;
    FBaseUrl: WideString;
    FAbort: Boolean;
    FDownloadLevel: integer;
    FFromBaseSiteOnly: Boolean;
    FGetCompleteBaseSite: Boolean;
    FItems: TDownloadList;
    FMultiDownloadOptions: TMultiDownloadOptions;
    FMultiState: TMultiState;
    FMaxItems: integer;
    FOnMultiBeforeDownload: TOnMultiBeforeDownloadEvent;
    FOnMultiComplete: TOnMultiCompleteEvent;
    FOnMultiGetDocInfo: TOnMultiGetDocInfoEvent;
    FOnMultiGetImage: TOnMultiGetImageEvent;
    FOnMultiGetLink: TOnMultiGetLinkEvent;
    FOnMultiGetQueryInfo: TOnMultiGetQueryInfoEvent;
    FOnMultiItemAdded: TOnMultiItemAddedEvent;
    FOnMultiParseComplete: TOnMultiParseCompleteEvent;
    FOnMultiParseDocument: TOnMultiParseDocumentEvent;
    FOnMultiParseError: TOnMultiParseErrorEvent;
    FOnMultiParseProgress: TOnMultiParseProgressEvent;
    FOnMultiStateChange: TOnMultiStateChangeEvent;
    FOnMultiStartParsing: TOnMultiStartParsingEvent;
    FOpenFolder: boolean;
    FProgress, FProgressMax: integer;
    FTimer: TTimer;
    FRoorUrl: string;
    HtmlParser: TIEParser;
    slDownloadedList: TStringList;
    UrlParser: TUrl;
    procedure DoOnExit;
    procedure DownloadList(const aItems: TDownloadList);
    procedure MultiAnchor(Sender: TObject; hRef, Target, Rel, Rev, Urn, Methods, Name, Host, HostName, PathName, Port, Protocol, Search, Hash, AccessKey, ProtocolLong, MimeType, NameProp: string; Element: TElementInfo);
    procedure MultiGetDocInfo(Sender: TObject; const Text: string);
    procedure MultiGetQueryInfo(const MimeType, Encoding, Disposition: string);
    procedure MultiImage(Sender: TObject; Source: string; ImgElement: IHTMLImgElement; Element: TElementInfo);
    procedure MultiParseComplete(Sender: TObject; Doc: IhtmlDocument2; All: IHtmlElementCollection);
    procedure MultiParseDocument(Sender: TObject; const Res: HRESULT; stMessage: string);
    procedure MultiParseError(Sender: TObject; const ErrorCode: integer; const Url, stError: string);
    procedure MultiStartParsing(Sender: TObject; const aUrl: WideString);
    procedure SetAbout(Value: string);
    procedure SetItems(Value: TDownloadList);
    procedure SetMaxItems(Value: Integer);
    procedure MultiTimer(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddItem(const aRef: string): TDownloadItem;
    procedure GoMulti(BaseUrl: WideString);
    procedure SetDownloadOptions(const Value: TMultiDownloadOptions);
    procedure Stop;

  public
    property DownloadedList: TStringList read slDownloadedList;
    property MultiState: TMultiState read FMultiState;
  published
    property About: string read FAbout write SetAbout;
    property BaseUrl: WideString read FBaseUrl write FBaseUrl;
    property DownloadLevel: integer read FDownloadLevel write FDownloadLevel default 1;
    property DownloadOptions: TMultiDownloadOptions read FMultiDownloadOptions write SetDownloadOptions default doAll;
    property FromBaseSiteOnly: boolean read FFromBaseSiteOnly write FFromBaseSiteOnly default True;
    property GetCompleteBaseSite: boolean read FGetCompleteBaseSite write FGetCompleteBaseSite default False;
    property Items: TDownloadList read FItems write SetItems;
    property MaxItems: integer read FMaxItems write SetMaxItems default 100;
    property OnMultiBeforeDownload: TOnMultiBeforeDownloadEvent read FOnMultiBeforeDownload write FOnMultiBeforeDownload;
    property OnMultiComplete: TOnMultiCompleteEvent read FOnMultiComplete write FOnMultiComplete;
    property OnMultiGetDocInfo: TOnMultiGetDocInfoEvent read FOnMultiGetDocInfo write FOnMultiGetDocInfo;
    property OnMultiGetImage: TOnMultiGetImageEvent read FOnMultiGetImage write FOnMultiGetImage;
    property OnMultiGetLink: TOnMultiGetLinkEvent read FOnMultiGetLink write FOnMultiGetLink;
    property OnMultiGetQueryInfo: TOnMultiGetQueryInfoEvent read FOnMultiGetQueryInfo write FOnMultiGetQueryInfo;
    property OnMultiItemAdded: TOnMultiItemAddedEvent read FOnMultiItemAdded write FOnMultiItemAdded;
    property OnMultiParseComplete: TOnMultiParseCompleteEvent read FOnMultiParseComplete write FOnMultiParseComplete;
    property OnMultiParseDocument: TOnMultiParseDocumentEvent read FOnMultiParseDocument write FOnMultiParseDocument;
    property OnMultiParseError: TOnMultiParseErrorEvent read FOnMultiParseError write FOnMultiParseError;
    property OnMultiParseProgress: TOnMultiParseProgressEvent read FOnMultiParseProgress write FOnMultiParseProgress;
    property OnMultiStateChange: TOnMultiStateChangeEvent read FOnMultiStateChange write FOnMultiStateChange;
    property OnMultiStartParsing: TOnMultiStartParsingEvent read FOnMultiStartParsing write FOnMultiStartParsing;
  end;

implementation

uses
  IEDownloadTools;

procedure TDownloadItem.Assign(Source: TPersistent);
var
  Item: TDownloadItem;
begin
  if (Source is TDownloadItem) then
  begin
    Item := (Source as TDownloadItem);
    FRef := Item.Ref;
    FProtocol := Item.Protocol;
    FRoot := Item.Root;
    FPort := Item.Port;
    FFileName := Item.FileName;
    FUser := Item.User;
    FPassword := Item.Password;
    FPath := Item.Path;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

procedure TDownloadItem.SetFileName(const Value: WideString);
begin
  if FFileName <> Value then
    FFileName := Value;
end;

procedure TDownloadItem.SetPath(const Value: WideString);
begin
  if FPath <> Value then
    FPath := Value;
end;

procedure TDownloadItem.SetRoot(const Value: WideString);
begin
  if FRoot <> Value then
    FRoot := Value;
end;

procedure TDownloadItem.SetPort(const Value: integer);
begin
  if FPort <> Value then
    FPort := Value;
end;

procedure TDownloadItem.SetUser(const Value: WideString);
begin
  if FUser <> Value then
    FUser := Value;
end;

procedure TDownloadItem.SetPassword(const Value: WideString);
begin
  if FPassword <> Value then
    FPassword := Value;
end;

procedure TDownloadItem.Set_Ref(const Value: WideString);
begin
  if FRef <> Value then
    FRef := Value;
end;

procedure TDownloadItem.SetProtocol(const Value: WideString);
begin
  if FProtocol <> Value then
    FProtocol := Value;
end;

//-------------------------------------------------------------------------------

procedure TDownloadList.DeleteItem(Index: Integer);
begin
  Delete(Index);
end;

procedure TDownloadList.ClearItems;
begin
  Clear;
end;

function TDownloadList.GetItem(Index: Integer): TDownloadItem;
begin
  Result := TDownloadItem(inherited GetItem(Index));
end;

procedure TDownloadList.SetItem(Index: Integer; Value: TDownloadItem);
begin
  inherited SetItem(Index, Value);
end;

function TDownloadList.Add: TDownloadItem;
begin
  Result := TDownloadItem(inherited Add);
end;

function TDownloadList.Insert(Index: Integer): TDownloadItem;
begin
  Result := Add;
  Result.Index := Index;
end;

function TDownloadList.IsListed(const aRef: WideString): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    if CompareText(LowerCase(Items[I].FRef), LowerCase(aRef)) = 0 then
      Exit;
  Result := False;
end;

constructor TDownloadList.Create(IEMD: TIEMultiDownload);
begin
  inherited Create(TDownloadItem);
  FIEMD := IEMD;
end;

function TDownloadList.GetOwner: TPersistent;
begin
  Result := FIEMD;
end;

//-------------------------------------------------------------------------------

function TIEMultiDownload.AddItem(const aRef: string): TDownloadItem;
var
  UP: TUrl;
begin
  Result := nil;
  if (not FItems.isListed(aRef)) and (FItems.Count <> FMaxItems) and
    (IEDownloadTools.IsValidURL(aRef)) then
  begin
    slDownloadedList.Add(aRef);
    UP := TUrl.Create(aRef);
    UP.Clear;
    UP.QueryUrl(aRef);
    with FItems.Add do
    begin
      FRef := aRef;
      FProtocol := UP.Protocol;
      FRoot := UP.HostName;
      FPort := UP.Port;
      FFileName := UP.Document;
      FUser := UP.UserName;
      FPassword := UP.Password;
      FPath := CharReplace(UP.UrlPath, '/', '\');
      ;
      if (FPath = Trim('/')) or (FPath = Trim('\')) then
        FPath := EmptyStr;
      if Assigned(FOnMultiItemAdded) then
        FOnMultiItemAdded(Self, FRef, FProtocol, FRoot, FPath, FFileName,
          FUser, FPassword, FPort);
    end;
    UP.Free;
  end;
end;

procedure TIEMultiDownload.DownloadList(const aItems: TDownloadList);
var
  bCancel: Boolean;
  I: integer;
  FDLFolder: WideString;
begin
  if (not FAbort) then
    for I := 0 to aItems.Count - 1 do
    begin
      if FAbort then
        Exit;
      with aItems.Items[I] do
      begin
        bCancel := False;
        if Assigned(FOnMultiBeforeDownload) then
          FOnMultiBeforeDownload(Self, aItems.Items[I].FRef, aItems.Items[I], bCancel);
        if not bCancel then
        begin
          FDLFolder := FBaseFolder + aItems.Items[I].FRoot +
            IncludeTrailingPathDelimiter(aItems.Items[I].FPath);
          if (aItems.Items[I].FRef <> EmptyStr) then
            Go(aItems.Items[I].FRef, aItems.Items[I].FFileName, FDLFolder);
          FDLFolder := EmptyStr;
        end;
      end;
    end;
end;

procedure TIEMultiDownload.GoMulti(BaseUrl: WideString);
var
  I: integer;
begin
  Reset;
  FAbort := False;
  FProgress := 0;
  FProgressMax := 100;
  FMultiState := msBusy;
  if Assigned(FOnMultiStateChange) then
    FOnMultiStateChange(Self, FMultiState);
  if OpenDownloadFolder = True then
  begin
    FOpenFolder := True;
    OpenDownloadFolder := False;
  end;
  if DownloadFolder = EmptyStr then
    DownloadFolder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName) + 'Downloads');
  FBaseFolder := DownloadFolder;
  for I := 0 to FItems.Count - 1 do
  begin
    if (FItems[i].FRef <> EmptyStr) then
      DownloadList(FItems);
  end;
  if (BaseUrl <> EmptyStr) then
  begin
    UrlParser := TUrl.Create(BaseUrl);
    UrlParser.CrackUrl(BaseUrl, ICU_ESCAPE);
    FRoorUrl := UrlParser.HostName;
    with HtmlParser do
    begin
      FTimer := TTimer.Create(nil);
      FTimer.Enabled := True;
      FTimer.Interval := 500;
      FTimer.OnTimer := MultiTimer;
      OnAnchor := MultiAnchor;
      OnImage := MultiImage;
      OnParseDocument := MultiParseDocument;
      OnParseComplete := MultiParseComplete;
      OnParseError := MultiParseError;
      OnDocInfo := MultiGetDocInfo;
      OnQueryInfo := MultiGetQueryInfo;
      OnStartParsing := MultiStartParsing;
      Parse(BaseUrl);
      if (not FGetCompleteBaseSite) then
      begin
        if Assigned(FTimer) then
          FreeAndNil(FTimer);
        DownloadList(FItems);
      end
      else
      begin
        if (not FAbort) then
          for I := 0 to slDownloadedList.Count - 1 do
          begin
            Parse(slDownloadedList[I]);
            if Assigned(FTimer) then
              FreeAndNil(FTimer);
          end;
        DownloadList(FItems);
      end;
    end;
    if UrlParser <> nil then
      FreeAndNil(UrlParser);
  end;
  DoOnExit;
end;

procedure TIEMultiDownload.MultiTimer(Sender: TObject);
begin
  FProgress := FProgress + 10;
  if FProgress = FProgressMax then
    FProgress := 1;
  if Assigned(FOnMultiParseProgress) then
    FOnMultiParseProgress(Self, FProgress, FProgressMax);
end;

procedure TIEMultiDownload.DoOnExit;
begin
  if FOpenFolder then
    OpenFolder(FBaseFolder);
  if Assigned(FOnMultiComplete) then
    FOnMultiComplete(Self, slDownloadedList);
  slDownloadedList.Clear;
  Items.Clear;
  FMultiState := msStopped;
  if Assigned(FOnMultiStateChange) then
    FOnMultiStateChange(Self, FMultiState);
end;

procedure TIEMultiDownload.Stop;

begin
  if FMultiState <> msBusy then
    Exit;
  FAbort := True;
  HtmlParser.Stop;
  CancelAll;
  while State <> sBusy do
    Forms.Application.ProcessMessages;
  DoOnExit;
end;

constructor TIEMultiDownload.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FAbout := 'TIEMultiDownload from: http://www.bsalsa.com';
  FDownloadLevel := 1;
  FFromBaseSiteOnly := True;
  FItems := TDownloadList.Create(Self);
  FMaxItems := 100;
  FMultiDownloadOptions := doAll;
  slDownloadedList := TStringList.Create;
  with slDownloadedList do
  begin
{$IFDEF DELPHI6UP}
    CaseSensitive := False;
{$ENDIF}
    Sorted := True;
    Duplicates := dupIgnore;
  end;
  FMultiState := msReady;
  HtmlParser := TIEParser.Create(nil);
end;

destructor TIEMultiDownload.Destroy;
begin
  slDownloadedList.Free;
  if HtmlParser <> nil then
    FreeAndNil(HtmlParser);
  FItems.Free;
  inherited Destroy;
end;

procedure TIEMultiDownload.SetAbout(Value: string);
begin
  Exit;
end;

procedure TIEMultiDownload.SetDownloadOptions(const Value: TMultiDownloadOptions);
begin
  FMultiDownloadOptions := Value;
end;

procedure TIEMultiDownload.SetItems(Value: TDownloadList);
begin
  FItems.Assign(Value);
end;

procedure TIEMultiDownload.SetMaxItems(Value: Integer);
begin
{$IFDEF DELPHI10_UP}
  if (Value <> FItems.Capacity) then
    FItems.Capacity := Value;
{$ENDIF}
end;

procedure TIEMultiDownload.MultiAnchor(Sender: TObject; hRef, Target, Rel, Rev, Urn,
  Methods, Name, Host, HostName, PathName, Port, Protocol, Search, Hash,
  AccessKey, ProtocolLong, MimeType, NameProp: string; Element: TElementInfo);
var
  bCancel: Boolean;
begin
  if FMultiDownloadOptions = doImages then
    Exit;
  bCancel := False;
  if (hRef <> EmptyStr) and (not StrContain('mailto', hRef)) then
  begin
    if FFromBaseSiteOnly and (not StrContain(FRoorUrl, hRef)) then
      Exit;
    if Assigned(FOnMultiGetLink) then
      FOnMultiGetLink(Self, hRef, Host, HostName, PathName, Port, Protocol,
        MimeType, NameProp, bCancel);
    if (not bCancel) and (not FAbort) and ((FMultiDownloadOptions = doAll) and (not FAbort)
      or (FMultiDownloadOptions = doPages)) then
    begin
      AddItem(LowerCase(hRef));
    end;
  end;
end;

procedure TIEMultiDownload.MultiImage(Sender: TObject; Source: string; ImgElement: IHTMLImgElement; Element: TElementInfo);
var
  bCancel: Boolean;
begin
  if FMultiDownloadOptions = doPages then
    Exit;
  bCancel := False;
  if Source <> EmptyStr then
  begin
    if Assigned(FOnMultiGetImage) then
      FOnMultiGetImage(Self, Source, bCancel);
    if (not bCancel) and (not FAbort) and ((FMultiDownloadOptions = doAll) or
      (FMultiDownloadOptions = doImages)) then
    begin
      AddItem(LowerCase(Source));
    end;
  end;
end;

procedure TIEMultiDownload.MultiGetDocInfo(Sender: TObject; const Text: string);
begin
  if Assigned(FOnMultiGetDocInfo) and not (FAbort) then
    FOnMultiGetDocInfo(Self, Text);
end;

procedure TIEMultiDownload.MultiParseError(Sender: TObject; const ErrorCode: integer; const
  Url, stError: string);
begin
  if Assigned(FOnMultiParseError) then
    FOnMultiParseError(Self, ErrorCode, Url, stError);
end;

procedure TIEMultiDownload.MultiStartParsing(Sender: TObject; const aUrl: WideString);
begin
  if Assigned(FOnMultiStartParsing) then
    FOnMultiStartParsing(Self, aUrl);
end;

procedure TIEMultiDownload.MultiGetQueryInfo(const MimeType, Encoding, Disposition: string);
begin
  if Assigned(FOnMultiGetQueryInfo) and not (FAbort) then
    FOnMultiGetQueryInfo(MimeType, Encoding, Disposition);
end;

procedure TIEMultiDownload.MultiParseDocument(Sender: TObject; const
  Res: HRESULT; stMessage: string);
begin
  if (Assigned(FOnMultiParseDocument)) and not (FAbort) then
    FOnMultiParseDocument(Self, Res, stMessage);
end;

procedure TIEMultiDownload.MultiParseComplete(Sender: TObject;
  Doc: IhtmlDocument2; All: IHtmlElementCollection);
begin
  if Assigned(FOnMultiParseComplete) then
    FOnMultiParseComplete(Self, Doc, All);

  if Assigned(FOnMultiParseProgress) then
    FOnMultiParseProgress(Self, 0, 0);
end;

end.

