unit Keyman.Developer.System.HttpServer.AppSource;

interface

uses
  System.Generics.Collections,
  System.StrUtils,
  System.SysUtils,

  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer,

  Keyman.System.HttpServer.Base;

// /app/source

type
  TAppSource = record
    Filename, Data: string;
  end;

  TAppSourceHttpResponder = class(TBaseHttpResponder)
  private
    FSources: TThreadList<TAppSource>;
    procedure RespondGet(const AFilename: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure RespondPost(const AFilename: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure RespondTouchEditor(const AFilename: string; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure RespondTouchEditorLib(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    function TrySetSource(const Filename, Data: string): Boolean;
    procedure RespondTouchEditorState(const AFilename: string;
      AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    procedure RegisterSource(const Filename, Data: string; Update: Boolean = False);
    procedure UnregisterSource(const Filename: string);
    function IsSourceRegistered(const Filename: string): Boolean;
    function GetSource(const Filename: string): string;
    function TryGetSource(const Filename: string; var Data: string): Boolean;
    procedure SetSource(const Filename, Data: string);
  end;

implementation

uses
  Xml.XMLDoc,
  Xml.XMLIntf,

  Keyman.System.KeymanSentryClient,
  RedistFiles;

{ TAppSourceHttpResponder }

{*
  Gets the current source content from the filename requested
}
constructor TAppSourceHttpResponder.Create;
begin
  inherited Create;
  FSources := TThreadList<TAppSource>.Create;
end;

destructor TAppSourceHttpResponder.Destroy;
var
  T: TList<TAppSource>;
begin
  T := FSources.LockList;
  try
    // Note: Unlike regular functions, Assert has short-circuit evaluation
    // intrinsics on the first param which makes it safe to dereference T[0] in
    // the second parameter.
    Assert(T.Count = 0, 'TAppSourceHttpResponder.Sources should be empty at destruction '+
      '(T.Count='+IntToStr(T.Count)+', T[0].Filename='+T[0].Filename+')');
  finally
    FSources.UnlockList;
  end;

  FSources.Free;
  inherited Destroy;
end;

procedure TAppSourceHttpResponder.ProcessRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Filename: string;
begin
  // /app/source?Filename=xxxx
  // POST = update
  // GET = retrieve

  if ARequestInfo.Document = '/app/source/file' then
  begin
    // TODO: We should be passing a token to the browser for future POST security
    Filename := CrackUTF8ZeroExtendedString(ARequestInfo.CommandType, ARequestInfo.Params.Values['Filename']);

    if ARequestInfo.CommandType = hcGET then
    begin
      RespondGet(Filename, AContext, ARequestInfo, AResponseInfo);
    end
    else if ARequestInfo.CommandType = hcPOST then
    begin
      RespondPost(Filename, AContext, ARequestInfo, AResponseInfo);
    end
    else
      Respond404(AContext, ARequestInfo, AResponseInfo);
  end
  else if ARequestInfo.Document = '/app/source/toucheditor' then
  begin
    // Respond files?
    Filename := CrackUTF8ZeroExtendedString(ARequestInfo.CommandType, ARequestInfo.Params.Values['Filename']);
    RespondTouchEditor(Filename, AContext, ARequestInfo, AResponseInfo);
  end
  else if ARequestInfo.Document = '/app/source/toucheditor/state' then
  begin
    Filename := CrackUTF8ZeroExtendedString(ARequestInfo.CommandType, ARequestInfo.Params.Values['Filename']);
    RespondTouchEditorState(Filename, AContext, ARequestInfo, AResponseInfo);
  end
  else if ARequestInfo.Document.StartsWith('/app/source/toucheditor/lib/') then
  begin
    RespondTouchEditorLib(AContext, ARequestInfo, AResponseInfo);
  end
  else
  begin
    Respond404(AContext, ARequestInfo, AResponseInfo);
    Exit;
  end;
end;

procedure TAppSourceHttpResponder.RespondTouchEditorState(const AFilename: string;
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  FData: string;
begin
  if ARequestInfo.CommandType = hcGET then
  begin
    AResponseInfo.ContentType := 'application/json';
    AResponseInfo.Charset := 'UTF-8';
    TKeymanSentryClient.Breadcrumb('default', Format('RespondTouchEditorState: Filename=%s[#state]', [AFilename]), 'AppSourceHttpResponder');
    if TryGetSource(AFilename + '#state', FData)
      then AResponseInfo.ContentText := FData
      else AResponseInfo.ContentText := '{}';
  end
  else if ARequestInfo.CommandType = hcPOST then
  begin
    // Posted data is already decoded correctly
    FData := ARequestInfo.Params.Values['State'];
    RegisterSource(AFilename + '#state', FData, True);
  end;
end;

procedure TAppSourceHttpResponder.RespondTouchEditor(const AFilename: string;
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  FStyleSheet: IXMLDocument;
  root: IXMLNode;
  output: Widestring;
  FData: string;
begin
  // TODO: data will be passed separate call from touch editor later
  TKeymanSentryClient.Breadcrumb('default', Format('RespondTouchEditor: Filename=%s', [AFilename]), 'AppSourceHttpResponder');
  if not TryGetSource(AFilename, FData) then
  begin
    Respond404(AContext, ARequestInfo, AResponseInfo);
    Exit;
  end;

  with NewXMLDocument do
  begin
    root := AddChild('TouchLayoutBuilder');
    //root.AddChild('FileName').NodeValue := FBaseFileName;
    root.AddChild('LibPath').NodeValue := '/app/source/toucheditor/lib/';//ConvertPathToFileURL(GetLayoutBuilderPath);
    root.AddChild('LayoutJS').NodeValue := FData;
    FStyleSheet := LoadXMLDocument(GetLayoutBuilderPath + 'builder.xsl');
    root.TransformNode(FStyleSheet.DocumentElement, output);
  end;

  AResponseInfo.ContentType := 'text/html';
  AResponseInfo.Charset := 'UTF-8';
  AResponseInfo.ContentText := output;
end;

procedure TAppSourceHttpResponder.RespondTouchEditorLib(
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  doc: string;
begin
  doc := ARequestInfo.Document;
  Delete(doc, 1, Length('/app/source/toucheditor/lib/'));
  if Pos('..', doc) > 0 then
    Respond404(AContext, ARequestInfo, AResponseInfo)
  else
    RespondFile(GetLayoutBuilderPath + doc, AContext, ARequestInfo, AResponseInfo);
end;

procedure TAppSourceHttpResponder.SetSource(const Filename, Data: string);
begin
  Assert(TrySetSource(Filename, Data),
    'TAppSourceHttpResponder.SetSource was asked for a file that was not registered: '+Filename);
end;

function TAppSourceHttpResponder.TrySetSource(const Filename, Data: string): Boolean;
var
  i: Integer;
  T: TList<TAppSource>;
  S: TAppSource;
begin
  T := FSources.LockList;
  try
    TKeymanSentryClient.Breadcrumb('default', Format('TrySetSource: Filename=%s count=%d', [Filename, T.Count]), 'AppSourceHttpResponder');

    for i := 0 to T.Count - 1 do
      if T[i].Filename = Filename then
      begin
        S := T[i];
        S.Data := Data;
        T[i] := S;
        Exit(True);
      end;
  finally
    FSources.UnlockList;
  end;
  Result := False;
end;

procedure TAppSourceHttpResponder.RespondGet(const AFilename: string;
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  FData: string;
begin
  TKeymanSentryClient.Breadcrumb('default', Format('RespondGet: Filename=%s', [AFilename]), 'AppSourceHttpResponder');
  if TryGetSource(AFilename, FData) then
  begin
    AResponseInfo.ContentType := 'application/json';
    AResponseInfo.CharSet := 'UTF-8';
    AResponseInfo.ContentText := FData;
  end
  else
    Respond404(AContext, ARequestInfo, AResponseInfo);
end;

procedure TAppSourceHttpResponder.RespondPost(const AFilename: string;
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  if TrySetSource(AFilename, ARequestInfo.Params.Values['Data']) then
  begin
    AResponseInfo.ContentType := 'application/json';
    AResponseInfo.CharSet := 'UTF-8';
    AResponseInfo.ContentText := '{result: true}';
  end
  else
    Respond404(AContext, ARequestInfo, AResponseInfo);
end;

function TAppSourceHttpResponder.TryGetSource(const Filename: string; var Data: string): Boolean;
var
  i: Integer;
  T: TList<TAppSource>;
begin
  T := FSources.LockList;
  try
    TKeymanSentryClient.Breadcrumb('default', Format('TryGetSource: Filename=%s count=%d', [Filename, T.Count]), 'AppSourceHttpResponder');
    for i := 0 to T.Count - 1 do
      if T[i].Filename = Filename then
      begin
        Data := T[i].Data;
        Exit(True);
      end;
  finally
    FSources.UnlockList;
  end;
  Result := False;
end;

function TAppSourceHttpResponder.GetSource(const Filename: string): string;
begin
  TKeymanSentryClient.Breadcrumb('default', Format('GetSource: Filename=%s', [Filename]), 'AppSourceHttpResponder');
  Assert(TryGetSource(Filename, Result),
    'TAppSourceHttpResponder.GetSource was asked for a file that was not registered: '+Filename);
end;

function TAppSourceHttpResponder.IsSourceRegistered(const Filename: string): Boolean;
var
  Data: string;
begin
  TKeymanSentryClient.Breadcrumb('default', Format('IsSourceRegistered: Filename=%s', [Filename]), 'AppSourceHttpResponder');
  Result := TryGetSource(Filename, Data);
end;

procedure TAppSourceHttpResponder.RegisterSource(const Filename, Data: string; Update: Boolean);
var
  i: Integer;
  T: TList<TAppSource>;
  S: TAppSource;
begin
  T := FSources.LockList;
  try
    TKeymanSentryClient.Breadcrumb('default', Format('RegisterSource: Filename=%s Update=%s count=%d', [Filename, BoolToStr(Update,True), T.Count]), 'AppSourceHttpResponder');

    S.Filename := Filename;
    S.Data := Data;

    for i := 0 to T.Count - 1 do
      if T[i].Filename = Filename then
      begin
        if Update then
          T[i] := S
        else
          Assert(False, 'TAppSourceHttpResponder.RegisterSource was called for a file that is already registered: '+Filename);
        Exit;
      end;

    T.Add(S);
  finally
    FSources.UnlockList;
  end;
end;

procedure TAppSourceHttpResponder.UnregisterSource(const Filename: string);
var
  i: Integer;
  T: TList<TAppSource>;
begin
  T := FSources.LockList;
  try
    TKeymanSentryClient.Breadcrumb('default', Format('UnregisterSource: Filename=%s count=%d', [Filename, T.Count]), 'AppSourceHttpResponder');
    for i := 0 to T.Count - 1 do
      if T[i].Filename = Filename then
      begin
        T.Delete(i);
        Exit;
      end;
  finally
    FSources.UnlockList;
  end;

  Assert(False, 'TAppSourceHttpResponder.UnregisterSource was called for a file that is not registered: '+Filename);
end;

end.
