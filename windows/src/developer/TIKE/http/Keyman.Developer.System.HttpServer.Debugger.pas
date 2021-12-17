unit Keyman.Developer.System.HttpServer.Debugger;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  System.SyncObjs,
  System.SysUtils,
  Winapi.Windows,

  IdContext,
  IdCustomHTTPServer,
  IdHTTPServer,

  Keyman.System.HttpServer.Base,

  KeyboardFonts;


type
  EWebDebugCache = class(Exception)
    constructor Create;
  end;

  TWebDebugInfo = class
  private
    FLastAccess: TDateTime;
  public
    constructor Create;
    procedure LoadFromCache(o: TJSONObject); virtual;
    procedure SaveToCache(o: TJSONObject); virtual;
    function CompareLastAccess(R: TWebDebugInfo): Integer;
    property LastAccess: TDateTime read FLastAccess write FLastAccess;
  end;

  TWebDebugKeyboardInfo = class(TWebDebugInfo)   // I4063
  strict private
    FID: string;
    FPath: string;
    FVersion: string;   // I4260
    FFontName: TKeyboardFontArray;   // I4409
    FFontData: array[TKeyboardFont] of TStream;   // I4409
    procedure LoadFontData(const AFontName: string; Data: TStream);
    function GetStoredFilename: string;
    function GetJSONFilename: string;   // I4409
    function GetFontData(Index: TKeyboardFont): TStream;   // I4409
    function GetFontName(Index: TKeyboardFont): string;
    function GetWebFilename: string;
    function GetName: string;
  private
    FFilename: string;
  public
    constructor Create(const AFilename, AVersion: string; AFonts: TKeyboardFontArray);   // I4409
    constructor CreateFromCache(o: TJSONObject);
    procedure SaveToCache(o: TJSONObject); override;
    destructor Destroy; override;
    property ID: string read FID;
    property Name: string read GetName;
    property StoredFilename: string read GetStoredFilename;
    property WebFilename: string read GetWebFilename;
    property Version: string read FVersion;
    property JSONFilename: string read GetJSONFilename;   // I4260
    property FontName[Index: TKeyboardFont]: string read GetFontName;   // I4409
    property FontData[Index: TKeyboardFont]: TStream read GetFontData;   // I4409
  end;

  TWebDebugPackageInfo = class(TWebDebugInfo)
  strict private
    FFilename: string;
    FName: string;
  public
    constructor Create(const AFilename, AName: string);
    constructor CreateFromCache(o: TJSONObject);
    procedure SaveToCache(o: TJSONObject); override;
    property Filename: string read FFilename;
    property Name: string read FName;
  end;

  TWebDebugModelInfo = class(TWebDebugInfo)
  strict private
    FFilename: string;
  public
    constructor Create(const AFilename: string);
    constructor CreateFromCache(o: TJSONObject);
    procedure SaveToCache(o: TJSONObject); override;
    property Filename: string read FFilename;
  end;

  TDebuggerHttpResponder = class(TBaseHttpResponder)
  private
    FModelsCS, FKeyboardsCS, FPackagesCS: TCriticalSection;   // I4036
    FModels: TObjectDictionary<string,TWebDebugModelInfo>;
    FKeyboards: TObjectDictionary<string,TWebDebugKeyboardInfo>;   // I4063
    FPackages: TObjectDictionary<string,TWebDebugPackageInfo>;
    function AccessKeyboardFileNameFromStore(const WebFilename: string): string;
    function AccessPackageFileNameFromStore(const WebFilename: string): string;
    function AccessModelFileNameFromStore(const WebFilename: string): string;

    function GetCacheFileName: string;
    procedure LoadFromCache;
    procedure SaveToCache;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearCache;
    procedure RegisterKeyboard(const Filename, Version: string; FontInfo: TKeyboardFontArray);   // I4063   // I4409
    procedure UnregisterKeyboard(const Filename: string);
    procedure RegisterPackage(const Filename, Name: string);
    procedure UnregisterPackage(const Filename: string);
    procedure RegisterModel(const Filename: string);
    procedure UnregisterModel(const Filename: string);

    procedure ProcessRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

implementation

uses
  System.DateUtils,
  System.Generics.Defaults,
  System.Hash,
  System.StrUtils,
  System.TimeSpan,
  System.TypInfo,
  Vcl.Graphics,
  Winapi.ShlObj,

  RegExpr,
  IdGlobalProtocols,

  JsonUtil,
  KeymanDeveloperOptions,
  RedistFiles,
  RegistryKeys,
  Upload_Settings,
  utilsystem;

const
  C_MaxCachedObjects = 10;

const
  TestFontName: array[TKeyboardFont] of string = (   // I4409
    'code', 'char', 'osk', 'touch-phone', 'touch-tablet', 'touch-desktop'
  );

function IsStandardFont(const FontName: string): Boolean;   // I4448
const
  StandardFontNames: array[0..9] of string = (
    'Arial', 'Calibri', 'Consolas', 'Courier New', 'Lucida Console', 'Lucida Sans Unicode', 'Segoe UI', 'Tahoma', 'Times New Roman', 'Verdana'
    );
begin
  Result := AnsiIndexText(FontName, StandardFontNames) >= 0;
end;

{ TDebuggerHttpServer }

procedure TDebuggerHttpResponder.ClearCache;
begin
  FKeyboardsCS.Enter;
  try
    FKeyboards.Clear;
  finally
    FKeyboardsCS.Leave;
  end;

  FModelsCS.Enter;
  try
    FModels.Clear;
  finally
    FModelsCS.Leave;
  end;

  FPackagesCS.Enter;
  try
    FPackages.Clear;
  finally
    FPackagesCS.Leave;
  end;
end;

constructor TDebuggerHttpResponder.Create;
begin
  FKeyboardsCS := TCriticalSection.Create;   // I4036
  FKeyboards := TObjectDictionary<string,TWebDebugKeyboardInfo>.Create;   // I4063

  FPackagesCS := TCriticalSection.Create;
  FPackages := TObjectDictionary<string,TWebDebugPackageInfo>.Create;   // I4063

  FModelsCS := TCriticalSection.Create;
  FModels := TObjectDictionary<string,TWebDebugModelInfo>.Create;   // I4063

  LoadFromCache;
end;

destructor TDebuggerHttpResponder.Destroy;
begin
  SaveToCache;

  FreeAndNil(FKeyboards);
  FreeAndNil(FKeyboardsCS);   // I4036

  FreeAndNil(FPackages);
  FreeAndNil(FPackagesCS);   // I4036

  FreeAndNil(FModels);
  FreeAndNil(FModelsCS);   // I4036

  inherited Destroy;
end;

function TDebuggerHttpResponder.GetCacheFileName: string;
begin
  Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\WebDebugCache.json';
end;

function TDebuggerHttpResponder.AccessKeyboardFileNameFromStore(
  const WebFilename: string): string;
begin
  FKeyboardsCS.Enter;   // I4036
  try
    if FKeyboards.ContainsKey(WebFilename) then
    begin
      FKeyboards[WebFilename].LastAccess := Now;
      Result := FKeyboards[WebFilename].StoredFilename
    end
    else
      Result := '';
  finally
    FKeyboardsCS.Leave;
  end;
end;

function TDebuggerHttpResponder.AccessModelFileNameFromStore(
  const WebFilename: string): string;
begin
  FModelsCS.Enter;   // I4036
  try
    if FModels.ContainsKey(WebFilename) then
    begin
      FModels[WebFilename].LastAccess := Now;
      Result := FModels[WebFilename].Filename
    end
    else Result := '';
  finally
    FModelsCS.Leave;
  end;
end;

function TDebuggerHttpResponder.AccessPackageFileNameFromStore(
  const WebFilename: string): string;
begin
  FPackagesCS.Enter;   // I4036
  try
    if FPackages.ContainsKey(WebFilename) then
    begin
      FPackages[WebFilename].LastAccess := Now;
      Result := FPackages[WebFilename].Filename
    end
    else Result := '';
  finally
    FPackagesCS.Leave;
  end;
end;

//
// Modified from System.DateUtils.DateToISO8601
// This does ISO8601 Date+Time+TZ without msec, which
// is what Swift 4.0 demands...
//
function FormatFinnickyISO8601Date(const ADate: TDateTime; AInputIsUTC: Boolean = true): string;
const
  SDateFormat: string = 'yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''Z'''; { Do not localize }
  SOffsetFormat: string = '%s%s%.02d:%.02d'; { Do not localize }
  Neg: array[Boolean] of string = ('+', '-'); { Do not localize }
var
  Bias: Integer;
  TimeZone: TTimeZone;
begin
  Result := FormatDateTime(SDateFormat, ADate);
  if not AInputIsUTC then
  begin
    TimeZone := TTimeZone.Local;
    Bias := Trunc(TimeZone.GetUTCOffset(ADate).Negate.TotalMinutes);
    if Bias <> 0 then
    begin
      // Remove the Z, in order to add the UTC_Offset to the string.
      SetLength(Result, Result.Length - 1);
      Result := Format(SOffsetFormat, [Result, Neg[Bias > 0], Abs(Bias) div MinsPerHour,
        Abs(Bias) mod MinsPerHour]);
    end
  end;
end;

procedure TDebuggerHttpResponder.ProcessRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

  procedure Respond404;
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ResponseText := 'File not found';
  end;

  procedure RespondProject;
  begin
    AResponseInfo.ContentText := 'ooh';
  end;

  procedure RespondKeyboardsCSS;   // I4063
  var
    key, name: string;
    value: TWebDebugKeyboardInfo;
    response: string;
    i: TKeyboardFont;   // I4409
  begin
    // list dynamic fonts
    response := '/* Dynamic fonts */';

    FKeyboardsCS.Enter;
    try
      for key in FKeyboards.Keys do
      begin
        name := ChangeFileExt(key, '');
        value := FKeyboards[key];

        for i := Low(TKeyboardFont) to High(TKeyboardFont) do   // I4409
          if (value.FontName[i] <> '') and not IsStandardFont(value.FontName[i]) then   // I4448
            response := response +
              '@font-face { '#13#10+
              '  font-family: "'+value.FontName[i]+'";'#13#10+
              '  src: local("'+value.FontName[i]+'"),'#13#10+
              '       url("/font/'+name+'-'+TestFontName[i]+'.ttf") format("truetype");'#13#10+
              '  font-weight: normal;'#13#10+
              '  font-style: normal;'#13#10+
              '}'#13#10#13#10;

      end;
    finally
      FKeyboardsCS.Leave;
    end;

    AResponseInfo.ContentType := 'text/css';
    AResponseInfo.CharSet := 'UTF-8';   // I4449
    AResponseInfo.ContentText := response;

    // Keyboard list always expire immediately
    AResponseInfo.Expires := EncodeDate(1990, 1, 1);
    AResponseInfo.CacheControl := 'no-cache, no-store';
    AResponseInfo.LastModified := Now;
  end;

  procedure RespondKeyboardsJS;
  var
    name: string;
    key: string;
    response: string;
    value: TWebDebugKeyboardInfo;
    id, src: string;
    n: Integer;
    srcVersion: string;
    model: string;
    sha: string;
  begin
    // Get dynamic keyboard registration

    response :=
      '(function() {'#13#10+
      '  var kmw=KeymanWeb;'#13#10;

    FKeyboardsCS.Enter;   // I4036
    try
      for key in FKeyboards.Keys do
      begin
        src := ChangeFileExt(key, '');   // I4140
        n := Pos('-', src);
        if n > 0 then
        begin
          name := Copy(src, 1, n-1);
          srcVersion := Copy(src, n+1, MaxInt);
        end
        else
        begin
          name := src;
          srcVersion := '1.0';
        end;

        value := FKeyboards[key];
        try
          sha := THashSHA2.GetHashStringFromFile(value.StoredFilename);
        except
          on E:Exception do sha := E.Message;
        end;
        response := response + '  kmw.KRS({'+#13#10+
          '    KN:"'+name+'",'#13#10+
          '    KI:"Keyboard_'+name+'",'#13#10+
          '    KL:"'+name+'",'#13#10+
          '    KLC:"en",'#13#10+
          '    KR:"Europe",'#13#10+
          '    KRC:"eu",'#13#10+
          '    KFont:{family:"'+value.FontName[kfontChar]+'"},'#13#10+   // I4063   // I4409
          '    KOskFont:{family:"'+value.FontName[kfontOSK]+'"},'#13#10+   // I4063   // I4409
          '    KF:"'+src+'.js"'#13#10+   // I4140
          '  }); /*sha256='+sha+'*/'#13#10+
          '  debugKeyboards["'+name+'"] = {'+#13#10+   // I4260
          '    id:"'+name+'",'#13#10+
          '    version:"'+srcVersion+'"'#13#10+
          '  };';
      end;
    finally
      FKeyboardsCS.Leave;
    end;

    FModelsCS.Enter;
    try
      for model in FModels.Keys do
      begin
        id := ChangeFileExt(model, '');
        try
          sha := THashSHA2.GetHashStringFromFile(FModels[model].Filename);
        except
          on E:Exception do sha := E.Message;
        end;
        response := response + Format('registerModel("%s", "%s"); /*sha256=%s*/'#13#10, [id, model, sha]);
      end;
    finally
      FModelsCS.Leave;
    end;

    response := response + '})();';

    AResponseInfo.CharSet := 'UTF-8';
    AResponseInfo.ContentType := 'application/javascript';
    AResponseInfo.ContentText := response;

    // Keyboard list always expire immediately
    AResponseInfo.Expires := EncodeDate(1990, 1, 1);   // I4037
    AResponseInfo.CacheControl := 'no-cache, no-store';   // I4037
    AResponseInfo.LastModified := Now;   // I4037
  end;

  procedure RespondPackagesJSON;
  var
    json: TJSONObject;
    jsonArray: TJSONArray;
    jsonPackage: TJSONObject;
    key: string;
    jsonUrls: TJSONObject;
  begin
    // Get dynamic keyboard registration
    FPackagesCS.Enter;   // I4036
    try

      json := TJSONObject.Create;
      try
        try
          jsonArray := TJSONArray.Create;

          for key in FPackages.Keys do
          begin
            jsonPackage := TJSONObject.Create;
            jsonPackage.AddPair('id', key);
            jsonPackage.AddPair('filename', FPackages[key].Filename);
            jsonPackage.AddPair('name', FPackages[key].Name);
            jsonArray.Add(jsonPackage);
          end;

          json.AddPair('packages', jsonArray);

          jsonUrls := TJSONObject.Create;
          jsonUrls.AddPair('installLinkAndroid', MakeKeymanURL(URLPath_KeymanDeveloper_KeymanForAndroidDownload));
          jsonUrls.AddPair('installLinkIos', MakeKeymanURL(URLPath_KeymanDeveloper_KeymanForIosDownload));
          json.AddPair('urls', jsonUrls);

          AResponseInfo.CharSet := 'UTF-8';
          AResponseInfo.ContentType := 'application/json';
          AResponseInfo.ContentText := JSONToString(json, True);

          // Keyboard JSON always expire immediately
          AResponseInfo.Expires := EncodeDate(1990, 1, 1);
          AResponseInfo.CacheControl := 'no-cache, no-store';
          AResponseInfo.LastModified := Now;
        finally
          json.Free;
        end;
      except
        on E:Exception do
        begin
          Respond404;
          Exit;
        end;
      end;
    finally
      FPackagesCS.Leave;
    end;
  end;

  procedure RespondFont(const src: string);   // I4063
  var
    name, key: string;
    value: TWebDebugKeyboardInfo;
    FontDataStream: TStream;
    Found: Boolean;
    I: TKeyboardFont;
  begin
    FontDataStream := nil;
    FKeyboardsCS.Enter;
    try
      for key in FKeyboards.Keys do
      begin
        name := ChangeFileExt(key, '');
        value := FKeyboards[key];
        Found := False;
        for I := Low(TKeyboardFont) to High(TKeyboardFont) do   // I4409
          if name+'-'+TestFontName[I]+'.ttf' = src then
          begin
            FontDataStream := value.FontData[I];
            Found := True;
            Break;
          end;
        if Found then
          Break;
      end;

      if Assigned(FontDataStream) then
      begin
        AResponseInfo.ContentType := 'application/octet-stream';
        AResponseInfo.ContentLength := FontDataStream.Size;
      //AResponseInfo.LastModified := GetFileDate(doc);
        AResponseInfo.WriteHeader;

        AContext.Connection.IOHandler.Write(FontDataStream);
      end
      else
        Respond404;

    finally
      FKeyboardsCS.Leave;
    end;
  end;

  procedure RespondKeyboardJson(filename: string);   // I4260
  var
    JSON: TJSONObject;
    jsonOptions: TJSONObject;
    value: TWebDebugKeyboardInfo;
    FKeyboardBaseUri: string;
    FFontBaseUri: string;
    jsonKeyboard: TJSONObject;
    jsonLanguages: TJSONArray;
    jsonLanguage, jsonFont, jsonOskFont: TJSONObject;
  begin
    if SameText(Copy(filename, Length(filename)-4, 5), '.json') then   // I4437
    begin
      Delete(filename, Length(filename)-1, 2); // look for .js
    end;

    FKeyboardsCS.Enter;
    try
      if not FKeyboards.ContainsKey(filename) then
      begin
        Respond404;
        Exit;
      end;

      value := FKeyboards[filename];

      // Build the JSON response

      json := TJSONObject.Create;
      try
        jsonOptions := TJSONObject.Create;
        jsonKeyboard := TJSONObject.Create;

        json.AddPair('options', jsonOptions)
            .AddPair('keyboard', jsonKeyboard);

        FKeyboardBaseUri := Format('http://%s/keyboard/',[ARequestInfo.Host]);
        FFontBaseUri := Format('http://%s/font/',[ARequestInfo.Host]);

        jsonOptions.AddPair('device', 'any');
        jsonOptions.AddPair('keyboardBaseUri', FKeyboardBaseUri);
        jsonOptions.AddPair('fontBaseUri', FFontBaseUri);

        jsonFont := TJSONObject.Create;
        jsonFont.AddPair('family', value.FontName[kfontChar]);   // I4409
        jsonFont.AddPair('source', value.ID+'-'+value.Version+'-'+TestFontName[kfontChar]+'.ttf');   // I4409
        jsonKeyboard.AddPair('font', jsonFont);

        jsonOskFont := TJSONObject.Create;
        jsonOskFont.AddPair('family', value.FontName[kfontOSK]);   // I4409
        jsonOskFont.AddPair('source', value.ID+'-'+value.Version+'-'+TestFontName[kfontOSK]+'.ttf');   // I4409
        jsonKeyboard.AddPair('oskFont', jsonOskFont);

        jsonKeyboard.AddPair('id', value.ID);
        jsonKeyboard.AddPair('name', value.Name);
        jsonKeyboard.AddPair('filename', value.WebFilename);
        jsonKeyboard.AddPair('version', value.Version);
        jsonKeyboard.AddPair('lastModified', FormatFinnickyISO8601Date(Now, False));

        jsonLanguages := TJSONArray.Create;
        jsonKeyboard.AddPair('languages', jsonLanguages);

        jsonLanguage := TJSONObject.Create;
        jsonLanguage.AddPair('id', 'en');
        jsonLanguage.AddPair('name', 'English');
        jsonLanguages.Add(jsonLanguage);

        try
          AResponseInfo.CharSet := 'UTF-8';
          AResponseInfo.ContentType := 'application/javascript';
          AResponseInfo.ContentText := JSONToString(json, True);

          // Keyboard JSON always expire immediately
          AResponseInfo.Expires := EncodeDate(1990, 1, 1);
          AResponseInfo.CacheControl := 'no-cache, no-store';
          AResponseInfo.LastModified := Now;
        finally
          json.Free;
        end;
      except
        on E:Exception do
        begin
          Respond404;
          Exit;
        end;
      end;
    finally
      FKeyboardsCS.Leave;
    end;
  end;

var
  doc: string;
  FFileRegExp: TRegExpr;
  FResourceFileRegExp: TRegExpr;
begin
  //    /keyboard/###.js -> looks up the list of currently testing keyboards
  //    everything else retrieved from xml/kmw/
  doc := ARequestInfo.Document;
  Delete(doc, 1, 1);

  if doc = '' then
    doc := 'index.html';

  if doc = 'inc/keyboards.js' then
  begin
    RespondKeyboardsJS;
    Exit;
  end;

  if doc = 'inc/keyboards.css' then   // I4063
  begin
    RespondKeyboardsCSS;
    Exit;
  end;

  if doc = 'inc/packages.json' then
  begin
    RespondPackagesJSON;
    Exit;
  end;

  FFileRegExp := TRegExpr.Create;   // I4036
  FResourceFileRegExp := TRegExpr.Create;   // I4036
  try
    FFileRegExp.Expression := '^[a-z0-9_.-]+$';
    FFileRegExp.ModifierI := True;

    FResourceFileRegExp.Expression := '^([a-z]+\/)*[a-z0-9_.-]+$';
    FResourceFileRegExp.ModifierI := True;

    if Copy(doc, 1, 9) = 'resource/' then
    begin
      Delete(doc, 1, 9);

      if not FResourceFileRegExp.Exec(doc) then
      begin
        Respond404;
        Exit;
      end;

      doc := GetXMLTemplatePath + 'kmw\resource\' + ReplaceText(doc, '/', '\');
    end
    else if Copy(doc, 1, 5) = 'font/' then   // I4063
    begin
      Delete(doc, 1, 5);
      RespondFont(doc);
    end
    else if Copy(doc, 1, 10) = 'kbinstall/' then   // I4260
    begin
      Delete(doc, 1, 10);

      // Keyboards always expire immediately
      AResponseInfo.Expires := EncodeDate(1990, 1, 1);   // I4037
      AResponseInfo.CacheControl := 'no-cache, no-store';   // I4037
      AResponseInfo.LastModified := Now;   // I4037
      if not FFileRegExp.Exec(doc) then
      begin
        Respond404;
        Exit;
      end;
      RespondKeyboardJson(doc);
      Exit;
    end
    else if Copy(doc, 1, 8) = 'package/' then
    begin
      Delete(doc, 1, 8);

      // Packages always expire immediately
      AResponseInfo.Expires := EncodeDate(1990, 1, 1);   // I4037
      AResponseInfo.CacheControl := 'no-cache, no-store';   // I4037
      AResponseInfo.LastModified := Now;   // I4037

      if not FFileRegExp.Exec(doc) then
      begin
        // We don't allow files with invalid names to be sent to
        // Android and iOS. TODO: We should probably consider giving
        // a friendly error :)
        Respond404;
        Exit;
      end;
      doc := AccessPackageFileNameFromStore(doc);
      if doc = '' then
      begin
        Respond404;
        Exit;
      end;
    end
    else if Copy(doc, 1, 9) = 'keyboard/' then
    begin
      Delete(doc, 1, 9);

      // Keyboards always expire immediately
      AResponseInfo.Expires := EncodeDate(1990, 1, 1);   // I4037
      AResponseInfo.CacheControl := 'no-cache, no-store';   // I4037
      AResponseInfo.LastModified := Now;   // I4037
      if not FFileRegExp.Exec(doc) then
      begin
        Respond404;
        Exit;
      end;
      doc := AccessKeyboardFileNameFromStore(doc);
      if doc = '' then
      begin
        Respond404;
        Exit;
      end;
    end
    else if Copy(doc, 1, 6) = 'model/' then
    begin
      Delete(doc, 1, 6);

      // Models always expire immediately
      AResponseInfo.ContentType := 'application/javascript';
      AResponseInfo.Expires := EncodeDate(1990, 1, 1);   // I4037
      AResponseInfo.CacheControl := 'no-cache, no-store';   // I4037
      AResponseInfo.LastModified := Now;   // I4037
      if not FFileRegExp.Exec(doc) then
      begin
        Respond404;
        Exit;
      end;
      doc := AccessModelFileNameFromStore(doc);
      if doc = '' then
      begin
        Respond404;
        Exit;
      end;
    end
    else
    begin
      if not FFileRegExp.Exec(doc) then
      begin
        Respond404;
        Exit;
      end;

      doc := GetXMLTemplatePath + 'kmw\' + doc;
    end;

    if not FileExists(doc) then
    begin
      Respond404;
      Exit;
    end;

    // Serve the file

    if AResponseInfo.ContentType = '' then
    begin
      AResponseInfo.HTTPServer.MIMETable.LoadTypesFromOS := False;
      AResponseInfo.ContentType :=  AResponseInfo.HTTPServer.MIMETable.GetFileMIMEType(doc);
    end;
    AResponseInfo.CharSet := 'UTF-8';
    AResponseInfo.ContentLength := FileSizeByName(doc);
  //AResponseInfo.LastModified := GetFileDate(doc);
    AResponseInfo.WriteHeader;

    AContext.Connection.IOHandler.WriteFile(doc);
  finally
    FreeAndNil(FFileRegExp);   // I4036
    FreeAndNil(FResourceFileRegExp);   // I4036
  end;
end;

procedure TDebuggerHttpResponder.RegisterKeyboard(const Filename, Version: string; FontInfo: TKeyboardFontArray);   // I4063   // I4409
var
  k: TWebDebugKeyboardInfo;
begin
  k := TWebDebugKeyboardInfo.Create(Filename, Version, FontInfo);
  FKeyboardsCS.Enter;   // I4036
  try
    FKeyboards.AddOrSetValue(k.WebFilename, k);   // I4063
  finally
    FKeyboardsCS.Leave;
  end;
end;

procedure TDebuggerHttpResponder.RegisterModel(const Filename: string);
var
  m: TWebDebugModelInfo;
begin
  m := TWebDebugModelInfo.Create(Filename);
  FModelsCS.Enter;
  try
    FModels.AddOrSetValue(ExtractFileName(Filename), m);
  finally
    FModelsCS.Leave;
  end;
end;

procedure TDebuggerHttpResponder.RegisterPackage(const Filename, Name: string);
var
  p: TWebDebugPackageInfo;
begin
  p := TWebDebugPackageInfo.Create(Filename, Name);
  FPackagesCS.Enter;
  try
    FPackages.AddOrSetValue(ExtractFileName(Filename), p);
  finally
    FPackagesCS.Leave;
  end;
end;

procedure TDebuggerHttpResponder.UnregisterKeyboard(const Filename: string);
begin
  FKeyboardsCS.Enter;   // I4036
  try
    FKeyboards.Remove(ExtractFileName(Filename));
  finally
    FKeyboardsCS.Leave;
  end;
end;

procedure TDebuggerHttpResponder.UnregisterModel(const Filename: string);
begin
  FModelsCS.Enter;   // I4036
  try
    FModels.Remove(ExtractFileName(Filename));
  finally
    FModelsCS.Leave;
  end;
end;

procedure TDebuggerHttpResponder.UnregisterPackage(const Filename: string);
begin
  FPackagesCS.Enter;   // I4036
  try
    FPackages.Remove(ExtractFileName(Filename));
  finally
    FPackagesCS.Leave;
  end;
end;

//
// Caching of keyboards, models and packages under test
//


procedure TDebuggerHttpResponder.LoadFromCache;
  procedure LoadPackagesFromCache(a: TJSONArray);
  var
    i: Integer;
    package: TWebDebugPackageInfo;
  begin
    for i := 0 to a.Count - 1 do
    begin
      if a.Items[i] is TJSONObject then
      begin
        try
          package := TWebDebugPackageInfo.CreateFromCache(a.Items[i] as TJSONObject);
        except
          on E:EWebDebugCache do Continue;
        end;
        if not FileExists(package.Filename) then
        begin
          package.Free;
          Continue;
        end;
        Self.FPackages.AddOrSetValue(ExtractFileName(package.Filename), package);
      end;
    end;
  end;

  procedure LoadModelsFromCache(a: TJSONArray);
  var
    i: Integer;
    model: TWebDebugModelInfo;
  begin
    for i := 0 to a.Count - 1 do
    begin
      if a.Items[i] is TJSONObject then
      begin
        try
          model := TWebDebugModelInfo.CreateFromCache(a.Items[i] as TJSONObject);
        except
          on E:EWebDebugCache do Continue;
        end;
        if not FileExists(model.Filename) then
        begin
          model.Free;
          Continue;
        end;
        Self.FModels.AddOrSetValue(ExtractFileName(model.Filename), model);
      end;
    end;
  end;

  procedure LoadKeyboardsFromCache(a: TJSONArray);
  var
    i: Integer;
    keyboard: TWebDebugKeyboardInfo;
  begin
    for i := 0 to a.Count - 1 do
    begin
      if a.Items[i] is TJSONObject then
      begin
        try
          keyboard := TWebDebugKeyboardInfo.CreateFromCache(a.Items[i] as TJSONObject);
        except
          on E:EWebDebugCache do Continue;
        end;
        if not FileExists(keyboard.FFilename) then
        begin
          keyboard.Free;
          Continue;
        end;
        Self.FKeyboards.AddOrSetValue(keyboard.WebFilename, keyboard);
      end;
    end;
  end;

var
  v: TJSONValue;
  json: TJSONObject;
  ss: TStringStream;
  a: TJSONArray;
  offset: Integer;
begin
  if not FileExists(GetCacheFileName) then
    Exit;

  ss := TStringStream.Create('', TEncoding.UTF8);
  try
    ss.LoadFromFile(Self.GetCacheFileName);
    offset := 0;
    v := ParseJSONValue(ss.DataString, offset);
    if not (v is TJSONObject) then
      Exit;
  finally
    ss.Free;
  end;

  json := v as TJSONObject;

  if json.TryGetValue<TJSONArray>('packages', a) then
  begin
    LoadPackagesFromCache(a);
  end;

  if json.TryGetValue<TJSONArray>('models', a) then
  begin
    LoadModelsFromCache(a);
  end;

  if json.TryGetValue<TJSONArray>('keyboards', a) then
  begin
    LoadKeyboardsFromCache(a);
  end;
end;

type
  TListReducer<T:TWebDebugInfo> = class
    class procedure Save(json: TJSONObject;
      const name: string;
      const Collection: TObjectDictionary<string,T>);
  end;

///
/// Reduce a list of cache objects to a maximum of 10 items
/// so that we don't overwhelm the user over time; use the
/// ten most recently loaded items based on LastAccess property
///
class procedure TListReducer<T>.Save(
  json: TJSONObject;
  const name: string;
  const Collection: TObjectDictionary<string,T>);
var
  item: T;
  list: TObjectList<T>;
  o: TJSONObject;
  a: TJSONArray;
begin
  list := TObjectList<T>.Create(False);
  try
    list.AddRange(Collection.Values);
    list.Sort(TComparer<T>.Construct(
      function (const L, R: T): integer
      begin
       Result := L.CompareLastAccess(R);
      end
    ));

    while list.Count > C_MaxCachedObjects do
      list.Delete(0);

    a := TJSONArray.Create;
    json.AddPair(name, a);

    // Use the original collection to
    // maintain the used order
    for item in Collection.Values do
    begin
      if list.Contains(item) then
      begin
        o := TJSONObject.Create;
        a.Add(o);
        item.SaveToCache(o);
      end;
    end;
  finally
    list.Free;
  end;
end;

procedure TDebuggerHttpResponder.SaveToCache;
var
  json: TJSONObject;
  ss: TStringStream;
  strings: TStringList;
begin
  json := TJSONObject.Create;
  try
    TListReducer<TWebDebugPackageInfo>.Save(json, 'packages', FPackages);
    TListReducer<TWebDebugKeyboardInfo>.Save(json, 'keyboards', FKeyboards);
    TListReducer<TWebDebugModelInfo>.Save(json, 'models', FModels);

    strings := TStringList.Create;
    try
      PrettyPrintJSON(json, strings, 2);
      ss := TStringStream.Create(strings.Text, TEncoding.UTF8);
      try
        ss.SaveToFile(Self.GetCacheFileName);
      finally
        ss.Free;
      end;
    finally
      strings.Free;
    end;
  finally
    json.Free;
  end;
end;


{ TWebDebugKeyboardInfo }

constructor TWebDebugKeyboardInfo.Create(const AFilename, AVersion: string; AFonts: TKeyboardFontArray);   // I4409
var
  I: TKeyboardFont;
begin
  inherited Create;
  FFilename := AFilename;
  FID := ChangeFileExt(ExtractFileName(AFilename), '');
  FVersion := AVersion;
  FPath := ExtractFilePath(AFilename);
  for I := Low(TKeyboardFont) to High(TKeyboardFont) do
  begin
    FFontName[I] := AFonts[I];
    if FFontName[I] <> '' then
    begin
      FFontData[I] := TMemoryStream.Create;
      LoadFontData(FFontName[i], FFontData[i]);
    end
    else
      FFontData[i] := nil;
  end;
end;

constructor TWebDebugKeyboardInfo.CreateFromCache(o: TJSONObject);
var
  fonts: TKeyboardFontArray;
  kf: TKeyboardFont;
  filename, version: string;
begin
  for kf := Low(TKeyboardFont) to High(TKeyboardFont) do
  begin
    if not o.TryGetValue<string>('font' + KeyboardFontId[kf], fonts[kf]) then
      fonts[kf] := '';
  end;

  if not o.TryGetValue<string>('filename', filename) or
      not o.TryGetValue<string>('version', version) then
    raise EWebDebugCache.Create;

  Create(filename, version, fonts);
  inherited LoadFromCache(o);
end;

procedure TWebDebugKeyboardInfo.SaveToCache(o: TJSONObject);
var
  kf: TKeyboardFont;
begin
  o.AddPair('filename', Self.FFilename);
  o.AddPair('version', Self.FVersion);
  for kf := Low(TKeyboardFont) to High(TKeyboardFont) do
  begin
    o.AddPair('font' + KeyboardFontId[kf], Self.FFontName[kf]);
  end;
  inherited SaveToCache(o);
end;

destructor TWebDebugKeyboardInfo.Destroy;   // I4063
var
  I: TKeyboardFont;
begin
  for I := Low(TKeyboardFont) to High(TKeyboardFont) do   // I4409
    FreeAndNil(FFontData[I]);
  inherited Destroy;
end;

function TWebDebugKeyboardInfo.GetStoredFilename: string;
begin
  Result := FPath + FID + '.js';
end;

function TWebDebugKeyboardInfo.GetWebFilename: string;
begin
  Result := FID + '-' + FVersion + '.js';
end;

function TWebDebugKeyboardInfo.GetFontData(Index: TKeyboardFont): TStream;   // I4409
begin
  Result := FFontData[Index];
  while not Assigned(Result) do
  begin
    Dec(Index);
    Result := FFontData[Index];
  end;
end;

function TWebDebugKeyboardInfo.GetFontName(Index: TKeyboardFont): string;   // I4409
begin
  Result := FFontName[Index];
end;

function TWebDebugKeyboardInfo.GetJSONFilename: string;
begin
  Result := FID + '-' + FVersion + '.json';
end;

function TWebDebugKeyboardInfo.GetName: string;
begin
  Result := FID;
end;

procedure TWebDebugKeyboardInfo.LoadFontData(const AFontName: string;
  Data: TStream);   // I4063
var
  sz: DWord;
  FHDC: THandle;
  Canvas: TCanvas;
begin
  if AFontName = '' then
    Exit;

  Canvas := TCanvas.Create;
  try
    FHDC := GetDC(GetDesktopWindow);
    try
      Canvas.Handle := FHDC;
      try
        Canvas.Font.Name := AFontName;
        Canvas.TextExtent('A'); // This forces a csFontValid   // I4824

        sz := Winapi.Windows.GetFontData(Canvas.Handle, 0, 0, nil, 0);
        if sz = GDI_ERROR then Exit;

        Data.Size := sz;
        Data.Position := 0;
        if Winapi.Windows.GetFontData(Canvas.Handle, 0, 0, (Data as TMemoryStream).Memory, sz) = GDI_ERROR then
        begin
          Data.Size := 0;
          Exit;
        end;
      finally
        Canvas.Handle := 0;
      end;
    finally
      ReleaseDC(GetDesktopWindow, FHDC);
    end;
  finally
    Canvas.Free;
  end;
end;

{ TWebDebugPackageInfo }

constructor TWebDebugPackageInfo.Create(const AFilename, AName: string);
begin
  inherited Create;
  FFilename := AFilename;
  FName := AName;
end;

constructor TWebDebugPackageInfo.CreateFromCache(o: TJSONObject);
var
  filename, name: string;
begin
  if not o.TryGetValue<string>('filename', filename) or
      not o.TryGetValue<string>('name', name) then
    raise EWebDebugCache.Create;

  Create(filename, name);
  inherited LoadFromCache(o);
end;

procedure TWebDebugPackageInfo.SaveToCache(o: TJSONObject);
begin
  o.AddPair('filename', FFilename);
  o.AddPair('name', FName);
  inherited SaveToCache(o);
end;

{ TWebDebugModelInfo }

constructor TWebDebugModelInfo.Create(const AFilename: string);
begin
  inherited Create;
  FFilename := AFilename;
end;

constructor TWebDebugModelInfo.CreateFromCache(o: TJSONObject);
var
  filename: string;
begin
  if not o.TryGetValue<string>('filename', filename) then
    raise EWebDebugCache.Create;

  Create(filename);
  inherited LoadFromCache(o);
end;

procedure TWebDebugModelInfo.SaveToCache(o: TJSONObject);
begin
  o.AddPair('filename', FFilename);
  inherited SaveToCache(o);
end;

{ EWebDebugCache }

constructor EWebDebugCache.Create;
begin
  inherited Create('Invalid cache data');
end;

{ TWebDebugInfo }

function TWebDebugInfo.CompareLastAccess(R: TWebDebugInfo): Integer;
begin
  if Self.LastAccess = R.LastAccess then
    Result := 0
  else if Self.LastAccess < R.LastAccess then
    Result := -1
  else
    Result := 1;
end;

constructor TWebDebugInfo.Create;
begin
  inherited Create;
  FLastAccess := Now;
end;

procedure TWebDebugInfo.LoadFromCache(o: TJSONObject);
var
  s: string;
begin
  if o.TryGetValue<string>('lastAccess', s) then
    Self.FLastAccess := ISO8601ToDate(s);
end;

procedure TWebDebugInfo.SaveToCache(o: TJSONObject);
begin
  o.AddPair('lastAccess', FormatFinnickyISO8601Date(FLastAccess));
end;

end.
