(*
  Name:             UmodWebHttpServer
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      7 Feb 2014

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          07 Feb 2014 - mcdurdin - I4036 - V9.0 - Keyboard Debug HTTP Host should be thread safe
                    07 Feb 2014 - mcdurdin - I4037 - V9.0 - Keyboard HTTP debugger sometimes caches old keyboards
                    21 Feb 2014 - mcdurdin - I4063 - V9.0 - Web Debugger needs to embed fonts for OSK and text area
                    19 Mar 2014 - mcdurdin - I4140 - V9.0 - Add keyboard version information to keyboards
                    10 Jun 2014 - mcdurdin - I4260 - V9.0 - Add link to install keyboard file into native app into debug web
                    25 Sep 2014 - mcdurdin - I4409 - V9.0 - Wrong font selected in keyboard debugger touch layout
                    10 Oct 2014 - mcdurdin - I4437 - V9.0 - JSON hosting for installing keyboard into native app has wrong extension
                    13 Oct 2014 - mcdurdin - I4448 - V9.0 - Standard fonts should not be downloaded to devices in test window
                    13 Oct 2014 - mcdurdin - I4449 - V9.0 - Font CSS responds as ISO-8859-1 instead of UTF-8
                    27 May 2015 - mcdurdin - I4304 - Keyman Developer fails to start if web debugger port is in use [CrashID:tike.exe_9.0.449.0_0060A38C_EIdSocketError]
                    03 Aug 2015 - mcdurdin - I4824 - Fonts don't always load correctly in KeymanWeb test page
                    24 Aug 2015 - mcdurdin - I4876 - Default "en" language should be added if missing during debug to JSON
*)
unit UmodWebHttpServer;

interface

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils,
  System.Generics.Collections,

  System.SyncObjs,

  Winapi.Windows,

  IdBaseComponent,
  IdComponent,
  IdContext,
  IdCustomTCPServer,
  IdCustomHTTPServer,
  IdHTTPServer,

  KeyboardFonts,
  RegExpr;

type
  TWebDebugKeyboardInfo = class   // I4063
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
  public
    constructor Create(const AFilename, AVersion: string; AFonts: TKeyboardFontArray);   // I4409
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

  TmodWebHttpServer = class(TDataModule)
    http: TIdHTTPServer;
    procedure httpCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FKeyboardsCS: TCriticalSection;   // I4036
    FKeyboards: TObjectDictionary<string,TWebDebugKeyboardInfo>;   // I4063
    function GetKeyboardStoredFileName(const WebFilename: string): string;
  public
    procedure RegisterKeyboard(const Filename, Version: string; FontInfo: TKeyboardFontArray);   // I4063   // I4409
    procedure UnregisterKeyboard(const Filename: string);
    function GetURL: string;
    procedure GetURLs(v: TStrings);
  end;

var
  modWebHttpServer: TmodWebHttpServer;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses
  IdException,
  IdGlobal,
  IdGlobalProtocols,
  IdStack,

  System.DateUtils,
  System.JSON,
  System.TimeSpan,
  System.TypInfo,

  Vcl.Dialogs,
  Vcl.Graphics,

  JsonUtil,
  KeymanDeveloperOptions,
  RedistFiles;

{$R *.dfm}

const
  TestFontName: array[TKeyboardFont] of string = (   // I4409
    'code', 'char', 'osk', 'touch-phone', 'touch-tablet', 'touch-desktop'
  );

procedure TmodWebHttpServer.DataModuleCreate(Sender: TObject);
begin
  FKeyboardsCS := TCriticalSection.Create;   // I4036
  FKeyboards := TObjectDictionary<string,TWebDebugKeyboardInfo>.Create;   // I4063

  http.DefaultPort := FKeymanDeveloperOptions.WebHostDefaultPort;

  try
    http.Active := True;
  except
    on E:EIdSocketError do   // I4304
    begin
      ShowMessage('The Keyman Developer debug web server was unable to start. '+
        'This is usually caused by a firewall blocking rule or another process '+
        'using the port '+IntToStr(http.DefaultPort)+' (change this in Tools/Options/Debugger). '+
        #13#10#13#10'You will not be able to debug web or touch keyboards until this issue '+
        'is resolved.'#13#10#13#10'Please note: restart Keyman Developer after resolving the issue to '+
        'enable the debugger again.');
    end;
    on E:EIdCouldNotBindSocket do   // I4304
    begin
      ShowMessage('The Keyman Developer debug web server was unable to start. '+
        'This is usually caused by a firewall blocking rule or another process '+
        'using the port '+IntToStr(http.DefaultPort)+' (change this in Tools/Options/Debugger). '+
        #13#10#13#10'You will not be able to debug web or touch keyboards until this issue '+
        'is resolved.'#13#10#13#10'Please note: restart Keyman Developer after resolving the issue to '+
        'enable the debugger again.');
    end;
  end;
end;

procedure TmodWebHttpServer.DataModuleDestroy(Sender: TObject);
begin
  http.Active := False;   // I4036

  FreeAndNil(FKeyboards);
  FreeAndNil(FKeyboardsCS);   // I4036
end;

function TmodWebHttpServer.GetKeyboardStoredFileName(const WebFilename: string): string;
begin
  FKeyboardsCS.Enter;   // I4036
  try
    if FKeyboards.ContainsKey(WebFilename) then Result := FKeyboards[WebFilename].StoredFilename
    else Result := '';
  finally
    FKeyboardsCS.Leave;
  end;
end;

function TmodWebHttpServer.GetURL: string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    GetURLs(str);
    if str.Count > 0
      then Result := str[0]
      else Result := '';
  finally
    str.Free;
  end;
end;

procedure TmodWebHttpServer.GetURLs(v: TStrings);
const
  IPv4Loopback  = '127.0.0.1';          {do not localize}

  function GetHostName(tp: TComputerNameFormat): string;
  var
    buf: array[0..260] of char;
    sz: Cardinal;
  begin
    if GetComputerNameEx(tp, buf, sz) then
      Result := buf
    else
      Result := '';
  end;
var
  port: string;
  sNetbios: string;
  sHost: string;
  sFull: string;
  i: Integer;
  FIPv4Addresses: TIdStackLocalAddressList;
begin
  port := ':'+IntToStr(http.DefaultPort);
  sFull := GetHostName(ComputerNameDnsFullyQualified);
  sHost := GetHostName(ComputerNameDnsHostname);
  sNetbios := GetHostName(ComputerNameNetBIOS);
  if SameText(sHost, sFull) then sHost := '';
  if SameText(sNetbios, sHost) or SameText(sNetbios, sFull) then sNetbios := '';

  if sFull <> '' then v.Add('http://'+sFull+port);
  if sHost <> '' then v.Add('http://'+sHost+port);
  if sNetbios <> '' then v.Add('http://'+sHost+port);

  FIPv4Addresses := TIdStackLocalAddressList.Create;
  try
    TIdStack.IncUsage;
    try
      GStack.GetLocalAddressList(FIPv4Addresses);
    finally
      TIdStack.DecUsage;
    end;

    for i := 0 to FIPv4Addresses.Count - 1 do
      v.Add('http://'+FIPv4Addresses[i].IPAddress+port);
  finally
    FIPv4Addresses.Free;
  end;

  v.Add('http://localhost'+port);
  v.Add('http://'+IPv4Loopback+port);
end;

function IsStandardFont(const FontName: string): Boolean;   // I4448
const
  StandardFontNames: array[0..9] of string = (
    'Arial', 'Calibri', 'Consolas', 'Courier New', 'Lucida Console', 'Lucida Sans Unicode', 'Segoe UI', 'Tahoma', 'Times New Roman', 'Verdana'
    );
begin
  Result := AnsiIndexText(FontName, StandardFontNames) >= 0;
end;

procedure TmodWebHttpServer.httpCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  doc: string;

  procedure Respond404;
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ResponseText := 'File not found';
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
    src: string;
    n: Integer;
    srcVersion: string;
  begin
    // Get dynamic keyboard registration

    response :=
      'var debugKeyboards = [];'#13#10+
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
          '  });'#13#10+
          '  debugKeyboards["'+name+'"] = {'+#13#10+   // I4260
          '    id:"'+name+'",'#13#10+
          '    version:"'+srcVersion+'"'#13#10+
          '  };';
      end;
    finally
      FKeyboardsCS.Leave;
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
      doc := GetKeyboardStoredFileName(doc);
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

    AResponseInfo.ContentType := http.MIMETable.GetFileMIMEType(doc);
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

procedure TmodWebHttpServer.RegisterKeyboard(const Filename, Version: string; FontInfo: TKeyboardFontArray);   // I4063   // I4409
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

procedure TmodWebHttpServer.UnregisterKeyboard(const Filename: string);
begin
  FKeyboardsCS.Enter;   // I4036
  try
    FKeyboards.Remove(ExtractFileName(Filename));
  finally
    FKeyboardsCS.Leave;
  end;
end;

{ TWebDebugKeyboardInfo }

constructor TWebDebugKeyboardInfo.Create(const AFilename, AVersion: string; AFonts: TKeyboardFontArray);   // I4409
var
  I: TKeyboardFont;
begin
  inherited Create;
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

end.
