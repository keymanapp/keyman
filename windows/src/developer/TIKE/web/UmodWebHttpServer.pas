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
//  System.SysUtils,

  Winapi.Windows,

  IdBaseComponent,
  IdComponent,
  IdContext,
  IdCustomTCPServer,
  IdCustomHTTPServer,
  IdHTTPServer,

  Keyman.Developer.System.HttpServer.App,
  Keyman.Developer.System.HttpServer.AppSource,
  Keyman.Developer.System.HttpServer.Debugger;

type

  TmodWebHttpServer = class(TDataModule)
    http: TIdHTTPServer;
    procedure httpCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FApp: TAppHttpResponder;
    FAppSource: TAppSourceHttpResponder;
    FDebugger: TDebuggerHttpResponder;
    function GetApp: TAppHttpResponder;
    function GetDebugger: TDebuggerHttpResponder;
    function GetAppSource: TAppSourceHttpResponder;
  public
    function GetURL: string;
    function GetLocalhostURL: string;
    function GetAppURL(s: string): string;
    procedure GetURLs(v: TStrings);

    property Debugger: TDebuggerHttpResponder read GetDebugger;
    property App: TAppHttpResponder read GetApp;
    property AppSource: TAppSourceHttpResponder read GetAppSource;
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

//  System.DateUtils,
//  System.JSON,
//  System.TimeSpan,
//  System.TypInfo,

  System.SysUtils,

  Vcl.Dialogs,
  Winapi.ActiveX,
  System.Win.ComObj,
//  Vcl.Graphics,

  KeymanDeveloperOptions;
//  RedistFiles;

{$R *.dfm}

procedure TmodWebHttpServer.DataModuleCreate(Sender: TObject);
begin
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

  FreeAndNil(FApp);
  FreeAndNil(FAppSource);
  FreeAndNil(FDebugger);
end;

function TmodWebHttpServer.GetApp: TAppHttpResponder;
begin
  if not Assigned(FApp) then
    FApp := TAppHttpResponder.Create;
  Result := FApp;
end;

function TmodWebHttpServer.GetAppSource: TAppSourceHttpResponder;
begin
  if not Assigned(FAppSource) then
    FAppSource := TAppSourceHttpResponder.Create;
  Result := FAppSource;
end;

function TmodWebHttpServer.GetAppURL(s: string): string;
begin
  Result := GetLocalhostURL + '/app/' + s;
end;

function TmodWebHttpServer.GetDebugger: TDebuggerHttpResponder;
begin
  if not Assigned(FDebugger) then
    FDebugger := TDebuggerHttpResponder.Create;
  Result := FDebugger;
end;

function TmodWebHttpServer.GetLocalhostURL: string;
begin
  Result := 'http://127.0.0.1:'+IntToStr(http.DefaultPort);
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
  if sNetbios <> '' then v.Add('http://'+sNetbios+port);

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

procedure TmodWebHttpServer.httpCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  doc: string;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    doc := ARequestInfo.Document;
    Delete(doc, 1, 1);

    if Copy(doc, 1, 11) = 'app/source/' then
    begin
      AppSource.ProcessRequest(AContext, ARequestInfo, AResponseInfo);
      Exit;
    end;

    if Copy(doc, 1, 4) = 'app/' then
    begin
      App.ProcessRequest(AContext, ARequestInfo, AResponseInfo);
      Exit;
    end;

    if Assigned(FDebugger) then
      FDebugger.ProcessRequest(AContext, ARequestInfo, AResponseInfo);
  finally
    CoUninitialize;
  end;
end;

end.
