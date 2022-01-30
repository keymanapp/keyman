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

  Winapi.Windows,

  IdBaseComponent,
  IdComponent,
  IdContext,
  IdCustomTCPServer,
  IdCustomHTTPServer,
  IdHTTPServer,

  Keyman.Developer.System.HttpServer.App,
  Keyman.Developer.System.HttpServer.AppSource;

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
    function GetApp: TAppHttpResponder;
    function GetAppSource: TAppSourceHttpResponder;
    function GetPort: Integer;
  public
    function GetLocalhostURL: string;
    function GetAppURL(s: string): string;
    procedure RestartServer;

    property App: TAppHttpResponder read GetApp;
    property AppSource: TAppSourceHttpResponder read GetAppSource;

    property Port: Integer read GetPort;
  end;

var
  modWebHttpServer: TmodWebHttpServer;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses
  IdException,
  IdStack,

  System.SysUtils,

  Vcl.Dialogs,
  Winapi.ActiveX,
  System.Win.ComObj,

  Keyman.Developer.System.ServerAPI,

  KeymanDeveloperOptions;

{$R *.dfm}

procedure TmodWebHttpServer.DataModuleCreate(Sender: TObject);
begin
  // TODO: split web host for project, editor, until we have moved
  // these over to the Server as well

  try
    http.Active := True;
  except
    on E:EIdSocketError do   // I4304
    begin
      ShowMessage('The Keyman Developer internal web server was unable to start. '+
        'Editors and Project view will not work correctly.');
    end;
    on E:EIdCouldNotBindSocket do   // I4304
    begin
      ShowMessage('The Keyman Developer internal web server was unable to start. '+
        'Editors and Project view will not work correctly.');
    end;
  end;

  try
    TServerDebugAPI.StartServer;
  except
    on E:Exception do
    begin
      ShowMessage('Unable to start Keyman Developer Server: '#13#10#13#10+
        E.Message+
        #13#10#13#10'The web debugger will not be available.');
    end;
  end;
end;

procedure TmodWebHttpServer.DataModuleDestroy(Sender: TObject);
begin
  if not FKeymanDeveloperOptions.ServerKeepAlive then
    TServerDebugAPI.StopServer;

  http.Active := False;   // I4036

  FreeAndNil(FApp);
  FreeAndNil(FAppSource);
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

function TmodWebHttpServer.GetLocalhostURL: string;
begin
  Result := 'http://127.0.0.1:'+IntToStr(Port);
end;

function TmodWebHttpServer.GetPort: Integer;
begin
  Result := http.Bindings[0].Port;
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

  finally
    CoUninitialize;
  end;
end;

procedure TmodWebHttpServer.RestartServer;
begin
  TServerDebugAPI.StopServer;
  Sleep(2000); // give server + ngrok time to shut down
  TServerDebugAPI.StartServer;
end;

end.
