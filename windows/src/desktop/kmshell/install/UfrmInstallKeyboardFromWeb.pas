(*
  Name:             UfrmInstallKeyboardFromWeb
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      6 Oct 2006

  Modified Date:    2 Oct 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          06 Oct 2006 - mcdurdin - Initial version
                    05 Dec 2006 - mcdurdin - Refactor using XML-Renderer
                    12 Dec 2006 - mcdurdin - Capitalize form name
                    04 Jan 2007 - mcdurdin - Add proxy support
                    15 Jan 2007 - mcdurdin - Use name of file in Content-Disposition
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    17 Sep 2007 - mcdurdin - I1070 - Add Download-Locale flag
                    27 Mar 2008 - mcdurdin - I1366 - Add version information to InstallKeyboardFromWeb so we can show appropriate content online
                    14 Jun 2008 - mcdurdin - I1496 - Fix filename when content disposition is missing [some proxy servers do this]
                    16 Jan 2009 - mcdurdin - I1679 - Fix crash installing keyboard when file is locked in temp dir
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    02 Oct 2014 - mcdurdin - I4414 - V9.0 - Download Keyboard dialog does not display correctly
*)
unit UfrmInstallKeyboardFromWeb;  // I3306   // I4414

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Winapi.Messages,
  Winapi.Windows,

  Keyman.UI.UframeCEFHost,
  UfrmWebContainer,
  UfrmDownloadProgress,
  UfrmKeymanBase;

type
  TfrmInstallKeyboardFromWeb = class(TfrmWebContainer)
    dlgSaveFile: TSaveDialog;
    procedure TntFormCreate(Sender: TObject);
  private
    FDownloadFilename: string;
    FDownloadURL: string;
    frmDownloadProgress: TfrmDownloadProgress;
    FDownloadStatusText: string;
    FDownloadStatusCode: Integer;
    procedure DoDownload(AOwner: TfrmDownloadProgress; var Result: Boolean);
    procedure cefBeforeBrowseEx(Sender: TObject; const Url: string;
      isMain, isPopup, wasHandled: Boolean);
    procedure cefBeforeBrowseExSync(Sender: TObject; const Url: string;
      isMain, isPopup: Boolean; out Handled: Boolean);
    procedure cefLoadingStateChange(Sender: TObject; isLoading, canGoBack, canGoForward: Boolean);
    procedure DownloadAndInstallPackage(const PackageID, BCP47: string);
    procedure HttpReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var Abort: Boolean);

  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
    procedure cefPreKeySyncEvent(Sender: TObject; e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean); override;
    procedure cefKeyEvent(Sender: TObject; e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean); override;
  end;

implementation

uses
  System.Net.HttpClient,
  System.Net.URLClient,
  System.RegularExpressions,
  uCEFTypes,

  httpuploader,
  GlobalProxySettings,
  Keyman.Configuration.UI.InstallFile,
  Keyman.System.LocaleStrings,
  kmint,
  MessageIdentifierConsts,
  Upload_Settings,
  utilfiletypes,
  utildir,
  utilhttp,
  utilexecute,
  VersionInfo;

{$R *.dfm}

{ TfrmInstallKeyboardFromWeb }

procedure TfrmInstallKeyboardFromWeb.FireCommand(const command: WideString;
  params: TStringList);
begin
  if command = 'footer_cancel' then
    ModalResult := mrCancel
  else
    inherited;
end;

procedure TfrmInstallKeyboardFromWeb.TntFormCreate(Sender: TObject);
begin
  inherited;
  // Ensures keyman.com hosted site opens locally
  cef.ShouldOpenRemoteUrlsInBrowser := False;
  cef.OnBeforeBrowseEx := cefBeforeBrowseEx;
  cef.OnBeforeBrowseExSync := cefBeforeBrowseExSync;
  cef.OnLoadingStateChange := cefLoadingStateChange;

  FRenderPage := 'downloadkeyboard';
  HelpTopic := 'context/download-keyboard';

  Content_Render;
end;

procedure TfrmInstallKeyboardFromWeb.cefBeforeBrowseExSync(Sender: TObject; const Url: string;
  isMain, isPopup: Boolean; out Handled: Boolean);
begin
  // This introduces some deeper knowledge of URL paths in Keyman Configuration, which is
  // a bit of a shame, because we try to keep our internal knowledge to /go/ urls on
  // keyman.com. However, there is not really any great way around this that I've found,
  // which allows us to handle internal navigation on the keyboard search and still lets
  // us open other URLs that may be in the search results in an external browser.

  Handled :=
    (IsMain and not IsLocalUrl(Url) and not Url.StartsWith('keyman:')) or // prevent Ctrl+click on iframe-internal link navigating top
    IsPopup or
    TRegEx.IsMatch(Url, URLPath_RegEx_MatchKeyboardsInstall) or       // capture https://keyman.com/keyboards/install/*
    (not TRegEx.IsMatch(Url, UrlPath_RegEx_MatchKeyboardsRoot) and    // don't capture https://keyman.com/keyboards*
    not Url.StartsWith('keyman:') and                                 // don't capture keyman:*
    not IsLocalUrl(Url) and                                           // don't capture the external frame or its resources
    not TRegEx.IsMatch(Url, UrlPath_RegEx_MatchKeyboardsGo));         // don't capture the launch url https://keyman.com/go/windows/download-keyboards
end;

procedure TfrmInstallKeyboardFromWeb.cefKeyEvent(Sender: TObject;
  e: TCEFHostKeyEventData; wasShortcut, wasHandled: Boolean);
begin
  if (e.event.kind in [KEYEVENT_RAWKEYDOWN, KEYEVENT_KEYDOWN]) and
      (e.event.windows_key_code = VK_ESCAPE) then
  begin
    ModalResult := mrCancel
  end
  else
    inherited;
end;

procedure TfrmInstallKeyboardFromWeb.cefLoadingStateChange(Sender: TObject;
  isLoading, canGoBack, canGoForward: Boolean);
begin
  if not isLoading then
  begin
    if canGoBack
      then cef.cef.ExecuteJavaScript('updateBackButtonState(true)', cef.cef.Browser.MainFrame.Url)
      else cef.cef.ExecuteJavaScript('updateBackButtonState(false)', cef.cef.Browser.MainFrame.Url);
  end;
end;

procedure TfrmInstallKeyboardFromWeb.cefPreKeySyncEvent(Sender: TObject;
  e: TCEFHostKeyEventData; out isShortcut, Handled: Boolean);
begin
  if (e.event.kind in [KEYEVENT_RAWKEYDOWN, KEYEVENT_KEYDOWN]) and
      (e.event.windows_key_code = VK_ESCAPE) then
    Handled := True
  else
    inherited;
end;

procedure TfrmInstallKeyboardFromWeb.cefBeforeBrowseEx(Sender: TObject; const Url: string;
  isMain, isPopup, wasHandled: Boolean);
var
  m: TMatch;
  uri: TURI;
  BCP47: string;
  PackageID: string;
begin

  if IsMain and not IsLocalUrl(Url) and not Url.StartsWith('keyman:') then
    Exit;                              // prevent Ctrl+click on iframe-internal link navigating top

  m := TRegEx.Match(Url, UrlPath_RegEx_MatchKeyboardsInstall);
  if m.Success then
  begin
    // We want to install the keyboard found in the path.
    PackageID := URLDecode(m.Groups[1].Value);
    uri := TURI.Create(url);

    try
      BCP47 := uri.ParameterByName['bcp47'];
    except
      // Sadly TURI does not have a TryGet pattern for parameters
      on E:ENetURIException do BCP47 := '';
    end;

    DownloadAndInstallPackage(PackageID, BCP47);
  end
  else if Url.StartsWith('http:') or Url.StartsWith('https:') or isPopup then
  begin
    if not TUtilExecute.URL(Url) then
      ShowMessage(SysErrorMessage(GetLastError));
  end;
end;

procedure TfrmInstallKeyboardFromWeb.DownloadAndInstallPackage(const PackageID, BCP47: string);
var
  FTempDir: string;
begin
  FTempDir := IncludeTrailingPathDelimiter(CreateTempPath);  // I1679
  try
    FDownloadFilename := FTempDir + PackageID + Ext_PackageFile;
    FDownloadURL := KeymanCom_Protocol_Server + URLPath_PackageDownload(PackageID, BCP47, False);

    frmDownloadProgress := TfrmDownloadProgress.Create(Self);
    try
      frmDownloadProgress.Callback := DoDownload;
      if frmDownloadProgress.ShowModal <> mrOk then
      begin
        ShowMessage(TLocaleStrings.MsgFromIdFormat(kmcom, S_DownloadKeyboard_DownloadError, [FDownloadStatusText, FDownloadStatusCode]));
        Exit;
      end;
    finally
      frmDownloadProgress.Free;
    end;

    if TInstallFile.Execute(Self, FDownloadFilename, False, False, '', BCP47) then
      ModalResult := mrOk;

  finally
    if FileExists(FDownloadFilename) then
      System.SysUtils.DeleteFile(FDownloadFilename);
    DeleteTempPath(FTempDir);
  end;

end;

procedure TfrmInstallKeyboardFromWeb.HttpReceiveData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
begin
  frmDownloadProgress.HTTPStatus(nil, 'Downloading '+ExtractFileName(FDownloadFilename), AReadCount, AContentLength);
  frmDownloadProgress.HTTPCheckCancel(nil, Abort);
end;

procedure TfrmInstallKeyboardFromWeb.DoDownload(AOwner: TfrmDownloadProgress; var Result: Boolean);
var
  Client: THTTPClient;
  Stream: TStream;
  Response: IHTTPResponse;
  FTempFilename: string;
begin
  Result := False;
  FTempFilename := FDownloadFilename + '.download';
  Client := THTTPClient.Create;
  try
    Client.SecureProtocols := [THTTPSecureProtocol.TLS1, THTTPSecureProtocol.TLS11, THTTPSecureProtocol.TLS12];
    Client.OnReceiveData := HttpReceiveData;

    Stream := TFileStream.Create(FTempFilename, fmCreate);
    try
      try
        Response := Client.Get(FDownloadURL, Stream);
        Result := Response.StatusCode = 200;
        FDownloadStatusText := Response.StatusText;
        FDownloadStatusCode := Response.StatusCode;
      except
        on E:ENetHTTPClientException do
        begin
          FDownloadStatusText := E.Message;
          FDownloadStatusCode := 0;
          Result := False;
        end;
      end;
    finally
      Stream.Free;
    end;

    if FileExists(FTempFilename) then
    begin
      if Result then
      begin
        if FileExists(FDownloadFilename) then
          System.SysUtils.DeleteFile(FDownloadFilename);
        RenameFile(FTempFilename, FDownloadFilename);
      end
      else
        System.SysUtils.DeleteFile(FTempFilename);
    end;
  finally
    Client.Free;
  end;
end;


end.
