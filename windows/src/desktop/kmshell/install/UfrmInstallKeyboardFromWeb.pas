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
    procedure cefBeforeBrowse(Sender: TObject; const Url: string;
      wasHandled: Boolean);
    procedure cefBeforeBrowseSync(Sender: TObject; const Url: string;
      out Handled: Boolean);
    procedure cefLoadingStateChange(Sender: TObject; isLoading, canGoBack, canGoForward: Boolean);
    procedure DownloadAndInstallPackage(const PackageID, BCP47: string);
    procedure HttpReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var Abort: Boolean);

  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  end;

implementation

uses
  System.Net.HttpClient,
  System.Net.URLClient,
  System.RegularExpressions,

  httpuploader,
  GlobalProxySettings,
  Keyman.Configuration.UI.InstallFile,
  Keyman.System.LocaleStrings,
  kmint,
  MessageIdentifierConsts,
  Upload_Settings,
  utilfiletypes,
  utildir,
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
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnBeforeBrowseSync := cefBeforeBrowseSync;
  cef.OnLoadingStateChange := cefLoadingStateChange;

  FRenderPage := 'downloadkeyboard';

  Content_Render;
end;

procedure TfrmInstallKeyboardFromWeb.cefBeforeBrowseSync(Sender: TObject; const Url: string; out Handled: Boolean);
begin
  Handled := TRegEx.IsMatch(Url, UrlPath_RegEx_MatchKeyboardsInstall);
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

procedure TfrmInstallKeyboardFromWeb.cefBeforeBrowse(Sender: TObject; const Url: string; wasHandled: Boolean);
var
  m: TMatch;
  uri: TURI;
  BCP47: string;
  PackageID: string;
begin

  m := TRegEx.Match(Url, UrlPath_RegEx_MatchKeyboardsInstall);
  if m.Success then
  begin
    // We want to install the keyboard found in the path.
    PackageID := m.Groups[1].Value;
    uri := TURI.Create(url);

    try
      BCP47 := uri.ParameterByName['bcp47'];
    except
      // Sadly TURI does not have a TryGet pattern for parameters
      on E:ENetURIException do BCP47 := '';
    end;

    DownloadAndInstallPackage(PackageID, BCP47);
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
    Client.OnReceiveData := HttpReceiveData;

    Stream := TFileStream.Create(FTempFilename, fmCreate);
    try
      Response := Client.Get(FDownloadURL, Stream);
      Result := Response.StatusCode = 200;
      FDownloadStatusText := Response.StatusText;
      FDownloadStatusCode := Response.StatusCode;
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
