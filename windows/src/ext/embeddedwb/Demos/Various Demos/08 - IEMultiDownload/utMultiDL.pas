//*************************************************************************
//                                                                        *
//                  IEMultiDownload Demo 2009                             *                                                      *
//                            by                                          *
//             Eran Bodankin - bsalsa - (bsalsa@gmail.com)                *
//  Updated versions:                                                     *
//               http://www.bsalsa.com                                    *
//*************************************************************************
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
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 3 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit utMultiDL;

interface

uses
  MSHTML_EWB, SysUtils, IEDownloadTools, Windows, Classes, Forms, IEDownload,
  IEMultiDownload, StdCtrls, ExtCtrls, ComCtrls, IEParser, OleCtrls, Controls;

type
  TfrmMultiDownloader = class(TForm)
    IEMultiDownload1: TIEMultiDownload;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    btnGo: TButton;
    Panel2: TPanel;
    PageControl1: TPageControl;
    tsEvents: TTabSheet;
    Panel5: TPanel;
    memEvents: TMemo;
    TabSheet5: TTabSheet;
    memResponse: TMemo;
    TabSheet6: TTabSheet;
    memProgress: TMemo;
    TabSheet8: TTabSheet;
    memErrors: TMemo;
    TabSheet10: TTabSheet;
    memDetails: TMemo;
    TabImages: TTabSheet;
    TabLinks: TTabSheet;
    memImages: TMemo;
    memLinks: TMemo;
    edtAddress: TEdit;
    ProgressBar1: TProgressBar;
    btnCancel: TButton;
    tsDownloads: TTabSheet;
    memDownloads: TMemo;
    tsItems: TTabSheet;
    memItems: TMemo;
    procedure btnGoClick(Sender: TObject);
    procedure IEMultiDownload1Error(const ErrorCode: Integer;
      const stError: string);
    function IEMultiDownload1Response(Sender: TBSCB; dwResponseCode: Cardinal;
      szResponseHeaders, szRequestHeaders: PWideChar;
      out szAdditionalRequestHeaders: PWideChar): HRESULT;
    procedure IEMultiDownload1StateChange(const State: TState);
    procedure FormCreate(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure IEMultiDownload1Progress(Sender: TBSCB; ulProgress, ulProgressMax,
      ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar; Downloaded,
      ElapsedTime, Speed, RemainingTime, Status, Percent: string);
    procedure IEMultiDownload1DataAvailableInfo(Sender: TBSCB;
      grfBSCF: Cardinal; Status: string);
    procedure IEMultiDownload1Complete(Sender: TCustomIEDownload;
      aFileNameAndPath, aFileName, aFolderName, aExtension: WideString;
      const ActiveConnections: Integer);
    procedure btnCancelClick(Sender: TObject);
    procedure IEMultiDownload1MultiParseError(Sender: TObject;
      const ErrorCode: Integer; const Url, stError: string);
    procedure IEMultiDownload1MultiStateChange(Sender: TObject;
      const State: TMultiState);
    procedure IEMultiDownload1MultiItemAdded(Sender: TObject; const hRef,
      hProtocol, hRoot, hPath, hFileName, hUser, hPassword: WideString;
      hPort: Integer);
    procedure IEMultiDownload1MultiBeforeDownload(Sender: TObject;
      const hRef: WideString; const Item: TDownloadItem; var Cancel: Boolean);
    procedure IEMultiDownload1MultiGetDocInfo(Sender: TObject;
      const Text: string);
    procedure IEMultiDownload1MultiGetLink(Sender: TObject; const hRef, Host,
      HostName, PathName, Port, Protocol, MimeType, NameProp: string;
      var Cancel: Boolean);
    procedure IEMultiDownload1MultiGetQueryInfo(const MimeType, Encoding,
      Disposition: string);
    procedure IEMultiDownload1MultiParseComplete(Sender: TObject;
      Doc: IHTMLDocument2; All: IHTMLElementCollection);
    procedure IEMultiDownload1MultiParseDocument(Sender: TObject;
      const Res: HRESULT; stMessage: string);
    procedure IEMultiDownload1MultiComplete(Sender: TObject;
      const DownloadedList: TStrings);
    procedure IEMultiDownload1MultiGetImage(Sender: TObject;
      const ImgName: string; var Cancel: Boolean);
    procedure IEMultiDownload1MultiParseProgress(Sender: TObject;
      const ulProgress, ulProgressMax: Integer);
    procedure IEMultiDownload1MultiStartParsing(Sender: TObject;
      const aUrl: WideString);
    procedure IEMultiDownload1BeforeDownload(Sender: TInfoData; const Url,
      FileName, FileExtension, Host, DownloadFolder: string;
      const FileSize: Integer; var Cancel: Boolean);
  private
    procedure AddLog(Memo: TMemo; const s: string);
  public

  end;

var
  frmMultiDownloader: TfrmMultiDownloader;

implementation

{$R *.dfm}

procedure TfrmMultiDownloader.AddLog(Memo: TMemo; const s: string);
begin
// Sleep(1);
//  OutputDebugString(PChar(s));
  Memo.Lines.Add(s);
end;

procedure TfrmMultiDownloader.btnCancelClick(Sender: TObject);
begin
  IEMultiDownload1.Stop;
end;

procedure TfrmMultiDownloader.btnGoClick(Sender: TObject);
begin
  memDownloads.Clear;
  IEMultiDownload1.GoMulti(edtAddress.Text);
end;

procedure TfrmMultiDownloader.FormCreate(Sender: TObject);
var
  pbStyle: integer;
begin
  StatusBar1.Panels[2].Style := psOwnerDraw;
  ProgressBar1.Parent := StatusBar1;
  pbStyle := GetWindowLong(ProgressBar1.Handle, GWL_EXSTYLE);
  pbStyle := pbStyle - WS_EX_STATICEDGE;
  SetWindowLong(ProgressBar1.Handle, GWL_EXSTYLE, pbStyle);
  StatusBar1.Panels[0].Text := 'Ready.'
end;

procedure TfrmMultiDownloader.IEMultiDownload1BeforeDownload(Sender: TInfoData;
  const Url, FileName, FileExtension, Host, DownloadFolder: string;
  const FileSize: Integer; var Cancel: Boolean);
begin
   AddLog(memDownloads, #13#10 +FileName +#13#10 +DownloadFolder);
end;

procedure TfrmMultiDownloader.IEMultiDownload1Complete(Sender: TCustomIEDownload;
  aFileNameAndPath, aFileName, aFolderName, aExtension: WideString;
  const ActiveConnections: Integer);
begin
  AddLog(memDownloads, 'Done with: ' + aFileName);
end;

procedure TfrmMultiDownloader.IEMultiDownload1DataAvailableInfo(Sender: TBSCB;
  grfBSCF: Cardinal; Status: string);
begin
  if not StrContain('intermediate', Status) then
    AddLog(memEvents, Status);
end;

procedure TfrmMultiDownloader.IEMultiDownload1Error(const ErrorCode: Integer;
  const stError: string);
begin
  AddLog(memErrors, stError);
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiBeforeDownload(
  Sender: TObject; const hRef: WideString; const Item: TDownloadItem;
  var Cancel: Boolean);
begin
  AddLog(memEvents, 'MultiBeforeDownload: ' + hRef);
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiComplete(Sender: TObject;
  const DownloadedList: TStrings);
begin
  //memDownloads.Lines := DownloadedList;
  AddLog(memEvents, 'MultiComplete ');
  StatusBar1.Panels[0].Text := 'Done.'
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiGetDocInfo(Sender: TObject;
  const Text: string);
begin
//  AddLog(memEvents, 'MultiGetDocInfo ');
  AddLog(memDetails, Text);
end;


procedure TfrmMultiDownloader.IEMultiDownload1MultiGetLink(Sender: TObject;
  const hRef, Host, HostName, PathName, Port, Protocol, MimeType,
  NameProp: string; var Cancel: Boolean);
begin
//  AddLog(memEvents, 'MultiGetLink');
  AddLog(MemLinks, 'hRef: ' + hRef + #13#10 + 'Host: ' + Host + #13#10 +
    'HostName: ' + HostName + #13#10 + 'PathName: ' + PathName + #13#10 + 'Port: ' +
    Port + #13#10 + 'Protocol: ' + Protocol + #13#10 +
    'MimeType: ' + MimeType + #13#10 + 'NameProp: ' + NameProp + #13#10);

end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiGetQueryInfo(const MimeType,
  Encoding, Disposition: string);
begin
  AddLog(memEvents, 'MultiGetQueryInfo ');
  if MimeType <> '' then
    AddLog(memDetails, 'MimeType: ' + MimeType);
  if Encoding <> '' then
    AddLog(memDetails, 'Encoding: ' + Encoding);
  if Disposition <> '' then
    AddLog(memDetails, 'Disposition: ' + Disposition);
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiItemAdded(Sender: TObject;
  const hRef, hProtocol, hRoot, hPath, hFileName, hUser, hPassword: WideString;
  hPort: Integer);
begin
 // AddLog(memEvents, 'MultiItemAdded ');
  memItems.Lines.Add(#13#10 + 'Ref: ' + hRef +
    #13#10 + 'Protocol: ' + hProtocol +
    #13#10 + 'Root: ' + hRoot +
    #13#10 + 'Path: ' + hPath +
    #13#10 + 'FileName: ' + hFileName +
    #13#10 + 'User: ' + hUser +
    #13#10 + 'Password: ' + hPassword +
    #13#10 + 'Port: ' + IntToStr(hPort));
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiParseComplete(
  Sender: TObject; Doc: IHTMLDocument2; All: IHTMLElementCollection);
begin
  AddLog(memEvents, 'MultiParseComplete');
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiParseDocument(
  Sender: TObject; const Res: HRESULT; stMessage: string);
begin
  StatusBar1.Panels[0].Text := 'Parsing.';
  AddLog(memEvents, 'MultiParseDocument: ' + stMessage);
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiParseError(Sender: TObject;
  const ErrorCode: Integer; const Url, stError: string);
begin
  AddLog(memEvents, 'MultiParseError');
  memErrors.Lines.Add(stError);
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiParseProgress(
  Sender: TObject; const ulProgress, ulProgressMax: Integer);
begin
    Progressbar1.Max:= ulProgressMax;
    Progressbar1.Position:= ulProgress;
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiStartParsing(Sender: TObject;
  const aUrl: WideString);
begin
  AddLog(memEvents, #13#10 +'Starting with: '+aUrl);
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiStateChange(Sender: TObject;
  const State: TMultiState);
begin
  btnGo.Enabled := IEMultiDownload1.MultiState <> msBusy;
  btnCancel.Enabled := IEMultiDownload1.MultiState = msBusy;
  case IEMultiDownload1.MultiState of
    msBusy: AddLog(memEvents, 'MultiDownload Busy');
    msReady: AddLog(memEvents, 'MultiDownload Ready');
    msStopped: AddLog(memEvents, 'MultiDownload Stopped');
  end;
end;

procedure TfrmMultiDownloader.IEMultiDownload1Progress(Sender: TBSCB; ulProgress,
  ulProgressMax, ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar;
  Downloaded, ElapsedTime, Speed, RemainingTime, Status, Percent: string);
begin
  AddLog(memResponse, Status);
  Progressbar1.Max := ulProgressMax;
  Progressbar1.Position := ulProgress;
end;

function TfrmMultiDownloader.IEMultiDownload1Response(Sender: TBSCB;
  dwResponseCode: Cardinal; szResponseHeaders, szRequestHeaders: PWideChar;
  out szAdditionalRequestHeaders: PWideChar): HRESULT;
begin
  AddLog(memResponse, szResponseHeaders);
  Result := S_OK;
end;

procedure TfrmMultiDownloader.IEMultiDownload1StateChange(const State: TState);
begin
  case IEMultiDownload1.State of
    sBusy:
      begin
        AddLog(memEvents, 'IEDownload Busy');
        StatusBar1.Panels[0].Text := 'Downloading, Please Wait..'
      end;
    sReady: AddLog(memEvents, 'IEDownload Ready');
    sStopped: AddLog(memEvents, 'IEDownload Stopped');
  end;
end;

procedure TfrmMultiDownloader.StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  if Panel = StatusBar.Panels[2] then
    with ProgressBar1 do
    begin
      Top := Rect.Top;
      Left := Rect.Left;
      Width := Rect.Right - Rect.Left - 15;
      Height := Rect.Bottom - Rect.Top
    end;
end;

procedure TfrmMultiDownloader.IEMultiDownload1MultiGetImage(
  Sender: TObject; const ImgName: string; var Cancel: Boolean);
begin
  //AddLog(memEvents, 'MultiGetImage');
  AddLog(memImages, 'ImageName: ' + ImgName);
end;

end.

