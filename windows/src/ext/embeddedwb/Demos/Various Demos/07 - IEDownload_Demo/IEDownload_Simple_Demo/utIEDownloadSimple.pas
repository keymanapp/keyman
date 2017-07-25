//*************************************************************************
//                                                                        *
//                     TIEDownload Simple Demo                            *
//                       For Delphi                                       *
//                                                                        *
//                     Freeware Demo                                      *
//  Developing Team:                                                      *
//  Eran Bodankin -bsalsa(bsalsa@bsalsa.com)                              *
//                                                                        *
//                                                                        *
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

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit utIEDownloadSimple;

interface

uses
  Dialogs, ActiveX, IEConst, Classes, Forms, StdCtrls, EmbeddedWB, UrlMon,
  IEAddress, ExtCtrls, EwbCore, Controls, OleCtrls, SHDocVw_EWB, IEDownload,
  ComCtrls, IEDownloadTools, Windows, IEDownloadAcc, SysUtils;

type
  TForm1 = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    Panel1: TPanel;
    Button1: TButton;
    IEDownload1: TIEDownload;
    IEAddress1: TEdit;
    Panel4: TPanel;
    PageControl1: TPageControl;
    TabSheet3: TTabSheet;
    Panel5: TPanel;
    memEvents: TMemo;
    TabSheet8: TTabSheet;
    memErrors: TMemo;
    TabSheet10: TTabSheet;
    memDetails: TMemo;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    procedure EmbeddedWB1FileDownload(Sender: TCustomEmbeddedWB; pmk: IMoniker;
      pbc: IBindCtx; dwBindVerb, grfBINDF: Cardinal; pBindInfo: PBindInfo;
      pszHeaders, pszRedir: PWideChar; uiCP: Cardinal; var Rezult: HRESULT);
    procedure Button1Click(Sender: TObject);
    procedure EmbeddedWB1DownloadComplete(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IEDownload1Error(const ErrorCode: Integer; const stError: string);
    procedure IEDownload1Progress(Sender: TBSCB; ulProgress, ulProgressMax,
      ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar; Downloaded,
      ElapsedTime, Speed, RemainingTime, Status, Percent: string);
    procedure Button2Click(Sender: TObject);
    procedure IEDownload1BeforeDownload(Sender: TInfoData; const Url, FileName,
      FileExtension, Host, DownloadFolder: string; const FileSize: Integer;
      var Cancel: Boolean);
    procedure IEDownload1Complete(Sender: TCustomIEDownload; aFileNameAndPath,
      aFileName, aFolderName, aExtension: WideString;
      const ActiveConnections: Integer);
    procedure IEDownload1StartBinding(Sender: TBSCB; var Cancel: Boolean;
      pib: IBinding; const FileName: WideString; const FileSize: Integer);
    procedure EmbeddedWB1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
  private
    dlUrl: string;
    procedure ClearComponents;
    procedure InitialComponents;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ClearComponents;
  InitialComponents;
  Embeddedwb1.Go(IEAddress1.Text);
end;

procedure TForm1.EmbeddedWB1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  dlUrl := URL;
end;

procedure TForm1.EmbeddedWB1DownloadComplete(Sender: TObject);
begin
  IEAddress1.Text := EmbeddedWB1.LocationURL;
end;

procedure TForm1.EmbeddedWB1FileDownload(Sender: TCustomEmbeddedWB;
  pmk: IMoniker; pbc: IBindCtx; dwBindVerb, grfBINDF: Cardinal;
  pBindInfo: PBindInfo; pszHeaders, pszRedir: PWideChar; uiCP: Cardinal;
  var Rezult: HRESULT);
begin
  Rezult := S_OK;
  if pszRedir <> '' then
    dlUrl := pszRedir;
  IEDownload1.Go(dlUrl);
  //IEDownload1.Download(pmk, pbc);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  EmbeddedWb1.Go(IEAddress1.Text);
end;

procedure TForm1.IEDownload1BeforeDownload(Sender: TInfoData; const Url,
  FileName, FileExtension, Host, DownloadFolder: string;
  const FileSize: Integer; var Cancel: Boolean);
begin
  memEvents.Lines.Add('BeforeDownload. ' + Url +
    #13#10 + FileName + #13#10 + DownloadFolder + #13#10);
end;

procedure TForm1.IEDownload1Complete(Sender: TCustomIEDownload;
  aFileNameAndPath, aFileName, aFolderName, aExtension: WideString;
  const ActiveConnections: Integer);
begin
  memEvents.Lines.Add('Complete Event:' + aFileName);
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.IEDownload1Error(const ErrorCode: Integer;
  const stError: string);
begin
  memEvents.Lines.Add('Error Event.' + stError);
  memErrors.Lines.Add('An error accrued:  ' + stError);
end;

procedure TForm1.IEDownload1Progress(Sender: TBSCB; ulProgress, ulProgressMax,
  ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar; Downloaded,
  ElapsedTime, Speed, RemainingTime, Status, Percent: string);
begin
  ProgressBar1.Max := ulProgressMax;
  ProgressBar1.Position := ulProgress;
end;

procedure TForm1.IEDownload1StartBinding(Sender: TBSCB; var Cancel: Boolean;
  pib: IBinding; const FileName: WideString; const FileSize: Integer);
begin
  memEvents.Lines.Add('StartBinding Event. '
    + #13#10 + 'Filename: ' + FileName
    + #13#10 + 'FileSize:' + IntToStr(FileSize));
end;

{Some procedures-----------------------------------------------------------------}

procedure TForm1.Button2Click(Sender: TObject);
begin
  IEDownload1.CancelAll;
end;

procedure TForm1.ClearComponents;
begin
  memEvents.Lines.Clear;
  MemErrors.Lines.Clear;
  MemDetails.Lines.Clear;
end;

procedure TForm1.InitialComponents;
begin
  PageControl1.ActivePageIndex := 0;
  ProgressBar1.Position := 0;
end;

end.

