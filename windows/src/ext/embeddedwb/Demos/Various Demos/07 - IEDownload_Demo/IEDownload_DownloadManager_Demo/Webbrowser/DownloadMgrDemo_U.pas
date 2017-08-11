//*************************************************************************
//                                                                        *
//                     IE Downloag Mgr                                    *
//                       For Delphi                                       *
//                                                                        *
//                     Freeware Demo                                      *
//  Developing Team:                                                      *
//  Eran Bodankin -bsalsa(bsalsa@bsalsa.com)                              *
//  Mathias Walter (mich@matze.tv)                                        *
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
AND DOCUMENTATION. [YOUR NAME] DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. VSOFT SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit DownloadMgrDemo_U;

interface

uses
  dialogs, ActiveX, IEConst, Classes, Forms, StdCtrls, EmbeddedWB, UrlMon,
  IEAddress, ExtCtrls, EwbCore, Controls, OleCtrls, SHDocVw_EWB;

type
  TForm1 = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    Panel1: TPanel;
    Button1: TButton;
    IEAddress1: TIEAddress;
    rgMethod: TRadioGroup;
    procedure EmbeddedWB1FileDownload(Sender: TCustomEmbeddedWB; pmk: IMoniker;
      pbc: IBindCtx; dwBindVerb, grfBINDF: Cardinal; pBindInfo: PBindInfo;
      pszHeaders, pszRedir: PWideChar; uiCP: Cardinal; var Rezult: HRESULT);
    procedure Button1Click(Sender: TObject);
    procedure EmbeddedWB1DownloadComplete(Sender: TObject);
    procedure EmbeddedWB1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure FormShow(Sender: TObject);
  private
    dlUrl: WideString;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  DownloadForm_U;

procedure TForm1.Button1Click(Sender: TObject);
begin
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
  case rgMethod.ItemIndex of
   0: DownloadForm.DoGo(dlUrl);
   1: DownloadForm.DoDownload(pmk, pbc);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  rgMethod.ItemIndex := 0;
  EmbeddedWb1.Go(IEAddress1.Text);
end;

end.

