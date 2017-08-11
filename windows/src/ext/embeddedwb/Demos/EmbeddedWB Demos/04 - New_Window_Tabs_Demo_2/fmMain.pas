//*************************************************************
//                    EmbeddedWB - Tabs Demo                  *
//                                                            *
//                            by                              *
//                     Eran Bodankin (bsalsa)                 *
//                     bsalsa@bsalsa.com                      *
//                                                            *
//     Documentation and updated versions:                    *
//               http://www.bsalsa.com                        *
//*************************************************************
{*******************************************************************************}
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

You may use, change or modify the demo under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please,  consider donation in our web site!
{*******************************************************************************}

unit fmMain;

interface

uses
  Classes, Controls, Forms, ComCtrls, ExtCtrls, StdCtrls, OleCtrls, SysUtils,
  IEAddress, EwbCore, EmbeddedWB, ShDocVw_Ewb;

type
  TTabSheetEx = class(TTabSheet)
  private
    EWB: TEmbeddedWB;
    CanBack: Boolean;
    CanForward: Boolean;
    CanStop: Boolean;
  end;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    btnGo: TButton;
    btnBack: TButton;
    btnForward: TButton;
    IEAddress1: TIEAddress;
    StatusBar1: TStatusBar;
    cbOpenNew: TCheckBox;
    btnStop: TButton;
    procedure PageControl1Change(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnForwardClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  private
    { Private declarations }
    procedure DownloadCompleteEvent(Sender: TObject);
    procedure CommandStateChangeEvent(Sender: TObject; Command: Integer; Enable: WordBool);
    procedure UpdateControls;
    procedure CreateNewTabBrowser(Url: string);
    procedure NewWindow2Event(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
    procedure StatusTextChangeEvent(Sender: TObject; const Text: WideString);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  TabSheetEx: TTabSheetEx;

implementation

{$R *.dfm}


procedure TForm1.UpdateControls;
var
  TabEx: TTabSheetEx;
begin
  TabEx := (PageControl1.ActivePage as TTabSheetEx);
  IEAddress1.Text := TabEx.EWB.LocationURL;
  PageControl1.ActivePage.Caption := TabEx.EWB.LocationName;
  Form1.Caption := TabEx.EWB.LocationName;
  BtnForward.Enabled := TabEx.CanForward;
  btnBack.Enabled := TabEx.CanBack;
  btnStop.Enabled := TabEx.CanStop;
end;

procedure TForm1.DownloadCompleteEvent(Sender: TObject);
begin
  UpdateControls;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  CreateNewTabBrowser('http://bsalsa.com/forum/');
  UpdateControls;
end;

procedure TForm1.CreateNewTabBrowser(Url: string);
begin
  TabSheetEx := TTabSheetEx.Create(PageControl1);
  with TabSheetEx do
  begin
    PageControl := PageControl1;
    EWB := TEmbeddedWB.Create(TabSheetEx);
    Caption := 'Loading...';
    TOleControl(TabSheetEx.EWB).Parent := TabSheetEx;
    PageControl1.ActivePage := TabSheetEx;
    with EWB do
    begin
      LoadSettings;
      Align := alClient;
      Visible := True;
      OnStatusTextChange := StatusTextChangeEvent;
      OnNewWindow2 := NewWindow2Event;
      OnDownloadComplete := DownloadCompleteEvent;
      OnCommandStateChange := CommandStateChangeEvent;
      Go(URL);
    end;
  end;
end;

procedure TForm1.btnBackClick(Sender: TObject);
begin
  btnBack.Enabled := False;
  (PageControl1.ActivePage as TTabSheetEx).EWB.GoBack;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  btnStop.Enabled := False;
  (PageControl1.ActivePage as TTabSheetEx).EWB.Stop;
end;

procedure TForm1.btnForwardClick(Sender: TObject);
begin
  btnForward.Enabled := False;
  (PageControl1.ActivePage as TTabSheetEx).EWB.GoForward;
end;

procedure TForm1.btnGoClick(Sender: TObject);
begin
  if cbOpenNew.Checked then
    CreateNewTabBrowser(IEAddress1.Text)
  else
    (PageControl1.ActivePage as TTabSheetEx).EWB.Go(IEAddress1.Text);
end;

procedure TForm1.CommandStateChangeEvent(Sender: TObject; Command: Integer; Enable: WordBool);
const
  CSC_UPDATECOMMANDS = $FFFFFFFF;
begin
  case TOleEnum(Command) of
    CSC_NAVIGATEBACK:
      ((Sender as TEmbeddedWB).Owner as TTabSheetEx).CanBack := Enable;
    CSC_NAVIGATEFORWARD:
      ((Sender as TEmbeddedWB).Owner as TTabSheetEx).CanForward := Enable;
    CSC_UPDATECOMMANDS:
      ((Sender as TEmbeddedWB).Owner as TTabSheetEx).CanStop := (Sender as TEmbeddedWB).Busy;
  end;
end;

procedure TForm1.NewWindow2Event(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
begin
  CreateNewTabBrowser(TabSheetEx.EWB.LocationURL);
  PageControl1.ActivePage := TabSheetEx;
  ppdisp := TabSheetEx.EWB.Application;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  UpdateControls;
end;

procedure TForm1.StatusTextChangeEvent(Sender: TObject;
  const Text: WideString);
begin
  StatusBar1.SimpleText := Text;
end;

// http://mp3.baidu.com/m?tn=baidump3&ct=134217728&lm=-1&li=500&word=%CE%D2%C3%C7%CB%B5%BA%C3%B5%C4+%D5%C5%F6%A6%D3%B1


end.

