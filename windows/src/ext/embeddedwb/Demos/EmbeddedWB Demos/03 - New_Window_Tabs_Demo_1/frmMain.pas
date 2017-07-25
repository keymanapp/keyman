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

unit frmMain;

interface

uses
  Classes, Controls, Forms, OleCtrls, EmbeddedWB, ComCtrls, StdCtrls,
  IEAddress, ExtCtrls, SysUtils;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    Button1: TButton;
    IEAddress1: TIEAddress;
    cbNewTab: TCheckBox;
    StatusBar1: TStatusBar;
    procedure PageControl1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure DownloadCompleteEvent(Sender: TObject);
    function GetActiveEWB: TEmbeddedWB;
    procedure UpdateAddress;
    procedure UpdateTab;
    function CreateNewTabBrowser(Url: string): TTabSheet;
    procedure NewWindowEvent(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
    procedure NewWebStatusTextChange(Sender: TObject;
      const Text: WideString);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  DesignTimeWB: TEmbeddedWB;
  NewTab: TTabSheet;
  i: integer;

implementation

{$R *.dfm}

//--------Private Section---------------------------------------------

procedure TForm1.UpdateTab;
begin
  PageControl1.ActivePage := NewTab;
  UpdateAddress;
end;

function TForm1.GetActiveEWB: TEmbeddedWB;
var
  i: Integer;
begin
  Result := nil;
  if PageControl1.ActivePage <> nil then
    for i := 0 to PageControl1.ActivePage.ControlCount - 1 do
    begin
      if PageControl1.ActivePage.Controls[i] is TEmbeddedWB then
      begin
        Result := TEmbeddedWB(PageControl1.ActivePage.Controls[i]);
        Exit;
      end;
    end;
end;

procedure TForm1.UpdateAddress;
var
  WB: TEmbeddedWB;
begin
  WB := GetActiveEWB;
  if Assigned(WB) then
  begin
    IEAddress1.EmbeddedWB := WB;
    IEAddress1.Text := WB.LocationURL;
    PageControl1.ActivePage.Caption := WB.LocationName;
  end;
end;

procedure TForm1.DownloadCompleteEvent(Sender: TObject);
begin
  UpdateAddress;
end;

function TForm1.CreateNewTabBrowser(Url: string): TTabSheet;
begin
  NewTab := TTabSheet.Create(PageControl1);
  with NewTab do
  begin
    PageControl := PageControl1;
    Parent := PageControl1;
    PageIndex := PageControl1.ActivePageIndex;
  end;
  DesignTimeWB := TEmbeddedWB.Create(NewTab);
  TControl(DesignTimeWB).Parent := NewTab;
  with DesignTimeWB do
  begin
    EnableMessageHandler := True;
    LoadSettings;
    Align := alClient;
    OnStatusTextChange := NewWebStatusTextChange;
    OnNewWindow2 := NewWindowEvent;
    OnDownloadComplete := DownloadCompleteEvent;
    if Trim(URL) <> '' then
      DesignTimeWB.NavigateWait(URL);
  end;
  PageControl1.ActivePage := NewTab;
  Result := NewTab;
end;

procedure TForm1.NewWindowEvent(Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
begin
  CreateNewTabBrowser(DesignTimeWB.LocationURL);
  PageControl1.ActivePage := NewTab;
  ppdisp := DesignTimeWB.Application;
  UpdateTab;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  UpdateAddress;
end;

procedure TForm1.NewWebStatusTextChange(Sender: TObject;
  const Text: WideString);
begin
  StatusBar1.SimpleText := Text;
end;

//--------End Of Private Section---------------------------------------------

procedure TForm1.FormShow(Sender: TObject);
begin
  CreateNewTabBrowser(DesignTimeWB.GetIEHomePage);
  UpdateTab;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if cbNewtab.Checked then
  begin
    CreateNewTabBrowser(IEAddress1.Text);
    UpdateTab;
  end
  else
  begin
    DesignTimeWB.Go(IEAddress1.Text);
    UpdateTab;
  end;
end;

end.

