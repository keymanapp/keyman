//*************************************************************
//                    EmbeddedWB - SDI Demo                   *
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
  Classes, Controls, Forms, EmbeddedWB, StdCtrls, IEAddress, ExtCtrls,
  ComCtrls, SHDocVw_EWB, EwbCore, OleCtrls;

type
  TForm1 = class(TForm)
    pnlAddressBar: TPanel;
    btnGo: TButton;
    IEAddress1: TIEAddress;
    EmbeddedWB1: TEmbeddedWB;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnGoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EmbeddedWB1NewWindow3(ASender: TObject;
      var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
      const bstrUrlContext, bstrUrl: WideString);
    procedure EmbeddedWB1NewWindow2(ASender: TObject;
      var ppDisp: IDispatch; var Cancel: WordBool);
    procedure EmbeddedWB1StatusTextChange(ASender: TObject;
      const Text: WideString);
  private
    { Private declarations }
    procedure OpenNewWindow(ASender: TObject; var ppDisp: IDispatch);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnGoClick(Sender: TObject);
begin
  EmbeddedWB1.Go(IEAddress1.Text);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  btnGoClick(nil);
end;

{
  In order to specify that your browser application should be used when a new window is opened,
  you set ppDisp equal to a new WebBrowser object that is contained in a new window created by your application.
  In this scenario, if a user chooses to open a Web page in a new window,
  the new window in your application will be used to display the new Web page.
}

procedure TForm1.OpenNewWindow(ASender: TObject; var ppDisp: IDispatch);
var
  NewApp: TForm1;
begin
  Application.CreateForm(TForm1, NewApp);
  NewApp.Visible := True;
  NewApp.Left := Left + 50;
  ppdisp := NewApp.EmbeddedWB1.Application;
  NewApp.EmbeddedWB1.RegisterAsBrowser := True;
  {
  Set the RegisterAsBrowser property to TRUE for the newly created WebBrowser control in
  order for it to participate in window name resolution.
  For example, if this window name is used elsewhere in script, then this control will be used instead of a newly
  created one because it checks all of the existing window names before opening a new window.
}
end;

{
  OnNewWindow2 Fires when a new window is to be created.
  http://msdn.microsoft.com/en-us/library/aa768336(VS.85).aspx
}

procedure TForm1.EmbeddedWB1NewWindow2(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool);
begin
  OpenNewWindow(ASender, ppDisp);
end;

{
  NewWindow3 Fires when a new window is to be created. Extends NewWindow2 with
  additional information about the new window.
  NewWindow3 is available only in Microsoft Windows XP Service Pack 2 (SP2) or later.
  http://msdn.microsoft.com/en-us/library/aa768337(VS.85).aspx
}

procedure TForm1.EmbeddedWB1NewWindow3(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
  const bstrUrlContext, bstrUrl: WideString);
begin
  OpenNewWindow(ASender, ppDisp);
end;

procedure TForm1.EmbeddedWB1StatusTextChange(ASender: TObject;
  const Text: WideString);
begin
  StatusBar1.SimpleText := Text;
end;

end.

