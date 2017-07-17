//*************************************************************
//                    EmbeddedWB - MDI Demo                   *
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

unit Child;

interface

uses
  Classes, Forms, OleCtrls, EmbeddedWB, Controls, SHDocVw_EWB, EwbCore,
  ComCtrls;

type
  TChildFrm = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    StatusBar1: TStatusBar;
    procedure EmbeddedWB1NewWindow2(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool);
    procedure EmbeddedWB1DownloadComplete(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EmbeddedWB1StatusTextChange(ASender: TObject;
      const Text: WideString);
    procedure FormActivate(Sender: TObject);
  private
  public
  end;

var
  ChildFrm: TChildFrm;

implementation

uses frmMain;

{$R *.DFM}

procedure TChildFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TChildFrm.EmbeddedWB1DownloadComplete(Sender: TObject);
begin
  if Pos(frmMain.MainFrm.IEAddress1.Text, Caption) = 0 then
    Caption := Caption + ' ' + frmMain.MainFrm.IEAddress1.Text;
end;

procedure TChildFrm.EmbeddedWB1NewWindow2(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool);
var
  NewApp: TChildFrm;
begin
  NewApp := TChildFrm.Create(Owner);
  NewApp.Visible := True;
  ppdisp := NewApp.EmbeddedWB1.Application;
end;

procedure TChildFrm.EmbeddedWB1StatusTextChange(ASender: TObject;
  const Text: WideString);
begin
  StatusBar1.SimpleText := Text;
end;

procedure TChildFrm.FormActivate(Sender: TObject);
begin
  MainFrm.IEAddress1.EmbeddedWB := EmbeddedWB1;
  if EmbeddedWB1.LocationURL <> '' then
  MainFrm.IEAddress1.Text := EmbeddedWB1.LocationURL;
end;

end.

