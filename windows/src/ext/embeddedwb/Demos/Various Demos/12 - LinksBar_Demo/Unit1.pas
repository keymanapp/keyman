//****************************************************
//                      LinksBar Demo                *
//                                                   *
//                                                   *
//  By:                                              *
//  Eran Bodankin (bsalsa)                           *
//  bsalsa@bsalsa.com                                *
//                                                   *
// Documentation and updated versions:               *
//               http://www.bsalsa.com               *
//****************************************************

{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DocUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SystemS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SystemS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a Link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

//I used 2 methods to get the info :
// 1. StringList (So you can SaveToFile)
// 2. String so you use it simple Add procedure
// Both of them do the job for every parsed item.

unit Unit1;

interface

uses
  Classes, Controls, Forms, ComCtrls, OleCtrls, StdCtrls, ExtCtrls,
  ToolWin, LinksBar, ImgList, Buttons, Menus, SHDocVw, SHDocVw_EWB, EmbeddedWB,
  EwbCore;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Button5: TButton;
    Button6: TButton;
    ImageList1: TImageList;
    ShowImages: TCheckBox;
    Panel2: TPanel;
    Edit1: TEdit;
    Button2: TButton;
    LinksBar1: TLinksBar;
    PopupMenu1: TPopupMenu;
    AddCurrentSiteToTheLinksList1: TMenuItem;
    RemoveTheCurrentSiteFromTheLinksList1: TMenuItem;
    N14: TMenuItem;
    ShowTheList2: TMenuItem;
    ClearTheLinksList1: TMenuItem;
    ViewHideLinksbar: TCheckBox;
    EmbeddedWB1: TEmbeddedWB;
    procedure ViewHideLinksbarClick(Sender: TObject);
    procedure ShowImagesClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure EmbeddedWB1DocumentComplete(ASender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
  private
    bFirstTime: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button2Click(Sender: TObject);
begin
  EmbeddedWB1.Navigate(Edit1.Text);
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  LinksBar1.AddToLinksList(EmbeddedWB1.LocationName, EmbeddedWB1.LocationURL);
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  bFirstTime := True;
  EmbeddedWB1.Navigate('http://www.bsalsa.com');
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  LinksBar1.ClearTheLinksList;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  LinksBar1.RemoveFromLinksList(EmbeddedWB1.LocationName);
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  Memo1.Clear;
  LinksBar1.GetTheLinksList(Memo1.lines);
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  Memo2.Clear;
  LinksBar1.GetTheLinksURLs(Memo2.lines);
end;

procedure TForm2.ShowImagesClick(Sender: TObject);
begin
  if ShowImages.Checked then
  begin
    LinksBar1.ShowImages := false;
    LinksBar1.CreateLinkButtons(-1);
  end
  else
  begin
    LinksBar1.ShowImages := true;
    LinksBar1.CreateLinkButtons(0);
  end
end;

procedure TForm2.ViewHideLinksbarClick(Sender: TObject);
begin
  Linksbar1.Shown := ViewHideLinksbar.Checked
end;

procedure TForm2.EmbeddedWB1DocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  if bFirstTime then
  begin
    LinksBar1.AddToLinksList(EmbeddedWB1.LocationName, EmbeddedWB1.LocationURL);
    bFirstTime := False;
  end;
end;

end.

