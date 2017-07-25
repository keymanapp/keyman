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


unit frmMain;

interface

uses
  Classes, Forms, Menus, Child, IEAddress, ExtCtrls,
  Controls, StdCtrls, SysUtils;

type
  TMainFrm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Newchild1: TMenuItem;
    Window1: TMenuItem;
    Tile1: TMenuItem;
    Cascade1: TMenuItem;
    ArrangeAll1: TMenuItem;
    N2: TMenuItem;
    CloseAll1: TMenuItem;
    N10: TMenuItem;
    MinimizeAll1: TMenuItem;
    Panel1: TPanel;
    IEAddress1: TIEAddress;
    Button1: TButton;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Timer1: TTimer;
    procedure Exit1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Newchild1Click(Sender: TObject);
    procedure CloseAll1Click(Sender: TObject);
    procedure Cascade1Click(Sender: TObject);
    procedure Tile1Click(Sender: TObject);
    procedure ArrangeAll1Click(Sender: TObject);
    procedure MinimizeAll1Click(Sender: TObject);
  private
    procedure CreateChildForm(const ChildName: string);
    { Private declarations }
  public
    { Public declarations }
  end;
var
  MainFrm: TMainFrm;

implementation

{$R *.DFM}

procedure TMainFrm.CreateChildForm(const ChildName: string);
var
  Child: TChildFrm;
begin
  Child := TChildFrm.Create(Application);
  if Assigned(Child) then
    Child.Caption := ChildName;
end;

procedure TMainFrm.NewChild1Click(Sender: TObject);
begin
  CreateChildForm('Child #' + IntToStr(MDIChildCount + 1));
end;

procedure TMainFrm.CloseAll1Click(Sender: TObject);
var
  i: Byte;
begin
  for i := 0 to MdiChildCount - 1 do
    MDIChildren[i].Close;
end;

procedure TMainFrm.Cascade1Click(Sender: TObject);
begin
  Cascade;
end;

procedure TMainFrm.Tile1Click(Sender: TObject);
begin
  if TileMode = tbHorizontal then
    TileMode := tbVertical
  else
    TileMode := tbHorizontal;
  Tile;
end;

procedure TMainFrm.ArrangeAll1Click(Sender: TObject);
begin
  ArrangeIcons;
end;

procedure TMainFrm.MinimizeAll1Click(Sender: TObject);
var
  I: Byte;
begin
  for i := MdiChildCount - 1 downto 0 do
    MDIChildren[i].WindowState := wsMinimized;
end;

procedure TMainFrm.Button1Click(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then
    TChildFrm(ActiveMDIChild).EmbeddedWB1.Go(IEAddress1.Text);
end;

procedure TMainFrm.FormShow(Sender: TObject);
begin
  CreateChildForm('Child  #' + IntToStr(MDIChildCount + 1));
  if Assigned(ActiveMDIChild) then
    TChildFrm(ActiveMDIChild).EmbeddedWB1.Go(IEAddress1.Text);
end;

procedure TMainFrm.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.

