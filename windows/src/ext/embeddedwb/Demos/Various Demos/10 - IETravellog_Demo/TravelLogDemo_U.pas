//***********************************************************
//                    IETravelLog Demo (2006)               *
//                                                          *
//               For Delphi 5 - 2009                        *
//                            by                            *
//                     Per Lindsø Larsen                    *
//                   per.lindsoe@larsen.mail.dk             *
//                                                          *
//Updates by:  Eran Bodankin (bsalsa)   bsalsa@bsalsa.com   *
//           Documentation and updated versions:            *
//               http://www.bsalsa.com                      *
//***********************************************************
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

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit TravelLogDemo_U;

interface

uses
  IETravelLog, Controls, Forms, Buttons, StdCtrls, Menus, OleCtrls, EmbeddedWB,
  SHDocVw_EWB, EwbCore, Classes, IEAddress, ExtCtrls;

type
  TForm1 = class(TForm)
    BackBtn: TBitBtn;
    BackDropDownBtn: TBitBtn;
    PopupMenu1: TPopupMenu;
    ForwardBtn: TBitBtn;
    ForwardDropDownBtn: TBitBtn;
    IETravelLog1: TIETravelLog;
    EmbeddedWb1: TEmbeddedWB;
    Button1: TButton;
    Panel1: TPanel;
    IEAddress1: TIEAddress;
    procedure IETravelLog1Entry(Title, Url: WideString; var Cancel: Boolean);
    procedure MyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject);
    procedure EmbeddedWb1CommandStateChange(Sender: TObject;
      Command: Integer; Enable: WordBool);
    procedure FormShow(Sender: TObject);
    procedure EmbeddedWb1BeforeNavigate2(Sender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);

  private
    { Private declarations }
    FBack: Boolean;
    PopUpItems: array[0..9] of TMenuItem;
    ItemsCounter: Integer;
    Popupx, PopupY: Integer;
    procedure MyPopupHandler(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;


implementation

{$R *.dfm}

procedure TForm1.MyPopupHandler(Sender: TObject);
var
  index: Integer;
begin
  with Sender as TMenuItem do
    if FBack then
      Index := 0 - PopupMenu1.Items.IndexOf(Sender as TMenuItem) - 1
    else
      Index := PopupMenu1.Items.IndexOf(Sender as TMenuItem) + 1;
  IETravelLog1.TravelTo(Index);
end;

procedure TForm1.MyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ItemsCounter := 0;
  PopUpX := Self.Left + (Sender as TBitBtn).Left;
  popUpY := Self.Top + (Sender as TBitBtn).Top + 50;
  PopupMenu1.Items.Clear;
  if (Sender as TBitbtn) = ForwardDropDownBtn then
  begin
    IETravellog1.EnumerateForward;
    FBack := False;
  end
  else
  begin
    IETravellog1.EnumerateBack;
    FBack := True;
  end;
  PopupMenu1.Popup(popupX, popupY);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  EmbeddedWb1.go(IEAddress1.Text);
end;

procedure TForm1.BackBtnClick(Sender: TObject);
begin
  EmbeddedWb1.GoBack;
end;

procedure TForm1.ForwardBtnClick(Sender: TObject);
begin
  EmbeddedWb1.GoForward;
end;

procedure TForm1.IETravelLog1Entry(Title, Url: WideString; var Cancel: Boolean);
begin
  PopUpItems[itemsCounter] := TMenuItem.Create(Self);
  PopUpItems[itemsCounter].Caption := Title;
  PopUpItems[itemsCounter].Hint := Url;
  PopUpItems[itemsCounter].OnClick := MyPopUpHandler;
  PopUpMenu1.Items.Add(PopUpItems[itemsCounter]);
  Inc(ItemsCounter);
  Cancel := ItemsCounter = 10; // limit to 10 entries
end;

procedure TForm1.EmbeddedWb1CommandStateChange(Sender: TObject;
  Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEFORWARD:
      begin
        ForwardBtn.Enabled := Enable;
        ForwardDropDownBtn.Enabled := Enable;
      end;
    CSC_NAVIGATEBACK:
      begin
        BackBtn.Enabled := Enable;
        BackDropDownBtn.Enabled := Enable;
      end;
  end;
end;  

procedure TForm1.FormShow(Sender: TObject);
begin
  EmbeddedWb1.AssignEmptyDocument;
  IETravellog1.Connect;
end;

procedure TForm1.EmbeddedWb1BeforeNavigate2(Sender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  IEAddress1.Text := Url;
end;

end.

