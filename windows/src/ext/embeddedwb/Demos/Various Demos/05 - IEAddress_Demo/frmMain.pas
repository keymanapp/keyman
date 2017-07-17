{*******************************************************}
{              IEAddress Demo   31/05/2006              }
{  by  Eran Bodankin (bsalsa) bsalsa@bsalsa.com         }
{                       Enjoy!                          }
{               For Delphi 5 - 2009                     }
{   UPDATES:                                            }
{               http://www.bsalsa.com                   }
{*******************************************************}
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
3. Mail me (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. You may consider donation in our web site!
{*******************************************************************************}

unit frmMain;

interface

uses
  Windows, EWBAcc, sysUtils, Graphics, Classes, Controls, Forms, EmbeddedWB, StdCtrls, IEAddress,
  ExtCtrls, Spin, SHDocVw_EWB, LinkLabel, ImgList, ComCtrls, OleCtrls, ActiveX, Dialogs,
  EwbCore;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    chkHasDropDown: TCheckBox;
    chkRegistryUpdate: TCheckBox;
    chkHasBorder: TCheckBox;
    IEAddress1: TIEAddress;
    cbShowFavicons: TCheckBox;
    cbShowAppIconToHtml: TCheckBox;
    chkAutoNavOnSelectLV: TCheckBox;
    Label3: TLabel;
    edtDLL: TEdit;
    Button2: TButton;
    chkAutoNavigateOnDblClk: TCheckBox;
    chkAutoNavigateOnEnterKey: TCheckBox;
    SpinDropDownCount: TSpinEdit;
    Image1: TImage;
    Image2: TImage;
    Label2: TLabel;
    Label4: TLabel;
    ImageList1: TImageList;
    EmbeddedWB1: TEmbeddedWB;
    procedure IEAddress1GetAppIcon(Sender: TObject; var Cancel: Boolean;
      Icon: TIcon);
    procedure IEAddress1GetFavicon(Sender: TObject; Favicon,
      SiteUrl: WideString; var Cancel: Boolean; Icon: TIcon);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure chkAutoNavOnSelectLVClick(Sender: TObject);
    procedure cbShowAppIconToHtmlClick(Sender: TObject);
    procedure cbShowFaviconsClick(Sender: TObject);
    procedure chkHasBorderClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure chkAutoNavigateOnDblClkClick(Sender: TObject);
    procedure chkAutoNavigateOnEnterKeyClick(Sender: TObject);
    procedure chkHasDropDownClick(Sender: TObject);
    procedure chkRegistryUpdateClick(Sender: TObject);
    procedure SpinDropDownCountChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.chkAutoNavOnSelectLVClick(Sender: TObject);
begin
  IEAddress1.AutoNavigateOnSelected := chkAutoNavOnSelectLV.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  EmbeddedWB1.Go(IEAddress1.Text);
  edtDll.Text := FloatToStr(IEAddress1.GetDllVersion('Shlwapi.dll'));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  IEAddress1.DroppedDown := True;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  IEAddress1.Flat := True;
end;

procedure TForm1.cbShowAppIconToHtmlClick(Sender: TObject);
begin
  IEAddress1.UseAppIcon := cbShowAppIconToHtml.Checked;
end;

procedure TForm1.cbShowFaviconsClick(Sender: TObject);
begin
  IEAddress1.ShowFavicon := cbShowFavicons.Checked;
end;

procedure TForm1.chkAutoNavigateOnDblClkClick(Sender: TObject);
begin
  IEAddress1.AutoNavigateOnDblClk := chkAutoNavigateOnDblClk.Checked;
end;

procedure TForm1.chkAutoNavigateOnEnterKeyClick(Sender: TObject);
begin
  IEAddress1.AutoNavigateOnEnterKey := chkAutoNavigateOnEnterKey.Checked;
end;

procedure TForm1.chkHasDropDownClick(Sender: TObject);
begin
  IEAddress1.HasDropDown := chkHasDropDown.Checked;
end;

procedure TForm1.chkRegistryUpdateClick(Sender: TObject);
begin
  IEAddress1.UpdateItemsToRegistry := chkRegistryUpdate.Checked;
end;

procedure TForm1.IEAddress1GetAppIcon(Sender: TObject; var Cancel: Boolean;
  Icon: TIcon);
begin
  if IEAddress1.UseAppIcon then
  begin
    Image2.Picture.Icon := Icon;
    Label4.Caption := 'App Icon:'
  end;
end;

procedure TForm1.IEAddress1GetFavicon(Sender: TObject; Favicon,
  SiteUrl: WideString; var Cancel: Boolean; Icon: TIcon);
begin
  if IEAddress1.ShowFavicon then
  begin
    Image1.Picture.Icon := Icon;
    Label2.Caption := 'Favicon:';
  end;
end;

procedure TForm1.SpinDropDownCountChange(Sender: TObject);
begin
  IEAddress1.DropDownCount := SpinDropDownCount.Value;
end;

procedure TForm1.chkHasBorderClick(Sender: TObject);
begin
  IEAddress1.HasBorder := chkHasBorder.Checked;
end;

end.

