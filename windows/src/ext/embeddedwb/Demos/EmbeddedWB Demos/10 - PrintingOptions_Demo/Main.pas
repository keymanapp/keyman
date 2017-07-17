//*************************************************************
//                    EmbeddedWB - Print  Demo                *
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

unit Main;

interface

uses
  Controls, Classes, Windows, Messages, Forms, Dialogs, Menus,
  EmbeddedWB, OleCtrls, SHDocVw_EWB, EwbCore;

type
  TPrintOptions = record
    Copies: Integer; //Number of copies
    Portrait: Boolean; //Paper orientation
    Left: string; //Left Margin
    Top: string; //Top Margin
    Right: string; //Right Margin
    Bottom: string; //Bottom Margin
    Header: string; //Header string
    Footer: string; //Footer string
  end;

var
  PrintOptions: TPrintOptions;
  bPrintWithOpt: Boolean;

type
  TMainFrm = class(TForm)
    Exit: TMenuItem;
    FileMenu: TMenuItem;
    LandscapeNoDlg: TMenuItem;
    LanscapeDlg: TMenuItem;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    PortraitDlg: TMenuItem;
    PortraiteNoDlg: TMenuItem;
    Print: TMenuItem;
    PrintPreview: TMenuItem;
    PrintSetup: TMenuItem;
    PrintWithDlg: TMenuItem;
    PrintWithOptions: TMenuItem;
    PrintWithOutDlg: TMenuItem;
    EmbeddedWB1: TEmbeddedWB;
    PrintPreviewMaximized: TMenuItem;
    procedure PrintPreviewMaximizedClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LandscapeNoDlgClick(Sender: TObject);
    procedure LanscapeDlgClick(Sender: TObject);
    procedure PortraitDlgClick(Sender: TObject);
    procedure PortraiteNoDlgClick(Sender: TObject);
    procedure PrintClick(Sender: TObject);
    procedure PrintPreviewClick(Sender: TObject);
    procedure PrintSetupClick(Sender: TObject);
    procedure PrintWithOptionsClick(Sender: TObject);

  private
   { Private declarations }
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;

  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}
//----------Private Section--------------------------------

procedure TMainFrm.WMActivate(var Msg: TWMActivate);
var
  S: string;
  wnd: HWND;
  I: Integer;
begin
  if bPrintWithOpt and (msg.active = 0) then
  begin
    bPrintWithOpt := false;
    wnd := Msg.ActiveWindow;
    I := GetWindowTextLength(wnd);
    SetLength(S, I + 1);
    GetWindowText(Wnd, PChar(S), I + 1);
    if (S = 'Print'#0) then
    begin
      SetDlgItemInt(wnd, 1154, PrintOptions.Copies, False);
      SendDlgItemMessage(Wnd, 1, BM_CLICK, 0, 0);
      bPrintWithOpt := True;
    end
    else
      if S = 'Page Setup'#0 then
      begin
        if PrintOptions.Portrait then
          SendDlgItemMessage(Wnd, 1056, BM_CLICK, 0, 0)
        else
          SendDlgItemMessage(Wnd, 1057, BM_CLICK, 0, 0);

        SetDlgItemText(wnd, 8147, PChar(PrintOptions.Header));
        SetDlgItemText(wnd, 8149, PChar(PrintOptions.Footer));
        SetDlgItemText(wnd, 1155, PChar(PrintOptions.Left));
        SetDlgItemText(wnd, 1156, PChar(PrintOptions.Top));
        SetDlgItemText(wnd, 1157, PChar(PrintOptions.Right));
        SetDlgItemText(wnd, 1158, PChar(PrintOptions.Bottom));
        SendDlgItemMessage(Wnd, 1, BM_CLICK, 0, 0);
        bPrintWithOpt := True;
      end;
  end;
end;

//-----End of private section-----------------------------------------------

procedure TMainFrm.FormShow(Sender: TObject);
begin
  EmbeddedWB1.Navigate('bsalsa.com');
end;

procedure TMainFrm.ExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainFrm.PrintClick(Sender: TObject);
begin
  EmbeddedWB1.PrintOptions.HideSetup := true;
  EmbeddedWB1.Print;
end;

procedure TMainFrm.PrintPreviewClick(Sender: TObject);
begin
  EmbeddedWB1.PrintPreView;
end;

procedure TMainFrm.PrintSetupClick(Sender: TObject);
begin
  EmbeddedWB1.PrintSetup;
end;

procedure TMainFrm.PrintWithOptionsClick(Sender: TObject);
begin
  EmbeddedWB1.PrintWithOptions;
end;

procedure TMainFrm.LanscapeDlgClick(Sender: TObject);
begin
  with EmbeddedWB1.PrintOptions do
  begin
    Orientation := poLandscape;
    Margins.Left := 16.75; // always use the unit of measurement defined on your own computer.
    Header := 'This is my new header';
  end;
  EmbeddedWB1.PrintWithOptions;
end;

procedure TMainFrm.PortraitDlgClick(Sender: TObject);
begin
  with EmbeddedWB1.PrintOptions do
  begin
    Orientation := poPortrait;
    Margins.Left := 16.75; // always use the unit of measurement defined on your own computer.
    Header := 'This is my new header';
  end;
  EmbeddedWB1.PrintWithOptions;
end;

procedure TMainFrm.LandscapeNoDlgClick(Sender: TObject);
begin
  with PrintOptions do
  begin
    Copies := 1;
    Portrait := False;
    Footer := 'This is my Footer';
    Header := 'This is my Header';
    Left := '25,0';
    Top := '20,0';
    Right := '22,0';
    Bottom := '19,7';
  end;
  bPrintWithOpt := True;
  Embeddedwb1.PageSetup(true);
  EmbeddedWB1.Print;
  bPrintWithOpt := False;
end;

procedure TMainFrm.PortraiteNoDlgClick(Sender: TObject);
begin
  with PrintOptions do
  begin
    Copies := 1;
    Portrait := True;
    Footer := 'This is my Footer';
    Header := 'This is my Header';
    Left := '25,0';
    Top := '20,0';
    Right := '22,0';
    Bottom := '19,7';
  end;
  bPrintWithOpt := True;
  if Embeddedwb1.PageSetup(True) then
    EmbeddedWB1.Print;
  bPrintWithOpt := False;
end;

procedure TMainFrm.PrintPreviewMaximizedClick(Sender: TObject);
begin
  EmbeddedWB1.PrintPreviewExtended(SW_MAXIMIZE);
end;

end.

