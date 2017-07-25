{*******************************************************}
{              SDI Embedded Web Browser Demo            }
{  By:                                                  }
{  Eran Bodankin (bsalsa) bsalsa@bsalsa.com             }
{                       Enjoy!                          }
{   Documentation and updated versions:                 }
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

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit frmMain;

interface

uses
  Classes, Controls, Forms, OleCtrls, EmbeddedWB, SHDocVw_EWB, Dialogs,
  StdCtrls, ExtCtrls, Windows, Messages, EwbCore, ComCtrls;

type
  TForm1 = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    pnlAddressBar: TPanel;
    edUrl: TEdit;
    btnGo: TButton;
    StatusBar1: TStatusBar;
    procedure EmbeddedWB1Move(Sender: TCustomEmbeddedWB; cx, cy: Integer);
    procedure EmbeddedWB1MoveBy(Sender: TCustomEmbeddedWB; cx, cy: Integer);
    procedure EmbeddedWB1Resize(Sender: TCustomEmbeddedWB; cx, cy: Integer);
    procedure EmbeddedWB1NewWindow2(ASender: TObject; var ppDisp: IDispatch;
      var Cancel: WordBool);
    procedure FormShow(Sender: TObject);
    procedure EmbeddedWB1AddressBar(Sender: TObject; AddressBar: WordBool);
    procedure edUrlKeyPress(Sender: TObject; var Key: Char);
    procedure EmbeddedWB1NewWindow3(ASender: TObject;
      var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
      const bstrUrlContext, bstrUrl: WideString);
    procedure EmbeddedWB1WindowSetHeight(ASender: TObject;
      Height: Integer);
    procedure btnGoClick(Sender: TObject);
    procedure EmbeddedWB1WindowSetLeft(ASender: TObject; Left: Integer);
    procedure EmbeddedWB1WindowSetResizable(ASender: TObject;
      Resizable: WordBool);
    procedure EmbeddedWB1WindowSetWidth(ASender: TObject; Width: Integer);
    procedure FormCreate(Sender: TObject);
    procedure EmbeddedWB1Visible(Sender: TObject; Visible: WordBool);
    procedure EmbeddedWB1WindowSetTop(ASender: TObject; Top: Integer);
  private
    { Private declarations }
    m_Rect: TRect;
    m_bCreatedManually: Boolean;
    m_bResizable: Boolean;
    m_bFullScreen: Boolean;
  protected
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  SysUtils;

const
  SZ_BOOL: array[boolean] of string = ('False', 'True');

var
  iBorderThick,
  iBorderSize,
  iCaptSize: Integer;

procedure TForm1.FormShow(Sender: TObject);
begin
  EmbeddedWB1.Go(edUrl.Text);
end;

procedure TForm1.EmbeddedWB1NewWindow2(ASender: TObject; var ppDisp: IDispatch;
  var Cancel: WordBool);
var
  NewApp: TForm1;
begin
  Application.CreateForm(TForm1, NewApp);
  NewApp.m_bCreatedManually := m_bCreatedManually;
  m_bCreatedManually := False;
  ppdisp := NewApp.EmbeddedWB1.Application;
end;

procedure TForm1.EmbeddedWB1AddressBar(Sender: TObject;
  AddressBar: WordBool);
begin
  pnlAddressBar.Visible := AddressBar;
end;

procedure TForm1.edUrlKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    Key := #0;
    EmbeddedWB1.Go(edUrl.Text);
 end;
end;

procedure TForm1.EmbeddedWB1NewWindow3(ASender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: Cardinal;
  const bstrUrlContext, bstrUrl: WideString);
var
  NewApp: TForm1;
begin
  ShowMessage(bstrUrl);
  Application.CreateForm(TForm1, NewApp);
  NewApp.m_bCreatedManually := m_bCreatedManually;
  m_bCreatedManually := False;
  ppdisp := NewApp.EmbeddedWB1.Application;
end;

procedure TForm1.EmbeddedWB1Move(Sender: TCustomEmbeddedWB; cx, cy: Integer);
begin
  if (WindowState = wsNormal) then begin
    SetBounds(cX, cY, Width, Height);
    OutputDebugString(PChar(Format('Move x:%d, y:%d', [cX, cY])));
    Statusbar1.SimpleText := Format('Move x:%d, y:%d', [cX, cY]);
  end;
end;

procedure TForm1.EmbeddedWB1MoveBy(Sender: TCustomEmbeddedWB; cx, cy: Integer);
begin
  if (WindowState = wsNormal) then begin
    OutputDebugString(PChar(Format('MoveBy cx:%d, cy:%d', [cx, cy])));
    Statusbar1.SimpleText := (Format('MoveBy cx:%d, cy:%d', [cx, cy]));
    SetBounds(Left + cx, Top + cy, Width, Height);
  end;
end;

procedure TForm1.btnGoClick(Sender: TObject);
begin
  EmbeddedWB1.Go(edUrl.Text);
end;

procedure TForm1.EmbeddedWB1Resize(Sender: TCustomEmbeddedWB; cx, cy: Integer);
begin
  if (WindowState = wsNormal) then begin
    OutputDebugString(PChar(Format('Resize Width:%d, Height:%d', [Width, Height])));
    SetBounds(Left, Top, Width, Height);
    Statusbar1.SimpleText := (Format('Resize Width:%d, Height:%d', [Width, Height]));
  end;
end;

procedure TForm1.EmbeddedWB1WindowSetLeft(ASender: TObject; Left: Integer);
begin
  OutputDebugString(PChar(Format('SetLeft: %d', [Top])));
  m_Rect.Left := Left;
  Statusbar1.SimpleText := Format('SetLeft: %d', [Top]);
end;

procedure TForm1.EmbeddedWB1WindowSetTop(ASender: TObject; Top: Integer);
begin
  OutputDebugString(PChar(Format('SetTop: %d', [Top])));
  m_Rect.Top := Top;
  Statusbar1.SimpleText := Format('SetTop: %d', [Top]);
end;

procedure TForm1.EmbeddedWB1WindowSetWidth(ASender: TObject;
  Width: Integer);
begin
  OutputDebugString(PChar(Format('SetWidth: %d', [Top])));
  m_Rect.Right := Width;
  Statusbar1.SimpleText := Format('SetWidth: %d', [Top]);
end;

procedure TForm1.EmbeddedWB1WindowSetHeight(ASender: TObject;
  Height: Integer);
begin
  OutputDebugString(PChar(Format('SetHeight: %d', [Top])));
  m_Rect.Bottom := Height;
  Statusbar1.SimpleText := Format('SetHeight: %d', [Top]);
end;

procedure TForm1.EmbeddedWB1WindowSetResizable(ASender: TObject;
  Resizable: WordBool);
begin
  OutputDebugString(PChar('SetResizable: ' + SZ_BOOL[Resizable]));
  m_bResizable := Resizable;
  Statusbar1.SimpleText := ('SetResizable: ' + SZ_BOOL[Resizable]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  m_Rect := Rect(0, 0, 0, 0);
  m_bResizable := True;
  m_bCreatedManually := False;
  m_bFullScreen := False;
end;

procedure TForm1.EmbeddedWB1Visible(Sender: TObject; Visible: WordBool);
var
  dwClHeight: Integer;
begin
  OutputDebugString(PChar('Visible: ' + SZ_BOOL[Visible]));
  if not m_bCreatedManually {and not m_bFullScreen} then begin
    if m_bResizable then begin
      BorderStyle := bsSizeable;
      BorderIcons := BorderIcons + [biMaximize];
    end
    else begin
      BorderStyle := bsSingle;
      BorderIcons := BorderIcons - [biMaximize];
    end;
    HandleNeeded;
    {
      pnlStatusBar.Visible := m_bStatusBar;
      CoolBar.Bands[0].Visible := m_bToolBar;
      if m_bAddressBar then
       CoolBar.Bands.FindBand(tbAddress).Break := m_bToolBar;
      CoolBar.Bands[2].Visible := m_bAddressBar;
      CoolBar.Visible := m_bToolBar or m_bAddressBar;
      pnlSep.Visible := CoolBar.Visible;
    }
    if (m_Rect.Right > 0) and (m_Rect.Bottom > 0) then begin
      dwClHeight := 0;

      if pnlAddressBar.Visible then
        dwClHeight := pnlAddressBar.Height;

      //  if m_bStatusBar then
      //    dwClHeight := dwClHeight + pnlStatusBar.Height;

      m_Rect.Bottom := (iBorderSize + iBorderThick) * 2 + iCaptSize + dwClHeight + m_Rect.Bottom;
      m_Rect.Right := (iBorderSize + iBorderThick) * 2 + m_Rect.Right;

      if m_bResizable then begin
        inc(m_Rect.Bottom, 2);
        inc(m_Rect.Right, 2);
      end;

      SetBounds(m_Rect.Left, m_Rect.Top, m_Rect.Right, m_Rect.Bottom);
    end;

    if m_bFullScreen then
      WindowState := wsMaximized;

  end
  else
    m_bCreatedManually := False;

  Self.Visible := Visible;
end;

initialization
  iCaptSize := GetSystemMetrics(SM_CYCAPTION);
  iBorderSize := GetSystemMetrics(SM_CXBORDER);
  iBorderThick := GetSystemMetrics(SM_CXSIZEFRAME);
end.

