//*************************************************************
//                         Menus Demo                         *
//                       For Delphi                           *
//                            by                              *
//          Eran Bodankin (bsalsa) -(bsalsa@bsalsa.com)       *
//                                                            *
//       Documentation and updated versions:                  *
//                                                            *
//               http://www.bsalsa.com                        *
//*************************************************************
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

You may use/ change/ modify the component under 4 conditions:
1. In your web site, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit  for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

{
  MSDN info:
  http://msdn.microsoft.com/en-us/library/aa753264(VS.85).aspx
}

unit FrmMain;

interface

uses
  Windows, Classes, Forms, Menus, Controls, OleCtrls, EmbeddedWB,
  SHDocVw_EWB, EwbCore, IEConst, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    DisableAllMenu1: TMenuItem;
    DisableAnchorMenu1: TMenuItem;
    DisableControlsMenu1: TMenuItem;
    DisableCtrlN1: TMenuItem;
    DisableDebugMenu1: TMenuItem;
    DisableDefaultMenu1: TMenuItem;
    DisableImageArtMenu1: TMenuItem;
    DisableImagesMenu1: TMenuItem;
    DisableImgDynSrcMenu1: TMenuItem;
    DisableOpenInANewWindow1: TMenuItem;
    DisableSelectedTextMenu1: TMenuItem;
    DisableTableMenu1: TMenuItem;
    DisableUnknownMenu1: TMenuItem;
    DisableViewSource1: TMenuItem;
    EnableAllMenus: TMenuItem;
    Exit1: TMenuItem;
    Note2: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Note1: TMenuItem;
    RightClickMenu1: TMenuItem;
    Shortcuts1: TMenuItem;
    DisableOpenLink1: TMenuItem;
    PopupMenu1: TPopupMenu;
    wer1: TMenuItem;
    ShowMyPopup1: TMenuItem;
    DisableCtrlP1: TMenuItem;
    DisableCtrlA1: TMenuItem;
    EnableAll1: TMenuItem;
    N6: TMenuItem;
    procedure DisableCtrlA1Click(Sender: TObject);
    procedure DisableCtrlP1Click(Sender: TObject);
    procedure DisableCtrlN1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure EnableAll1Click(Sender: TObject);
    procedure EmbeddedWB1ShowContextMenu(Sender: TCustomEmbeddedWB;
      const dwID: Cardinal; const ppt: PPoint;
      const CommandTarget: IUnknown; const Context: IDispatch;
      var Result: HRESULT);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.EmbeddedWB1ShowContextMenu(Sender: TCustomEmbeddedWB;
  const dwID: Cardinal; const ppt: PPoint; const CommandTarget: IUnknown;
  const Context: IDispatch; var Result: HRESULT);
begin
//=====Disable Menus=======================================================
  case dwID of
    CONTEXT_MENU_DEFAULT:
      if DisableDefaultMenu1.Checked then
        Result := S_OK;
    CONTEXT_MENU_ANCHOR:
      if DisableAnchorMenu1.Checked then
        Result := S_OK;
    CONTEXT_MENU_CONTROL:
      if DisableControlsMenu1.Checked then
        Result := S_OK;
    CONTEXT_MENU_DEBUG:
      if DisableDebugMenu1.Checked then
        Result := S_OK;
    CONTEXT_MENU_IMAGE:
      if DisableImagesMenu1.Checked then
        Result := S_OK;
    CONTEXT_MENU_IMGART:
      if DisableImageArtMenu1.Checked then
        Result := S_OK;
    CONTEXT_MENU_IMGDYNSRC:
      if DisableImgDynSrcMenu1.Checked then
        Result := S_OK;
    CONTEXT_MENU_TABLE:
      if DisableTableMenu1.Checked then
        Result := S_OK;
    CONTEXT_MENU_TEXTSELECT:
      if DisableSelectedTextMenu1.Checked then
        Result := S_OK;
    CONTEXT_MENU_UNKNOWN:
      if DisableUnknownMenu1.Checked then
        Result := S_OK;
  end;

//=====Disable Menu Items=======================================================

  with EmbeddedWB1 do
  begin
  if DisableViewSource1.Checked then
    DisabledPopupMenuItems := DisabledPopupMenuItems + [rcsViewSource]
  else
    DisabledPopupMenuItems := DisabledPopupMenuItems - [rcsViewSource];

  if DisableOpenInANewWindow1.Checked then
    DisabledPopupMenuItems := DisabledPopupMenuItems + [rcsOpenNWindow]
  else
    DisabledPopupMenuItems := DisabledPopupMenuItems - [rcsOpenNWindow];

  if DisableOpenLink1.Checked then
    DisabledPopupMenuItems := DisabledPopupMenuItems + [rcsOpenLink]
  else
    DisabledPopupMenuItems := DisabledPopupMenuItems - [rcsOpenLink];
  end;

  if DisableAllMenu1.checked then
      Result := S_OK;

//===Show your own Popup=====================================================
  if ShowMyPopup1.Checked then
  begin
    PopupMenu1.Popup(ppt.x, ppt.y);
    Result := S_OK;
  end;
end;

procedure TForm1.DisableCtrlA1Click(Sender: TObject);
begin
  if DisableCtrlA1.Checked then
    EmbeddedWB1.DisableCtrlShortcuts := 'A';
end;

procedure TForm1.DisableCtrlN1Click(Sender: TObject);
begin
  if DisableCtrlN1.Checked then
    EmbeddedWB1.DisableCtrlShortcuts :=  'N';
end;

procedure TForm1.DisableCtrlP1Click(Sender: TObject);
begin
  if DisableCtrlP1.Checked then
    EmbeddedWB1.DisableCtrlShortcuts :=  'P';
end;

procedure TForm1.EnableAll1Click(Sender: TObject);
begin
  EmbeddedWB1.DisableCtrlShortcuts := '';
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;



end.

