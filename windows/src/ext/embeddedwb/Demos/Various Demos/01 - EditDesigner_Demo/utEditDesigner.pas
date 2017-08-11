{*******************************************************}
{              EditDesigner Demo                        }
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

unit utEditDesigner;

interface

uses
  EwbAcc, Windows, Classes, Dialogs, ShellAPI, SHDocVw_EWB, EmbeddedWB, MSHTML_Ewb, EditDesigner, Forms,
  Controls, ExtCtrls, StdCtrls, IEAddress, ComCtrls, SysUtils, OleCtrls, Menus,
  EwbCore;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    EditDesigner1: TEditDesigner;
    Panel2: TPanel;
    btnGo: TButton;
    IEAddress1: TIEAddress;
    StatusBar1: TStatusBar;
    MemoDocumentInfo: TMemo;
    StatusBar2: TStatusBar;
    EmbeddedWB1: TEmbeddedWB;
    MemoInnerText: TMemo;
    MemoInnerHTML: TMemo;
    btnRemoveDesigner: TButton;
    btnConnectDesigner: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MainMenu1: TMainMenu;
    Add1: TMenuItem;
    HyperLink1: TMenuItem;
    Image1: TMenuItem;
    File1: TMenuItem;
    SaveToFile1: TMenuItem;
    Fonts1: TMenuItem;
    Bold1: TMenuItem;
    SetSelTextAsUndelline1: TMenuItem;
    Italic1: TMenuItem;
    RadioButton1: TMenuItem;
    Exit1: TMenuItem;
    procedure Italic1Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure SetSelTextAsUndelline1Click(Sender: TObject);
    procedure Bold1Click(Sender: TObject);
    procedure SaveToFile1Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure HyperLink1Click(Sender: TObject);
    function EditDesigner1PreDrag: HRESULT;
    function EditDesigner1SnapRect(const pIElement: IHTMLElement;
      var prcNew: TRect; eHandle: TOleEnum): HRESULT;
    procedure EditDesigner1InnerText(const innerText: string);
    procedure EditDesigner1InnerHtml(const innerHtml: string);
    procedure EditDesigner1ToString(const toString: string);
    procedure EditDesigner1KeyState(const CapsLock, NumLock, InsertKey, altKey,
      ctrlKey, shiftKey: Boolean);
    procedure EditDesigner1EvtDispId(const inEvtDispId: Integer);
    procedure EditDesigner1Type_(const type_: string);
    procedure EditDesigner1TagName(const tagName: string);
    procedure EditDesigner1MouseButton(const Button: Integer);
    procedure EditDesigner1KeyPress(const Key: Integer);
    procedure EditDesigner1Error(const ErrorCode: Integer; ErrMessage: string);
    procedure EditDesigner1MousePosition(X, Y: Integer);
    procedure btnRemoveDesignerClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    function EditDesigner1PostEditorEventNotify(inEvtDispId: Integer;
      const pIEventObj: IHTMLEventObj): HRESULT;
    function EditDesigner1PostHandleEvent(inEvtDispId: Integer;
      const pIEventObj: IHTMLEventObj): HRESULT;
    function EditDesigner1PreHandleEvent(inEvtDispId: Integer;
      const pIEventObj: IHTMLEventObj): HRESULT;
    function EditDesigner1TranslateAccelerator(inEvtDispId: Integer;
      const pIEventObj: IHTMLEventObj): HRESULT;
    procedure btnConnectDesignerClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    procedure UpdatePageProperties;
    procedure UpdateDesigner;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//EditDesigner Procedures-------------------------------------------------------

procedure TForm1.EditDesigner1MousePosition(X, Y: Integer);
begin
  StatusBar1.Panels[0].Text := 'X: ' + IntToStr(X);
  StatusBar1.Panels[1].Text := 'Y: ' + IntToStr(Y);
end;

procedure TForm1.EditDesigner1TagName(const tagName: string);
begin
  StatusBar1.Panels[2].Text := 'Tag: ' + tagName;
end;

procedure TForm1.EditDesigner1Type_(const type_: string);
begin
  StatusBar1.Panels[3].Text := 'Type: ' + type_;
end;

procedure TForm1.HyperLink1Click(Sender: TObject);
begin
  EditDesigner1.InsertHyperlink;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  EditDesigner1.InsertImage;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  EditDesigner1.InsertRadioButton;
end;

procedure TForm1.SaveToFile1Click(Sender: TObject);
var
  sFileName: string;
begin
  try
    sFileName := ExtractFilePath(Paramstr(0)) + 'Demo.html';
    if EditDesigner1.SaveToFile(SFileName) = S_OK then
      ShellExecute(Forms.Application.Handle, 'explore', PChar(SFileName), nil,
        nil, SW_SHOWNORMAL);
  except
    ShowMessage('Error while Saving.')
  end;
end;

procedure TForm1.EditDesigner1KeyPress(const Key: Integer);
begin
  StatusBar1.Panels[4].Text := 'Key: ' + IntToStr(Key);
end;

procedure TForm1.EditDesigner1MouseButton(const Button: Integer);
begin
  StatusBar1.Panels[5].Text := 'Button: ' + IntToStr(Button);
end;

procedure TForm1.EditDesigner1EvtDispId(const inEvtDispId: Integer);
begin
  StatusBar1.Panels[6].Text := 'DispId: ' + IntToStr(inEvtDispId);
end;

procedure TForm1.EditDesigner1InnerHtml(const innerHtml: string);
begin
  MemoInnerHTML.Lines.Text := innerHtml;
end;

procedure TForm1.EditDesigner1InnerText(const innerText: string);
begin
  MemoInnerText.Lines.Text := innerText;
end;

procedure TForm1.EditDesigner1Error(const ErrorCode: Integer;
  ErrMessage: string);
begin
  StatusBar1.Panels[7].Text := ErrMessage;
end;

procedure TForm1.EditDesigner1ToString(const toString: string);
begin
  StatusBar2.Panels[6].Text := toString;
end;

function TForm1.EditDesigner1PostEditorEventNotify(inEvtDispId: Integer;
  const pIEventObj: IHTMLEventObj): HRESULT;
begin
  Result := S_FALSE;
end;

function TForm1.EditDesigner1PostHandleEvent(inEvtDispId: Integer;
  const pIEventObj: IHTMLEventObj): HRESULT;
begin
  Result := S_FALSE;
end;

function TForm1.EditDesigner1PreDrag: HRESULT;
begin
  Result := S_FALSE;
end;

function TForm1.EditDesigner1PreHandleEvent(inEvtDispId: Integer;
  const pIEventObj: IHTMLEventObj): HRESULT;
begin
  Result := S_FALSE;
end;

function TForm1.EditDesigner1TranslateAccelerator(inEvtDispId: Integer;
  const pIEventObj: IHTMLEventObj): HRESULT;
begin
  Result := S_FALSE;
end;

function TForm1.EditDesigner1SnapRect(const pIElement: IHTMLElement;
  var prcNew: TRect; eHandle: TOleEnum): HRESULT;
begin
  case eHandle of
    ELEMENT_CORNER_NONE: ; // Code for moving the element
    ELEMENT_CORNER_TOP: ; // Code for resizing the element
    ELEMENT_CORNER_LEFT: ; // Code for resizing the element
    ELEMENT_CORNER_BOTTOM: ; // Code for resizing the element
    ELEMENT_CORNER_RIGHT: ; // Code for resizing the element
    ELEMENT_CORNER_TOPLEFT: ; // Code for resizing the element
    ELEMENT_CORNER_TOPRIGHT: ; // Code for resizing the element
    ELEMENT_CORNER_BOTTOMLEFT: ; // Code for resizing the element
    ELEMENT_CORNER_BOTTOMRIGHT: ; // Code for resizing the element
  end;
  Result := S_OK;
end;

procedure TForm1.EditDesigner1KeyState(const CapsLock, NumLock, InsertKey,
  altKey, ctrlKey, shiftKey: Boolean);
const
  OnOff: array[Boolean] of string = ('Off', 'On');
begin
  StatusBar2.Panels[0].Text := 'Alt: ' + OnOff[altKey];
  StatusBar2.Panels[1].Text := 'Ctrl: ' + OnOff[ctrlKey];
  StatusBar2.Panels[2].Text := 'Shift: ' + OnOff[shiftKey];
  StatusBar2.Panels[3].Text := 'CapsLock: ' + OnOff[CapsLock];
  StatusBar2.Panels[4].Text := 'NumLock: ' + OnOff[NumLock];
  StatusBar2.Panels[5].Text := 'Insert: ' + OnOff[InsertKey];
end;

//Button Procedures-------------------------------------------------------------

procedure TForm1.Bold1Click(Sender: TObject);
begin
  EditDesigner1.SetFontBold;
end;

procedure TForm1.SetSelTextAsUndelline1Click(Sender: TObject);
begin
  EditDesigner1.SetFontUnderline;
end;

procedure TForm1.Italic1Click(Sender: TObject);
begin
  EditDesigner1.SetFontItalic;
end;

procedure TForm1.btnConnectDesignerClick(Sender: TObject);
begin
  UpdateDesigner;
end;

procedure TForm1.btnRemoveDesignerClick(Sender: TObject);
var
  I: Integer;
begin
  if EditDesigner1.RemoveDesigner = S_OK then
  begin
    btnRemoveDesigner.Enabled := False;
    btnConnectDesigner.Enabled := not btnRemoveDesigner.Enabled;
    for I := 0 to Pred(StatusBar1.Panels.Count) do
      StatusBar1.Panels[I].Text := '';
  end;
end;

procedure TForm1.btnGoClick(Sender: TObject);
begin
  EmbeddedWB1.Go(IEAddress1.Text);
  UpdateDesigner;
end;

//Private Procedures------------------------------------------------------------

procedure TForm1.UpdatePageProperties;
begin
  MemoDocumentInfo.Clear;
  MemoDocumentInfo.Lines.AddStrings(EditDesigner1.GetPageProperties);
end;

procedure TForm1.UpdateDesigner;
begin
  if EditDesigner1.ConnectDesigner = S_OK then
  begin
    btnConnectDesigner.Enabled := False;
    btnRemoveDesigner.Enabled := not btnConnectDesigner.Enabled;
    UpdatePageProperties;
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

end.

