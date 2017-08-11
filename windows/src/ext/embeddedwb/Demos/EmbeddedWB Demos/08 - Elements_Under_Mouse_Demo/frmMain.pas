//*************************************************************
//          EmbeddedWB - Elements Under Mouse Demo            *
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

You may use, change or modify the demo under 3 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please,  consider donation in our web site!
{*******************************************************************************}

unit frmMain;

{$I EWB.inc}

interface

uses
  Windows, Forms,  ExtCtrls, Controls, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}
  IEAddress, OleCtrls, MSHTML_EWB, SHDocVw_EWB, EwbCore, EmbeddedWB, Classes, StdCtrls, SysUtils;

type
  TfrmElements = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    Panel1: TPanel;
    edtTag: TEdit;
    edtID: TEdit;
    edtInnerText: TEdit;
    edtInnerHtml: TEdit;
    edtHref: TEdit;
    Timer1: TTimer;
    edtClassName: TEdit;
    Panel2: TPanel;
    Button1: TButton;
    IEAddress1: TIEAddress;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label3: TLabel;
    edtFrame: TEdit;
    Label7: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure EmbeddedWB1WindowClosing(ASender: TObject;
      IsChildWindow: WordBool; var Cancel: WordBool);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  frmElements: TfrmElements;

implementation

{$R *.dfm}

function OleVarToStr(const V: OleVariant): string;
begin
{$IFDEF DELPHI_6_UP}
  if VarIsStr(V) or VarIsNumeric(V) or (VarType(V) = varDate) then
    Result := V
  else
    Result := '';
{$ELSE}
  if VarIsNull(v) then
    Result := ''
  else
    Result := string(V)
{$ENDIF}
end;

procedure TfrmElements.Button1Click(Sender: TObject);
begin
  EmbeddedWB1.Go(IEAddress1.Text);
  Timer1.Enabled := True;
end;

procedure TfrmElements.Timer1Timer(Sender: TObject);
var
  MousePos: TPoint;
  HtmlElement: IHTMLElement;
  ImgElement: IHTMLIMGElement;
  AnchorElement: IHTMLAnchorElement;
  iHTMLDoc: IHtmlDocument2;
  LinkElement: IHTMLLinkElement;
  varName, varSource: OleVariant;
begin
  if Supports(EmbeddedWB1.Document, IHtmlDocument2, iHTMLDoc) then
  begin
    if GetCursorPos(MousePos) then
    begin
      MousePos := EmbeddedWB1.ScreenToClient(MousePos);
      HtmlElement := iHTMLDoc.ElementFromPoint(MousePos.X, MousePos.Y);
      if Assigned(HtmlElement) then
      begin
        try
          edtTag.Text := HtmlElement.tagName;
          edtId.Text := HtmlElement.id;
          edtInnerText.Text := HtmlElement.innerText;

          edtInnerHTML.Text := HtmlElement.innerHTML;
          edtClassName.Text := HtmlElement.className;
          HtmlElement.QueryInterface(IHTMLIMGElement, imgElement);

          edthref.Text := '';

          if Assigned(imgElement) then
            edthref.Text := imgElement.href;

          HtmlElement.QueryInterface(IHTMLAnchorElement, AnchorElement);
          if Assigned(AnchorElement) then
            edthref.Text := AnchorElement.href;

          HtmlElement.QueryInterface(IHTMLLinkElement, LinkElement);
          if Assigned(LinkElement) then
            edthref.Text := LinkElement.href;

          varName := HtmlElement.GetAttribute('Name', 0);
          varSource := HtmlElement.GetAttribute('Source', 0);

          edtFrame.Text := OleVarToStr(varName) + '  ' + OleVarToStr(varSource);
        except
        end;
      end;
    end;
  end;
end;

procedure TfrmElements.EmbeddedWB1WindowClosing(ASender: TObject;
  IsChildWindow: WordBool; var Cancel: WordBool);
begin
  Cancel := True;
  (ASender as TEmbeddedWB).GoAboutBlank;
end;

end.

