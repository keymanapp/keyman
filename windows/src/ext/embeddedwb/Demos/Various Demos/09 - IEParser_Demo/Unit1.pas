//****************************************************
//             Extended IEParser Demo                *
//                Freeware Demo                      *
//                   by                              *
//                                                   *
//          Eran Bodankin (bsalsa)                   *
//            bsalsa@bsalsa.com                      *
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

You may use, change or modify the component under 3 conditions:
1. In your website, add a Link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit Unit1;

interface

{$I EWB.inc}

uses
  MSHTML_EWB, {$IFDEF DELPHI6_UP}Variants, {$ENDIF} SysUtils, Classes, Controls,
  Forms, StdCtrls, IEAddress, ExtCtrls, IEParser, ComCtrls, RichEditBrowser;

type
  TForm1 = class(TForm)
    edtAddress: TEdit;
    btnGo: TButton;
    IEParser1: TIEParser;
    Images: TTabSheet;
    memAnchor: TMemo;
    memBase: TMemo;
    memBaseFont: TMemo;
    memBody: TMemo;
    memBR: TMemo;
    memComment: TMemo;
    memDiv: TMemo;
    memFont: TMemo;
    memForm: TMemo;
    memHR: TMemo;
    memImages: TMemo;
    memMarquee: TMemo;
    memMeta: TMemo;
    memParse: TMemo;
    memParseError: TMemo;
    memProp: TMemo;
    memScript: TMemo;
    Meta: TTabSheet;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    tsAnchor: TTabSheet;
    tsBase: TTabSheet;
    tsBaseFont: TTabSheet;
    tsBody: TTabSheet;
    tsBR: TTabSheet;
    tsComment: TTabSheet;
    tsDiv: TTabSheet;
    tsFont: TTabSheet;
    tsForm: TTabSheet;
    tsHR: TTabSheet;
    tsMarquee: TTabSheet;
    tsParse: TTabSheet;
    tsPError: TTabSheet;
    tsProp: TTabSheet;
    tsScript: TTabSheet;
    Button1: TButton;
    procedure IEParser1BusyStateChange(Sender: TObject);
    procedure IEParser1Meta(Sender: TObject; HttpEquiv, Content, Name, URL,
      Charset: string; Element: TElementInfo);
    procedure IEParser1Anchor(Sender: TObject; hRef, Target, Rel, Rev, Urn,
      Methods, Name, Host, HostName, PathName, Port, Protocol, Search, Hash,
      AccessKey, ProtocolLong, MimeType, NameProp: string;
      Element: TElementInfo);
    procedure btnGoClick(Sender: TObject);
    procedure IEParser1Base(Sender: TObject; hRef, Target: string;
      Element: TElementInfo);
    procedure IEParser1BaseFont(Sender: TObject; Color: OleVariant;
      Face: string; Size: Integer; Element: TElementInfo);
    procedure IEParser1Body(Sender: TObject; Background, bgProperties: string;
      LeftMargin, TopMargin, RightMargin, BottomMargin, bgColor, Text, Link,
      vLink, aLink: OleVariant; NoWrap: Boolean; Element: TElementInfo);
    procedure IEParser1BR(Sender: TObject; Clear: string;
      Element: TElementInfo);
    procedure IEParser1Comment(sender: TObject; Text: string;
      Element: TElementInfo);
    procedure IEParser1Div(Sender: TObject; Align: string; NoWrap: Boolean;
      Element: TElementInfo);
    procedure IEParser1Font(Sender: TObject; Color, Size: OleVariant;
      Face: string; Element: TElementInfo);
    procedure IEParser1Form(Sender: TObject; Action, Dir, Encoding, Method,
      Target, Name: string; Element: TElementInfo);
    procedure IEParser1HR(Sender: TObject; Align: string; Color, Width,
      Size: OleVariant; NoShade: Boolean; Element: TElementInfo);
    procedure IEParser1Marquee(Sender: TObject; bgColor, Width,
      Height: OleVariant; Direction, Behavior: string; ScrollAmount,
      ScrollDelay, Loop, vSpace, hSpace: Integer; Element: TElementInfo);
    procedure IEParser1ParseComplete(Sender: TObject; Doc: IHTMLDocument2;
      All: IHTMLElementCollection);
    procedure IEParser1ParseDocument(Sender: TObject; const Res: HRESULT;
      stMessage: string);
    procedure IEParser1ParseError(Sender: TObject; const ErrorCode: Integer;
      const Url, stError: string);
    procedure IEParser1StateChange(Sender: TObject; const State: TParserState);
    procedure IEParser1DocInfo(Sender: TObject; const Text: string);
    procedure IEParser1Image(Sender: TObject; Source: String;
      ImgElement: IHTMLImgElement; Element: TElementInfo);
    procedure IEParser1Script(Sender: TObject; Source: String;
      ScriptElement: IHTMLScriptElement; Element: TElementInfo);
  private
    procedure UpdateControls;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.UpdateControls;
begin
  memAnchor.Clear;
  memBase.Clear;
  memBaseFont.Clear;
  memBody.Clear;
  memBR.Clear;
  memComment.Clear;
  memDiv.Clear;
  memFont.Clear;
  memForm.Clear;
  memHR.Clear;
  memImages.Clear;
  memMarquee.Clear;
  memMeta.Clear;
  memParse.Clear;
  memParseError.Clear;
  memProp.Clear;
  memScript.Clear;
end;

procedure TForm1.btnGoClick(Sender: TObject);
begin
  UpdateControls;
  IEParser1.Parse(edtAddress.Text);
end;

procedure TForm1.IEParser1BusyStateChange(Sender: TObject);
begin
 //
end;

//Events-------------------------------------------------------------------

procedure TForm1.IEParser1Anchor(Sender: TObject; hRef, Target, Rel, Rev, Urn,
  Methods, Name, Host, HostName, PathName, Port, Protocol, Search, Hash,
  AccessKey, ProtocolLong, MimeType, NameProp: string; Element: TElementInfo);
begin
  with memAnchor.Lines do
  begin
    if hRef <> '' then
      Add(#13#10 + 'hRef:' + hRef);
    if Rel <> '' then
      Add('Rel:' + Rel);
    if Rev <> '' then
      Add('Rev:' + Rev);
    if Urn <> '' then
      Add('Urn:' + Urn);
    if Target <> '' then
      Add('Target: ' + Target);
    if Methods <> '' then
      Add('Methods: ' + Methods);
    if Name <> '' then
      Add('Name: ' + Name);
    if Host <> '' then
      Add('Host: ' + Host);
    if HostName <> '' then
      Add('HostName: ' + HostName);
    if PathName <> '' then
      Add('PathName: ' + PathName);
    if Port <> '' then
      Add('Port: ' + Port);
    if Protocol <> '' then
      Add('Protocol: ' + Protocol);
    if Search <> '' then
      Add('Search: ' + Search);
    if AccessKey <> '' then
      Add('AccessKey: ' + AccessKey);
    if ProtocolLong <> '' then
      Add('ProtocolLong: ' + ProtocolLong);
    if MimeType <> '' then
      Add('MimeType: ' + MimeType);
    if NameProp <> '' then
      Add('NameProp: ' + NameProp);
  end;
end;


procedure TForm1.IEParser1Base(Sender: TObject; hRef, Target: string;
  Element: TElementInfo);
begin
  with memBase.Lines do
  begin
    if hRef <> '' then
      Add(#13#10 + 'hRef: ' + hRef);
    if Target <> '' then
      Add('Target: ' + Target);
    if Element.InnerHTML <> '' then
      Add('Element.InnerHTML:' + #13#10 + Element.InnerHTML);
  end;
end;

procedure TForm1.IEParser1BaseFont(Sender: TObject; Color: OleVariant;
  Face: string; Size: Integer; Element: TElementInfo);
begin
  with memBaseFont.Lines do
  begin
    if VarToStr(Color) <> '' then
      Add(#13#10 + 'Color: ' + VarToStr(Color));
    if Face <> '' then
      Add('Face: ' + Face);
    if IntToStr(Size) <> '' then
      Add('Size: ' + IntToStr(Size));
    if Element.InnerHTML <> '' then
      Add('Element.InnerHTML:' + #13#10 + Element.InnerHTML);
  end;
end;

procedure TForm1.IEParser1Body(Sender: TObject; Background,
  bgProperties: string; LeftMargin, TopMargin, RightMargin, BottomMargin,
  bgColor, Text, Link, vLink, aLink: OleVariant; NoWrap: Boolean;
  Element: TElementInfo);
begin
  with memBody.Lines do
  begin
    if Background <> '' then
      Add(#13#10 + 'Background:' + Background);
    if bgProperties <> '' then
      Add('bgProperties: ' + bgProperties);
    if VarToStr(bgColor) <> '' then
      Add('bgColor: ' + VarToStr(bgColor));
    if VarToStr(Text) <> '' then
      Add('Text: ' + VarToStr(Text));
    if VarToStr(Link) <> '' then
      Add('Link: ' + VarToStr(Link));
    if VarToStr(vLink) <> '' then
      Add('vLink: ' + VarToStr(vLink));
    if VarToStr(aLink) <> '' then
      Add('aLink: ' + VarToStr(aLink));
    if Element.InnerHTML <> '' then
      Add('Element.InnerHTML:' + #13#10 + Element.InnerHTML);
  end;
end;

procedure TForm1.IEParser1BR(Sender: TObject; Clear: string;
  Element: TElementInfo);
begin
  memBR.Lines.Add(#13#10 + 'Clear: ' + Clear + #13#10);
end;

procedure TForm1.IEParser1Comment(sender: TObject; Text: string;
  Element: TElementInfo);
begin
  memComment.Lines.Add(#13#10 + 'Text: ' + Text);
  if Element.InnerHTML <> '' then
    memComment.Lines.Add('Element.InnerHTML:' + #13#10 + Element.InnerHTML);
end;

procedure TForm1.IEParser1Div(Sender: TObject; Align: string; NoWrap: Boolean;
  Element: TElementInfo);
begin
  memDiv.Lines.Add(#13#10 + 'Align: ' + Align);
  if Element.InnerHTML <> '' then
    memDiv.Lines.Add('Element.InnerHTML:' + Element.InnerHTML);
end;

procedure TForm1.IEParser1DocInfo(Sender: TObject; const Text: string);
begin
  memProp.Lines.Add(Text);
end;

procedure TForm1.IEParser1Form(Sender: TObject; Action, Dir, Encoding, Method,
  Target, Name: string; Element: TElementInfo);
begin
  with memForm.Lines do
  begin
    if Action <> '' then
      Add(#13#10 + 'Action: ' + Action);
    if Dir <> '' then
      Add('Dir: ' + Dir);
    if Encoding <> '' then
      Add('Encoding: ' + Encoding);
    if Method <> '' then
      Add('Method: ' + Method);
    if Target <> '' then
      Add('Target: ' + Target);
    if Name <> '' then
      Add('Name: ' + Name);
    if Element.InnerHTML <> '' then
      Add('Element.InnerHTML:' + #13#10 + Element.InnerHTML);
  end;
end;

procedure TForm1.IEParser1HR(Sender: TObject; Align: string; Color, Width,
  Size: OleVariant; NoShade: Boolean; Element: TElementInfo);
begin
  with memHR.Lines do
  begin
    if Align <> '' then
      Add(#13#10 + 'Align: ' + Align);
    if VarToStr(Color) <> '' then
      Add('Color: ' + VarToStr(Color));
    if VarToStr(Size) <> '' then
      Add('Size: ' + VarToStr(Size));
    if Element.OuterHTML <> '' then
      Add('Element.OuterHTML:' + #13#10 + Element.OuterHTML);
  end;
end;

procedure TForm1.IEParser1Marquee(Sender: TObject; bgColor, Width,
  Height: OleVariant; Direction, Behavior: string; ScrollAmount, ScrollDelay,
  Loop, vSpace, hSpace: Integer; Element: TElementInfo);
begin
  with memMarquee.Lines do
  begin
    if VarToStr(bgColor) <> '' then
      Add(#13#10 + 'bgColor: ' + VarToStr(bgColor));
    if Direction <> '' then
      Add('Direction: ' + Direction);
    if Behavior <> '' then
      Add('Behavior: ' + Behavior);
    if Element.OuterHTML <> '' then
      Add('Element.OuterHTML:' + #13#10 + Element.OuterHTML);
  end;
end;

procedure TForm1.IEParser1Meta(Sender: TObject; HttpEquiv, Content, Name, URL,
  Charset: string; Element: TElementInfo);
begin
  with memMeta.Lines do
  begin
    if HttpEquiv <> '' then
      Add('HttpEquiv: ' + HttpEquiv);
    if Content <> '' then
      Add('Content: ' + Content);
    if Name <> '' then
      Add('Name: ' + Name);
    if URL <> '' then
      Add('Url: ' + URL);
    if Charset <> '' then
      Add('Charset: ' + Charset);
    if Element.OuterHTML <> '' then
      Add('Element.OuterHTML:' + #13#10 + Element.OuterHTML);
  end;
end;

procedure TForm1.IEParser1Script(Sender: TObject; Source: String;
  ScriptElement: IHTMLScriptElement; Element: TElementInfo);
begin
  with memScript.Lines, ScriptElement do
  begin
    if Element.Language <> '' then
      Add('Element.Language:' + Element.Language);
    if Source <> '' then
      Add(#13#10 + 'Source: ' + Source);
    if HtmlFor <> '' then
      Add('HtmlFor: ' + HtmlFor);
    if Event <> '' then
      Add('Event: ' + Event);
    if Text <> '' then
      Add('Text: ' + Text);
    if Element.OuterHTML <> '' then
      Add('Element.OuterHTML:' + #13#10 + Element.OuterHTML);
    if Element.InnerHTML <> '' then
      Add('Element.InnerHTML:' + #13#10 + Element.InnerHTML);
  end;
end;

procedure TForm1.IEParser1StateChange(Sender: TObject;
  const State: TParserState);
begin
   btnGo.Enabled := not IEParser1.Busy;
  if IEParser1.Busy then
    StatusBar1.Panels[0].Text := 'Parsing..'
  else
    StatusBar1.Panels[0].Text := 'Ready.'
end;

procedure TForm1.IEParser1ParseComplete(Sender: TObject; Doc: IHTMLDocument2;
  All: IHTMLElementCollection);
begin
 // StatusBar1.Panels[0].Text := stMessage;
//  memParse.Lines.Add(stMessage + #13#10);
end;

procedure TForm1.IEParser1ParseDocument(Sender: TObject; const Res: HRESULT;
  stMessage: string);
begin
 StatusBar1.Panels[1].Text := stMessage;
 memParse.Lines.Add(stMessage + #13#10);
end;

procedure TForm1.IEParser1ParseError(Sender: TObject; const ErrorCode: Integer;
  const Url, stError: string);
begin
   memParseError.Lines.Add(Url+ #13#10+ stError);
end;

procedure TForm1.IEParser1Font(Sender: TObject; Color, Size: OleVariant;
  Face: string; Element: TElementInfo);
begin
  with memFont.Lines do
  begin
    if VarToStr(Color) <> '' then
      Add(#13#10 + 'Color: ' + VarToStr(Color));
    if VarToStr(Size) <> '' then
      Add('Size: ' + VarToStr(Size));
    if Face <> '' then
      Add('Face: ' + Face);
    if Element.InnerHTML <> '' then
      Add('Element.InnerHTML:' + Element.InnerHTML);
  end;
end;

procedure TForm1.IEParser1Image(Sender: TObject; Source: String;
  ImgElement: IHTMLImgElement; Element: TElementInfo);
begin
  with memImages.Lines, ImgElement do
  begin
    if Source <> '' then
      Add(#13#10 + 'Source:' + Source);
    if LowSrc <> '' then
      Add('LowSrc:' + LowSrc);
    if Vrml <> '' then
      Add('Vrml:' + Vrml);
    if DynSrc <> '' then
      Add('DynSrc:' + DynSrc);
    if Alt <> '' then
      Add('Alt: ' + Alt);
    if Align <> '' then
      Add('Align: ' + Align);
    if Name <> '' then
      Add('Name: ' + Name);
    if UseMap <> '' then
      Add('UseMap: ' + UseMap);
    if IntToStr(hSpace) <> '' then
      Add('hSpace: ' + IntToStr(hSpace));
    if IntToStr(Width) <> '' then
      Add('Width: ' + IntToStr(Width));
    if IntToStr(Height) <> '' then
      Add('Height: ' + IntToStr(Height));
  end;

  with memImages.Lines, ImgElement as IHTMLImgElement2 do
  begin
    if longDesc <> '' then
      Add('longDesc: ' + longDesc);
  end;

end;



end.

