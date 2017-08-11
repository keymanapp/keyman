//*************************************************************
//                    EmbeddedWB - MSHTML Events              *
//                                                            *
//                            by                              *
//                     Thomas Stutz (smot)                    *
//                     smot777@yahoo.com                      *
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

unit Unit1;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OleCtrls {$IFDEF Enable_EwbMSHTMLEvents}, MSHTMLEvents{$ENDIF}, SHDocVw_EWB, EwbCore, EmbeddedWB,
  ExtCtrls;

type
  TForm1 = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    Panel1: TPanel;
    Label1: TLabel;
    Memo1: TMemo;
{$IFDEF Enable_EwbMSHTMLEvents}
    procedure EmbeddedWB1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure EmbeddedWB1DownloadComplete(Sender: TObject);
  private
    { Private declarations }
     function DocOnClick(Sender: TObject): WordBool;
     function DocSelectStart(Sender: TObject): WordBool;
{$ENDIF}
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
 MSHTML;

{$IFDEF Enable_EwbMSHTMLEvents}

function TForm1.DocOnClick(Sender: TObject): WordBool;
var
  SrcElement: IHTMLElement;
  Doc: IHTMLDocument2;
begin
  Result := False;
  Doc :=  TMSHTMLHTMLDocumentEvents(Sender).Source as IHTMLDocument2;
  if Doc <> nil then
  begin
    SrcElement := Doc.parentWindow.event.srcElement;
    // now you can do what you want with srcelement, like:
    Memo1.Text := SrcElement.innerhtml;
    Result := True;
  end;
end;

function TForm1.DocSelectStart(Sender: TObject): WordBool;
var
  Doc: IHTMLDocument2;
begin
  Result := True;
  Doc :=  TMSHTMLHTMLDocumentEvents(Sender).Source as IHTMLDocument2;
  if Doc <> nil then
  begin
    Result := False;
    Caption := 'Selecting disabled!';
  end;
end;

procedure TForm1.EmbeddedWB1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  FSinkComponent: TMSHTMLHTMLDocumentEvents;
begin
  FSinkComponent := TEmbeddedWB(ASender).SinkComponent;  // $DEFINE Enable_EwbMSHTMLEvents must be enabled in EWB.inc !
  if Assigned(FSinkComponent) then
    FSinkComponent.Disconnect;
end;

procedure TForm1.EmbeddedWB1DownloadComplete(Sender: TObject);
var
   FSinkComponent: TMSHTMLHTMLDocumentEvents;
   EWB : TEmbeddedWB;
   Document: IHTMLDocument2;
begin
   EWB := Sender as TEmbeddedWB;
   if Assigned(EWB.Document) then
   begin
     if Succeeded(EWB.Document.QueryInterface(IID_IHTMLDocument2, Document)) then
     begin
       FSinkComponent := EWB.SinkComponent;
       FSinkComponent.onselectstart := DocSelectStart;
       FSinkComponent.OnClick := DocOnClick;
       FSinkComponent.Connect(Document);
     end;
   end;
end;

{$ENDIF}

end.
