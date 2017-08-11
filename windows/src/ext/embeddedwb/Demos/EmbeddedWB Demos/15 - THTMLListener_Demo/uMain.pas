unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, EwbBehaviorsComp, EwbEventsComp, EwbEvents, OleCtrls, SHDocVw_EWB, MSHTML_EWB, EwbCore,
  EmbeddedWB, StdCtrls, ComObj, ActiveX, ExtCtrls;



type
  TForm1 = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    Panel1: TPanel;
    btnEnableDisable: TButton;
    lblClickedOnElement: TLabel;
    lblElementUndertheMouse: TLabel;
    HtmlListener1: THtmlListener;
    procedure HtmlListener1HandlersOnClickHandle(Sender: TObject;
      Event: IHTMLEventObj);
    procedure FormCreate(Sender: TObject);
    procedure HtmlListener1HandlersOnMouseMoveHandle(Sender: TObject;
      Event: IHTMLEventObj);
    procedure EmbeddedWB1DocumentComplete(ASender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
    procedure btnEnableDisableClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EmbeddedWB1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
  private
    { Private declarations }
    HubLinksList: TInterfaceList;
    FEventsEnabled: Boolean;
    HubLink: IHubLink;
    procedure DisconnectHtmlListeners;
    procedure ConnectHtmlListeners;
    procedure ConnectHtmlListener(ASender: TObject; Doc: IHTMLDocument2);
  public
    { Public declarations }
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FEventsEnabled := True;
  HubLinksList := TInterfaceList.Create;
  EmbeddedWB1.Navigate('www.google.com');
end;

procedure TForm1.HtmlListener1HandlersOnClickHandle(Sender: TObject;
  Event: IHTMLEventObj);
begin
  lblClickedOnElement.Caption := Format('Clicked on Element with Tag: [%s]', [Event.srcElement.tagName])
end;

procedure TForm1.HtmlListener1HandlersOnMouseMoveHandle(Sender: TObject;
  Event: IHTMLEventObj);
begin
  lblElementUndertheMouse.Caption := Format('Element under the Mouse [%s]', [Event.srcElement.tagName])
end;

procedure TForm1.DisconnectHtmlListeners;
var
  i: Integer;
begin
  for i := HubLinksList.Count - 1 downto 0 do
  begin
    (HubLinksList[i] as IHubLink).Disconnect;
    HubLinksList.Remove(HubLinksList[i]);
  end;
end;

procedure TForm1.ConnectHtmlListener(ASender: TObject; Doc: IHTMLDocument2);
var
  CPC: IConnectionPointContainer;
begin
  if Assigned(Doc) then
  begin
    Doc.QueryInterface(IConnectionPointContainer, CPC);
    if Assigned(CPC) then
    begin
      HubLink := HtmlListener1.Connect2(CPC);
      HubLinksList.Add(HubLink);
    end;
  end;
end;

procedure TForm1.ConnectHtmlListeners;
var
  i: Integer;
  WB: IWebbrowser2;
  HTMLDoc: IHTMLDocument2;
begin
  for i := 0 to EmbeddedWB1.FrameCount - 1 do
  begin
    WB := EmbeddedWB1.GetFrame(i);
    if Assigned(WB) then
    begin
      if Succeeded(WB.Document.QueryInterface(IHTMLDocument2, HTMLDoc)) then
        ConnectHtmlListener(nil, HTMLDoc);
    end;
  end;
end;

procedure TForm1.EmbeddedWB1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  with ASender as TEmbeddedWB do
    if pDisp = DefaultInterface then
    begin
      DisconnectHtmlListeners;
    end;
end;

procedure TForm1.EmbeddedWB1DocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
var
  WB: IWebbrowser;
  HTMLDoc: IHTMLDocument2;
begin
  if FEventsEnabled then
  begin
    WB := (pDisp as IWebBrowser);
    if Assigned(WB) then
      if Succeeded(WB.Document.QueryInterface(IHTMLDocument2, HTMLDoc)) then
        ConnectHtmlListener(ASender, HTMLDoc);
  end;
end;

procedure TForm1.btnEnableDisableClick(Sender: TObject);
begin
  if FEventsEnabled then
  begin
    DisconnectHtmlListeners;
    btnEnableDisable.Caption := 'Enable Events';
    FEventsEnabled := False;
  end else
  begin
    ConnectHtmlListeners;
    btnEnableDisable.Caption := 'Disable Events';
    FEventsEnabled := True;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DisconnectHtmlListeners;
  HubLinksList.Free;
end;


end.

