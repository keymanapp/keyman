//***********************************************************
//                      URL  Tools  demo                    *
//                (Uniform Resource identifier)             *
//                                                          *
//               For Delphi 5 - 2009                        *
//                     Freeware demo                        *
// By:  Eran Bodankin (bsalsa)   bsalsa@bsalsa.com          *
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
4. Please consider donation in our web site!
{*******************************************************************************}

unit frmMain;

interface

uses
  EwbUrl, SysUtils, StdCtrls, Forms, IEAddress, ExtCtrls,
  IEConst, WinInet, Classes, Controls, Windows, ComCtrls, LinkLabel;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    IEAddress1: TIEAddress;
    PageControl2: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel5: TPanel;
    Label7: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label4: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edtURL: TEdit;
    edtHost: TEdit;
    edtPort: TEdit;
    edtDocument: TEdit;
    edtUser: TEdit;
    edtPath: TEdit;
    edtProtocol: TEdit;
    edtBookmark: TEdit;
    edtPass: TEdit;
    edtParameters: TEdit;
    edtExtraInfo: TEdit;
    Panel2: TPanel;
    Label12: TLabel;
    Button2: TButton;
    edtComp1: TEdit;
    edtResult: TEdit;
    Panel4: TPanel;
    btnCan: TButton;
    edtCan: TEdit;
    edtCano: TEdit;
    cbCFlag: TCheckBox;
    Panel3: TPanel;
    Label10: TLabel;
    Label11: TLabel;
    btnCombine: TButton;
    edtCombine: TEdit;
    edtReletive: TEdit;
    edtBase: TEdit;
    cbFlag: TCheckBox;
    TabSheet3: TTabSheet;
    Panel7: TPanel;
    btnValid: TButton;
    btnSize: TButton;
    btnProtocol: TButton;
    Panel6: TPanel;
    btnBuild: TButton;
    btnCreate: TButton;
    btnAnalize: TButton;
    btnCrack: TButton;
    cbCrack: TCheckBox;
    EdtRes: TEdit;
    btnCharSet: TButton;
    btnEntity: TButton;
    btnServer: TButton;
    btnStatusCode: TButton;
    btnModified: TButton;
    btnUrlDate: TButton;
    btnUrlType: TButton;
    Memo1: TMemo;
    btnDetails: TButton;
    Label15: TLabel;
    btnQueryAll: TButton;
    Label16: TLabel;
    LinkLabel1: TLinkLabel;
    TabSheet4: TTabSheet;
    Memo2: TMemo;
    Panel8: TPanel;
    btnRead: TButton;
    btnCache: TButton;
    edtCache: TEdit;
    procedure btnCacheClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnQueryAllClick(Sender: TObject);
    procedure btnDetailsClick(Sender: TObject);
    procedure btnUrlTypeClick(Sender: TObject);
    procedure btnModifiedClick(Sender: TObject);
    procedure btnUrlDateClick(Sender: TObject);
    procedure btnStatusCodeClick(Sender: TObject);
    procedure btnServerClick(Sender: TObject);
    procedure btnEntityClick(Sender: TObject);
    procedure btnCharSetClick(Sender: TObject);
    procedure btnLangClick(Sender: TObject);
    procedure btnProtocolClick(Sender: TObject);
    procedure btnSizeClick(Sender: TObject);
    procedure btnValidClick(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnCanClick(Sender: TObject);
    procedure btnCombineClick(Sender: TObject);
    procedure btnCrackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnAnalizeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure InsertData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  UP: TUrl;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  UP := TUrl.Create(IEAddress1.Text);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  UP.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  IEAddress1.Text := 'http://username:password@hostname:port/path/document.html#bookmark';
end;

procedure TForm1.InsertData;
begin
  edtUrl.Text := UP.Url;
  edtDocument.Text := UP.Document;
  edtProtocol.Text := UP.Protocol;
  edtPort.Text := IntToStr(UP.Port);
  edtPath.Text := UP.UrlPath;
  edtHost.Text := UP.HostName;
  edtExtraInfo.Text := UP.ExtraInfo;
  edtPass.Text := UP.Password;
  edtUser.Text := UP.UserName;
  edtParameters.Text := UP.Parameters;
  edtBookmark.Text := UP.Bookmark;
end;

procedure TForm1.btnAnalizeClick(Sender: TObject);
begin
  UP.QueryUrl(IEAddress1.Text);
  InsertData;
  btnCreate.Enabled := True;
  btnBuild.Enabled := True;
end;

procedure TForm1.btnCrackClick(Sender: TObject);
begin
  if cbCrack.Checked then
    edtUrl.Text := UP.CrackUrl(IEAddress1.Text, ICU_DECODE)
  else
    edtUrl.Text := UP.CrackUrl(IEAddress1.Text, ICU_ESCAPE);

  InsertData;
  btnCreate.Enabled := True;
  btnBuild.Enabled := True;
end;

procedure TForm1.btnCreateClick(Sender: TObject);
begin
  edtUrl.Text := Up.CreateUrl(ICU_USERNAME);
end;

procedure TForm1.btnCharSetClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlCharset(IEAddress1.Text);
end;

procedure TForm1.btnLangClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlServer(IEAddress1.Text);
end;

procedure TForm1.btnModifiedClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlLastModified(IEAddress1.Text);
end;

procedure TForm1.btnValidClick(Sender: TObject);
begin
  if Up.IsUrlValid(IEAddress1.Text) then
    edtRes.Text := 'A valid url'
  else
    edtRes.Text := 'Not a valid url!'
end;

procedure TForm1.btnSizeClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlSize(IEAddress1.Text);
end;

procedure TForm1.btnStatusCodeClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlStatusCode(IEAddress1.Text);
end;

procedure TForm1.btnServerClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlServer(IEAddress1.Text);
end;

procedure TForm1.btnUrlDateClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlDate(IEAddress1.Text);
end;

procedure TForm1.btnUrlTypeClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlType(IEAddress1.Text);
end;

procedure TForm1.btnEntityClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlEntityTag(IEAddress1.Text);
end;

procedure TForm1.btnProtocolClick(Sender: TObject);
begin
  edtRes.Text := Up.GetUrlProtocolVersion(IEAddress1.Text);
end;

procedure TForm1.btnQueryAllClick(Sender: TObject);
var
  i: Integer;
  st: string;
begin
  for i := 1 to 75 do
  begin
    st := (UP.QueryInfo(IEAddress1.Text, i));
    if st <> '' then
    begin
      Memo1.Lines.Add('Flag' + '# ' + IntToStr(i) + ': ');
      Memo1.Lines.Add(st);
    end;
  end;
end;

procedure TForm1.btnReadClick(Sender: TObject);
begin
  Memo2.Lines.Add(UP.ReadFile(IEAddress1.Text, 50));
end;

procedure TForm1.btnDetailsClick(Sender: TObject);
var
  st: string;
begin
  st := UP.GetUrlServerDetails(IEAddress1.Text);
  if Trim(st) <> '' then
    Memo1.Lines.Add(st);
end;

procedure TForm1.btnCacheClick(Sender: TObject);
begin
  if Up.isUrlCached(IEAddress1.Text) then
    edtCache.Text := 'Url is Cached.'
  else
    edtCache.Text := 'Url is not Cached.'
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Up.CompareUrl(IEAddress1.Text, edtComp1.Text) = S_OK then
    edtResult.Text := 'Equal'
  else
    edtResult.Text := 'Not Equal'
end;

procedure TForm1.btnBuildClick(Sender: TObject);
begin
  edtUrl.Text := Up.BuildUrl;
end;

procedure TForm1.btnCanClick(Sender: TObject);
begin
  if cbCFlag.Checked then
    edtCano.Text := UP.CanonicalizeUrl(edtCan.Text, ICU_NO_ENCODE)
  else
    edtCano.Text := UP.CanonicalizeUrl(edtCan.Text, ICU_DECODE);
end;

procedure TForm1.btnCombineClick(Sender: TObject);
begin
  if cbFlag.Checked then
    edtCombine.Text := UP.CombineUrl(edtBase.Text, edtReletive.Text, ICU_DECODE)
  else
    edtCombine.Text := UP.CombineUrl(edtBase.Text, edtReletive.Text, ICU_NO_ENCODE);
end;

end.

