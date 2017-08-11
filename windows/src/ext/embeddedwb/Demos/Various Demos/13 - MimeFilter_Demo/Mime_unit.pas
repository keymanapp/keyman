 {*****************************************************************}
{                                                                 }
{            MimeFilter interface Demo                            }
{                                                                 }
{            By Eran Bodankin - bsalsa                            }
{            Converted 6 May 2007 bsalsa@bsalsa.com               }
{            Last modified 6 May 2007 bsalsa@bsalsa.com           }
{            Version 2                                            }
{                                                                 }
{       Documentation and updated versions:                       }
{                                                                 }
{               http://www.bsalsa.com                             }
{*****************************************************************}
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

unit Mime_unit;

interface

uses
  ComServ, Classes, Forms, SysUtils, ActiveX, UrlMon, StdCtrls, OleCtrls,
  SHDocVw_EWB, EwbCore, EmbeddedWB, ExtCtrls, Controls, IEAddress;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    btnInternet: TButton;
    btnLocal: TButton;
    EmbeddedWB1: TEmbeddedWB;
    edtIn: TEdit;
    edtOut: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    cbFilter: TCheckBox;
    IEAddress1: TIEAddress;
    Panel2: TPanel;
    Memo1: TMemo;
    procedure EmbeddedWB1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnLocalClick(Sender: TObject);
    procedure btnInternetClick(Sender: TObject);
  private
    bConnected: Boolean;
    procedure RegisterFilter;
    procedure UnregisterFilter;
    procedure SetFilter;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses MimeFilter;

var
  Factory: IClassFactory;
  InternetSession: IInternetSession;

procedure TForm1.UnregisterFilter;
begin
  if bConnected then
  begin
    InternetSession.UnregisterMimeFilter(Factory, 'text/html');
    if InternetSession <> nil then
    begin
      InternetSession._Release;
      InternetSession := nil;
    end;
    if Factory <> nil then
    begin
      Factory._Release;
      Factory := nil;
    end;
    bConnected := False;
  end;
end;

procedure TForm1.RegisterFilter;
begin
  if Succeeded(CoInternetGetSession(0, InternetSession, 0))
    and Succeeded(CoGetClassObject(Class_StrMimeFilter, CLSCTX_SERVER, nil, IClassFactory, Factory))
    and Succeeded(InternetSession.RegisterMimeFilter(Factory, Class_StrMimeFilter, 'text/html')) then
    bConnected := True;
end;

procedure TForm1.SetFilter;
begin
  if cbFilter.Checked then
  begin
    MimeFilter.StrIn := edtIn.Text;
    MimeFilter.StrOut := edtOut.Text;
  end else
  begin
    MimeFilter.StrIn := '';
    MimeFilter.StrOut := '';
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  EmbeddedWB1.Go('http://www.bsalsa.com/forum/');
  RegisterFilter;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  UnRegisterFilter;
end;

procedure TForm1.btnLocalClick(Sender: TObject);
begin
  SetFilter;
  EmbeddedWB1.Go('file://' + ExtractFilePath(Application.ExeName) + 'demo.htm');
end;

procedure TForm1.btnInternetClick(Sender: TObject);
begin
  SetFilter;
  EmbeddedWB1.Go(IEAddress1.Text);
end;

procedure TForm1.EmbeddedWB1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  IEAddress1.Text := EmbeddedWB1.LocationURL;
end;

end.

