//*************************************************************
//                          GuidCreator                       *
//                                                            *
//                            by                              *
//                     bsalsa - Eran Bodankin                 *
//       Documentation and updated versions:                  *
//                                                            *
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

You may use/ change/ modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit  for the benefit
   of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit utGuidCreator;

interface

uses
  Messages, SysUtils, IEGuid, Classes, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Browse4Folder, IEAddress, OleCtrls,
  SHDocVw_EWB, EmbeddedWB, ComCtrls, LinkLabel, EwbCore;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    btnIEGuid: TButton;
    btnHPath: TButton;
    btnCreateIEGuid: TButton;
    btnCreateIEList: TButton;
    Label1: TLabel;
    edtHPath: TEdit;
    edtIEGuid: TEdit;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    Label3: TLabel;
    btnIEList: TButton;
    edtIEList: TEdit;
    cbOpenNotepad: TCheckBox;
    GroupBox1: TGroupBox;
    btnGetInterfaces: TButton;
    btnConnectionP: TButton;
    btnGetServices: TButton;
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Memo1: TMemo;
    TabSheet2: TTabSheet;
    EmbeddedWB1: TEmbeddedWB;
    Panel2: TPanel;
    IEAddress1: TIEAddress;
    btnGo: TButton;
    LinkLabel1: TLinkLabel;
    LinkLabel3: TLinkLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnGetServicesClick(Sender: TObject);
    procedure btnConnectionPClick(Sender: TObject);
    procedure btnGetInterfacesClick(Sender: TObject);
    procedure btnIEListClick(Sender: TObject);
    procedure btnCreateIEListClick(Sender: TObject);
    procedure btnCreateIEGuidClick(Sender: TObject);
    procedure btnHPathClick(Sender: TObject);
    procedure btnIEGuidClick(Sender: TObject);
    procedure EmbeddedWB1QueryService(Sender: TObject; const rsid,
      iid: TGUID; var Obj: IUnknown);
  end;

var
  Form1: TForm1;
  WBGuids: TIEGuid;

implementation

uses
  Windows, ShellAPI, ActiveX, MSHTML_EWB, ShDocVw;

{$R *.dfm}

procedure TForm1.btnCreateIEGuidClick(Sender: TObject);
begin
  if (edtIEGuid.Text = '') or (edtHPath.Text = '') then
  begin
    MessageDlg('Please enter a valid path and file name!', mtError, [mbOK], 0);
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  try
    Memo1.Clear;
    Memo1.Lines.Add('Please Wait..');
    CreateIEGuid(edtHPath.Text, edtIEGuid.Text);
    Memo1.Lines.LoadFromFile(edtIEGuid.Text);
    if cbOpenNotepad.Checked then
      ShellExecute(Handle, 'open', 'notepad.exe', PChar(edtIEGuid.Text), nil, SW_SHOWNORMAL);
    GroupBox1.Enabled := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnCreateIEListClick(Sender: TObject);
begin
  if (edtIEGuid.Text = '') or (edtIEList.Text = '') then
  begin
    MessageDlg('Please enter a valid path and file name!', mtError, [mbOK], 0);
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  try
    Memo1.Clear;
    Memo1.Lines.Add('Please Wait..');
    CreateIEList(edtIEGuid.Text, edtIEList.Text);
    Memo1.Lines.LoadFromFile(edtIEList.Text);
    if cbOpenNotepad.Checked then
      ShellExecute(Handle, 'open', 'notepad.exe', PChar(edtIEGuid.Text), nil, SW_SHOWNORMAL);
    GroupBox1.Enabled := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.btnIEGuidClick(Sender: TObject);
var
  sd: TSaveDialog;
begin
  Sd := TSaveDialog.Create(Self);
  with Sd do
  begin
    FileName := 'GuidList.txt';
    InitialDir := ExtractFilePath(Forms.Application.ExeName);
    Filter := 'Text files|*.txt';
    HelpContext := 0;
    Options := Options + [ofShowHelp, ofEnableSizing];
  end;
  if Sd.Execute then
    edtIEGuid.Text := Sd.FileName;
  Sd.Free;
end;

procedure TForm1.btnIEListClick(Sender: TObject);
var
  sd: TSaveDialog;
begin
  Sd := TSaveDialog.Create(Self);
  with Sd do
  begin
    FileName := 'IEGuidList.txt';
    InitialDir := ExtractFilePath(Forms.Application.ExeName);
    Filter := 'Text files|*.txt|Word files|*.doc';
    HelpContext := 0;
    Options := Options + [ofShowHelp, ofEnableSizing];
  end;
  if Sd.Execute then
    edtIEList.Text := Sd.FileName;
  Sd.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'notepad.exe', PChar(edtIEGuid.Text), nil, SW_SHOWNORMAL);
end;

procedure TForm1.EmbeddedWB1QueryService(Sender: TObject; const rsid,
  iid: TGUID; var Obj: IUnknown);
begin
  Memo1.Lines.Add(WBGuids.NameFromGuid(rsid) + ' - ' + WBGuids.NameFromGuid(iid));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not FileExists('IEGuidList.txt') then
    Memo1.Lines.SaveToFile(ExtractFilePath(Forms.Application.ExeName) + 'IEGuidList.txt'); //dummy To prevent a crash on load
  WBGuids := TIEGuid.Create(ExtractFilePath(Forms.Application.ExeName) + 'IEGuidList.txt');
  PageControl1.ActivePageIndex := 0;
  EmbeddedWB1.AssignEmptyDocument;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  WBGuids.Free;
end;

procedure TForm1.btnGetServicesClick(Sender: TObject);
var
  wb: TWebbrowser;
  MyGuids: TIEGuid;
begin
  Memo1.Clear;
  WB := TWebbrowser.Create(Self);
  try
    MyGuids := TIEGuid.Create(edtIEList.Text);
    try
      MyGuids.GetServices(wb.Application, '', Memo1.lines);
    finally
      MyGuids.Free;
    end;
  finally
    WB.Free;
  end;
end;

procedure TForm1.btnGoClick(Sender: TObject);
begin
  EmbeddedWB1.Go(IEAddress1.Text);
end;

procedure TForm1.btnConnectionPClick(Sender: TObject);
var
  wb: TWebbrowser;
  MyGuids: TIEGuid;
begin
  Memo1.Clear;
  MyGuids := TIEGuid.Create(edtIEList.Text);
  try
    WB := TWebbrowser.Create(Self);
    try
      MyGuids.GetConnectionPoints(wb.Document, Memo1.lines, TRUE);
    finally
      WB.Free;
    end;
  finally
    MyGuids.Free;
  end;
end;

procedure TForm1.btnGetInterfacesClick(Sender: TObject);
var
  wb: TWebbrowser;
  MyGuids: TIEGuid;
begin
  Memo1.Clear;
  MyGuids := TIEGuid.Create(edtIEList.Text);
  try
    WB := TWebbrowser.Create(Self);
    try
      MyGuids.GetInterfaces(WB.Application, Memo1.lines);
    finally
      WB.Free;
    end;
  finally
    MyGuids.Free;
  end;
end;

procedure TForm1.btnHPathClick(Sender: TObject);
var
  b4f: TBrowse4Folder;
begin
  b4f := TBrowse4Folder.Create(Self);
  try
    b4F.InitialDir := ExtractFilePath(Forms.Application.ExeName);
    edtHPath.Text := b4f.Execute2;
  finally
    b4f.Free;
  end;
end;


end.

