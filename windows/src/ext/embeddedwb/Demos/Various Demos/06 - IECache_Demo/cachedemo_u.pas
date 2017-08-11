//***********************************************************
//               IECache Demo ver 1.01 (2006)               *
//                                                          *
//               For Delphi 5 - 2009                        *
//                     Freeware Demo                        *
//                            by                            *
//                     Per Lindsø Larsen                    *
//                                                          *
//  Contributions:                                          *
//  Eran Bodankin -bsalsa(bsalsa@bsalsa.com)                *
//  Updated versions:                                       *
//               http://www.bsalsa.com                      *
//*************************************************************************
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

You may use, change or modify the component under 3 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit cachedemo_u;

interface

uses
  wininet, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IECache, StdCtrls, ExtCtrls, OleCtrls, SHDocVw_EWB, EmbeddedWB, EwbCore;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    RadioGroup1: TRadioGroup;
    Button2: TButton;
    Button1: TButton;
    IECache1: TIECache;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Button3: TButton;
    OpenDlg: TOpenDialog;
    CheckBox1: TCheckBox;
    DeleteEntryBtn: TButton;
    Panel1: TPanel;
    EmbeddedWB1: TEmbeddedWB;
    edtFileName: TEdit;
    procedure edtFileNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure IECache1Entry(Sender: TObject; var Cancel: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure DeleteEntryBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.IECache1Entry(Sender: TObject; var Cancel: Boolean);
begin
  listbox1.Items.Add(IECache1.EntryInfo.SourceUrlName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IECache1.SearchPattern := spAll;
  IECache1.RetrieveEntries(0);
  EmbeddedWB1.AssignEmptyDocument;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  IECache1.SearchPattern := spAll;
//e.g.: set SearchPattern:=spCookies if you only want to delete cookies
  IECache1.ClearAllEntries;
  Listbox1.Items.Clear;
  IECache1.SearchPattern := spAll;
  IECache1.RetrieveEntries(0);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  Listbox1.Items.BeginUpdate;
  try
  Listbox1.Items.Clear;
  with IECache1 do
  begin
    case RadioGroup1.ItemIndex of
      0: SearchPattern := spAll;
      1: SearchPattern := spCookies;
      2: SearchPattern := spHistory;
      3: SearchPattern := spUrl;
    end;
    RetrieveEntries(0);
  end;
  finally
    Listbox1.Items.EndUpdate;
  end;
end;

function DTString(DT: TDatetime): string;
begin
  if DT < 0 then
    Result := ''
  else
    Result := DateTimeToStr(DT);
end;

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  EmbeddedWB1.Wait;
  if Listbox1.Items.Count > 1 then
    IECache1.GetEntryInfo(ListBox1.Items.Strings[ListBox1.ItemIndex]);
  with IECache1.EntryInfo do
  begin
    if ((pos('.htm', Localfilename) > 0) or (pos('.gif', Localfilename) > 0) or (pos('.jpg', Localfilename) > 0))
      and Checkbox1.checked then
      EmbeddedWB1.Go(LocalFileName);
    Label1.Caption := 'Hitrate: ' + InttoStr(HitRate);
    Label2.Caption := 'FileSize: ' + InttoStr(FSize);
    Label3.Caption := 'Last access: ' + DTString(LastAccessTime);
    Label4.Caption := 'Last modified: ' + DTString(LastModifiedTime);
    Label5.Caption := 'Expire: ' + DTString(ExpireTime);
    edtFileName.Text := LocalFileName;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Listbox1.SetFocus;
  listbox1.ItemIndex := 1;
  Listbox1Click(Sender);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with OpenDlg do
  begin
    filter := 'Internet files|*.htm;*.html;*.gif;*.jpg';
    if Execute then
      if IECache1.CopyFileToCache('file:///' + FileName, FileName,
        NORMAL_CACHE_ENTRY, StrToDateTime('01-01-02 00:00:00')) = S_OK
        then
      begin
        Radiogroup1.ItemIndex := 0;
        Radiogroup1Click(Sender);
        listbox1.ItemIndex := Listbox1.Items.IndexOf('file:///' + FileName);
      end;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if not Checkbox1.Checked then
    EmbeddedWB1.GoAboutBlank;
  listbox1.Setfocus;
end;

procedure TForm1.DeleteEntryBtnClick(Sender: TObject);
begin
  EmbeddedWB1.GoAboutBlank;
  IECache1.DeleteEntry(Listbox1.Items[Listbox1.Itemindex]);
  RadioGroup1Click(Sender);
  Listbox1.Setfocus;
end;

procedure TForm1.edtFileNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Exit;
end;

end.

