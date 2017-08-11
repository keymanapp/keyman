{*******************************************************}
{                File Associate demo                    }
{                                                       }
{               For Delphi 5 - 2009                     *
{           by bsalsa  - bsalsa@bsalsa.com              }
{            Please note our uses terms                 }
{                       Enjoy!                          }
{   UPDATES:                                            }
{               http://www.bsalsa.com                   }
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
3. Mail me (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please consider donation in our web site!
{*******************************************************************************}

unit frmMain;

interface

uses
  Classes, Graphics, Windows, Controls, Forms, SysUtils, StdCtrls, ComCtrls,
  FileExtAssociate, ExtCtrls;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    FileExtAssociate1: TFileExtAssociate;
    Panel2: TPanel;
    bttGetIcon: TButton;
    edtIcon: TEdit;
    Image1: TImage;
    Panel3: TPanel;
    btnExecute: TButton;
    btnRemove: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edtExt: TEdit;
    edtRemove: TEdit;
    Label5: TLabel;
    btnCheck: TButton;
    edtCheck: TEdit;
    Label3: TLabel;
    edtPath: TEdit;
    Panel1: TPanel;
    edtPath2: TEdit;
    btnFileType: TButton;
    Image2: TImage;
    Label4: TLabel;
    Label7: TLabel;
    lbNum: TLabel;
    MemoFileInfo: TMemo;
    procedure FormShow(Sender: TObject);
    procedure btnFileTypeClick(Sender: TObject);
    procedure bttGetIconClick(Sender: TObject);
    procedure FileExtAssociate1BusyStateChange(Sender: TObject);
    procedure FileExtAssociate1Complete(Extension, Status: string;
      HResult: HRESULT);
    procedure FileExtAssociate1SuccessText(Text: string);
    procedure FileExtAssociate1ErrorText(Text: string);
    procedure btnCheckClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCheckClick(Sender: TObject);
var
  st: string;
begin
  st := FileExtAssociate1.GetExeByExtension(EdtCheck.Text);
  if st <> '' then
    EdtPath.Text := st
  else
    Statusbar1.Panels[0].Text := 'No association found.';
end;

procedure TForm1.btnExecuteClick(Sender: TObject);
begin
  FileExtAssociate1.Execute;
end;

procedure TForm1.btnRemoveClick(Sender: TObject);
begin
  FileExtAssociate1.Remove;
end;

procedure TForm1.bttGetIconClick(Sender: TObject);
begin
  Image1.Picture.Icon.Handle :=
    FileExtAssociate1.GetIconByExtension(edtIcon.Text, False);
end;

procedure TForm1.btnFileTypeClick(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('File Type: ' + FileExtAssociate1.GetFileTypeName(edtPath2.Text));
    sl.Add('Display Name: ' + FileExtAssociate1.GetFileDisplayName(edtPath2.Text));
    sl.Add('File Size: ' + FileExtAssociate1.GetFileSize(edtPath2.Text));
    sl.Add('File Creation Date: ' + DateTimeToStr(FileExtAssociate1.GetFileDateTime(edtPath2.Text)));
    Image2.Picture.Icon.Handle := FileExtAssociate1.GetFileIcon(edtPath2.Text);
    lbNum.Caption := IntToStr(FileExtAssociate1.GetFileImage(edtPath2.Text));
    MemoFileInfo.Lines.Clear;
    MemoFileInfo.Lines.AddStrings(sl);
  finally
    sl.Free;
  end;
end;

procedure TForm1.FileExtAssociate1BusyStateChange(Sender: TObject);
begin
  btnExecute.Enabled := not FileExtAssociate1.Busy;
end;

procedure TForm1.FileExtAssociate1Complete(Extension, Status: string;
  HResult: HRESULT);
begin
  if (HResult = S_OK) and (Pos('success', Status) <> 0) then
  begin
    edtRemove.Text := Extension;
    edtCheck.Text := Extension;
    edtIcon.Text := Extension;
    EdtExt.Text := '';
  end;
  if (HResult = S_OK) and (Pos('Un', Status) <> 0) then
  begin
    edtRemove.Text := '';
    edtCheck.Text := Extension;
    EdtExt.Text := Extension;
  end;
end;

procedure TForm1.FileExtAssociate1ErrorText(Text: string);
begin
  Statusbar1.Panels[1].Text := 'Errors: ' + Text;
end;

procedure TForm1.FileExtAssociate1SuccessText(Text: string);
begin
  Statusbar1.Panels[0].Text := Text;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  edtPath2.Text := ExtractFilePath(Application.ExeName);
end;

end.

