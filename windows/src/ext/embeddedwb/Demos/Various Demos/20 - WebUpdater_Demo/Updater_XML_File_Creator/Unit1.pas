//***********************************************************
//                      WebUpdater  demo                    *
//                                                          *
//               For Delphi 6 - 2009                        *
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

unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Controls, forms, Dialogs, ShellAPI, Grids,
  LibXmlParser, LibXmlComps, StdCtrls, ComCtrls, ExtCtrls, RichEditBrowser,
  OleCtrls, EmbeddedWB, Buttons, SHDocVw_EWB, EwbCore;

type
  Tform1 = class(Tform)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    XmlScanner1: TXmlScanner;
    TreeView: TTreeView;
    Panel1: TPanel;
    memInfo: TMemo;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Panel3: TPanel;
    Panel4: TPanel;
    stgrInst: TStringGrid;
    Panel5: TPanel;
    Panel6: TPanel;
    GroupBox2: TGroupBox;
    cbOverWrite: TCheckBox;
    Panel7: TPanel;
    Button1: TButton;
    Button2: TButton;
    TabSheet4: TTabSheet;
    EmbeddedWB1: TEmbeddedWB;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn4: TBitBtn;
    gbFile: TGroupBox;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    edtPath: TEdit;
    cbNumerator: TCheckBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    edtName: TEdit;
    edtMajor: TEdit;
    edtAuthor: TEdit;
    edtCompany: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Author: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    edtMinor: TEdit;
    edtRelease: TEdit;
    edtBuild: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure XmlScanner1XmlProlog(Sender: TObject; XmlVersion,
      Encoding: string; Standalone: Boolean);
    procedure XmlScanner1StartTag(Sender: TObject; TagName: string;
      Attributes: TAttrList);
    procedure XmlScanner1PI(Sender: TObject; Target, Content: string;
      Attributes: TAttrList);
    procedure XmlScanner1EndTag(Sender: TObject; TagName: string);
    procedure XmlScanner1EmptyTag(Sender: TObject; TagName: string;
      Attributes: TAttrList);
    procedure XmlScanner1DtdRead(Sender: TObject; RootElementName: string);
    procedure XmlScanner1Content(Sender: TObject; Content: string);
    procedure XmlScanner1Comment(Sender: TObject; Comment: string);
    procedure XmlScanner1CData(Sender: TObject; Content: string);
    procedure btnLoadClick(Sender: TObject);
    procedure btnExploreClick(Sender: TObject);
    procedure stgrInstKeyPress(Sender: TObject; var Key: Char);
    procedure btnOpenIEClick(Sender: TObject);
    procedure btnOpenNotepadClick(Sender: TObject);
    procedure ledtVersionKeyPress(Sender: TObject; var Key: Char);
    procedure ledtCompanyKeyPress(Sender: TObject; var Key: Char);
    procedure ledtAuthorKeyPress(Sender: TObject; var Key: Char);
    procedure ledtNameKeyPress(Sender: TObject; var Key: Char);
    procedure btnLoadDemoClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCreateXMLClick(Sender: TObject);
  private
    CurNode: TTreeNode;
    fXmlParser: TXmlParser;
    procedure HideControls();
    procedure UpdateControls(Name: string);
    procedure UpdateComponents();
    procedure ParseInit(XmlFile: string);
    procedure SetAttr(AttrName: string; var st: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  form1: Tform1;

implementation

{$R *.dfm}

procedure Tform1.UpdateComponents();
begin
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  CurNode := nil;
  XmlScanner1.Filename := edtPath.Text;
  XmlScanner1.Execute;
  TreeView.Items.EndUpdate;
  TreeView.FullExpand;
  EmbeddedWB1.Go(edtPath.Text);
end;

procedure Tform1.UpdateControls(Name: string);
var
  i: integer;
begin
  edtPath.Text := Name;
  gbFile.Visible := true;
  Caption := 'Updates XML creator : ' + Name;
  for i := 1 to PageControl1.PageCount - 1 do
    PageControl1.Pages[i].TabVisible := true;
end;

procedure Tform1.HideControls();
var
  i: integer;
begin
  gbFile.Visible := false;
  TreeView.Items.Clear;
  Caption := 'XML creator : by bsalsa';
  EmbeddedWB1.GoAboutBlank;
  for i := 1 to PageControl1.PageCount - 1 do
    PageControl1.Pages[i].TabVisible := false;
end;

procedure Tform1.FormShow(Sender: TObject);
var
  i: integer;
begin
  Caption := 'Updates XML creator : by bsalsa';
  stgrInst.Cols[0].Text := '#';
  for i := 1 to stgrInst.RowCount - 1 do
  begin
    stgrInst.Cells[0, i] := IntToStr(i);
  end;
  with stgrInst do
  begin
    Cols[1].Text := 'File Name';
    Cols[2].Text := 'Destination folder (Include SubFolders if needed)';
    Cols[3].Text := 'Terminate (yes/no)';
  end;
  memInfo.Lines.Text := 'The Change Log: ';
  PageControl1.ActivePageIndex := 0;
  HideControls();
end;

procedure Tform1.btnClearClick(Sender: TObject);
var
  i, j: integer;
begin
  for i := 1 to stgrInst.RowCount - 1 do
    for j := 1 to stgrInst.ColCount - 1 do
      stgrInst.Cells[j, i] := '';
  memInfo.Lines.Clear;
  edtName.Text := '';
  edtMajor.Text := '';
  edtMinor.Text := '';
  edtRelease.Text := '';
  edtBuild.Text := '';
  edtCompany.Text := '';
  edtAuthor.Text := '';
  TreeView.Items.Clear;
  HideControls();
end;

procedure Tform1.btnLoadDemoClick(Sender: TObject);
begin
  edtName.Text := 'project1';
  edtMajor.Text := '1';
  edtMinor.Text := '0';
  edtRelease.Text := '0';
  edtBuild.Text := '0';
  edtCompany.Text := 'bsalsa Productions';
  edtAuthor.Text := 'bsalsa';
  with memInfo.Lines do
  begin
    Add('*Added new demo for the updater.');
    Add('*Added an option to create XML files.');
    Add('*Cleaned up the code.');
    Add('*Faster update procedures.');
    Add('*Option to add personal details.');
    Add('*Option to match details (application and remote file).');
    Add('and so on...');
  end;
  with stgrInst do
  begin
    Cells[1, 1] := 'Test.txt';
    Cells[2, 1] := 'Application new folder';
    Cells[3, 1] := 'no';
    Cells[1, 2] := 'Credits.txt';
    Cells[2, 2] := 'Updater_Test';
    Cells[3, 2] := 'no';
    Cells[1, 3] := 'ReadMe.txt';
    Cells[2, 3] := 'Updater_Test';
    Cells[3, 3] := 'yes';
    Cells[1, 4] := 'Project1.exe';
    Cells[2, 4] := '';
    Cells[3, 4] := 'yes';
  end;
end;

procedure Tform1.ledtNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (key = #13) or (key = #09) then EdtMajor.SetFocus;
end;

procedure Tform1.ledtVersionKeyPress(Sender: TObject; var Key: Char);
begin
  if (key = #13) or (key = #09) then EdtAuthor.SetFocus;
end;

procedure Tform1.ledtAuthorKeyPress(Sender: TObject; var Key: Char);
begin
  if (key = #13) or (key = #09) then EdtCompany.SetFocus;
end;

procedure Tform1.ledtCompanyKeyPress(Sender: TObject; var Key: Char);
begin
  if (key = #13) or (key = #09) then memInfo.SetFocus;
end;

procedure Tform1.btnOpenNotepadClick(Sender: TObject);
begin
  if edtPath.Text <> '' then
    ShellExecute(Handle, 'open', 'notepad.exe', Pchar(edtPath.Text), nil, SW_SHOWNORMAL)
  else
    MessageDlg('What file exactly you want to open?',
      mtError, [mbCancel], 0);
end;

procedure Tform1.btnOpenIEClick(Sender: TObject);
begin
  if edtPath.Text <> '' then
    ShellExecute(Handle, 'open', 'Explorer', Pchar(edtPath.Text), nil, SW_SHOWNORMAL)
  else
    MessageDlg('What file exactly do you want to open?', mtError, [mbCancel], 0);
end;

procedure Tform1.btnExploreClick(Sender: TObject);
begin
  if edtPath.Text <> '' then
    ShellExecute(Application.Handle, PChar('explore'),
      Pchar(ExtractFilePath(edtPath.Text)), nil, nil, SW_SHOWNORMAL)
  else
    MessageDlg('What file exactly do you want to open?',
      mtError, [mbCancel], 0);
end; ///ExtractFilePath

procedure Tform1.stgrInstKeyPress(Sender: TObject; var Key: Char);
begin
  if (key = #13) or (key = #09) then
    if stgrInst.Col < stgrInst.ColCount - 1 then
      with stgrInst do
      begin
        if col < 3 then
        begin
          Col := Col + 1;
          SetFocus;
        end
      end
    else
      with stgrInst do
      begin
        Row := Row + 1;
        Col := 1;
        SetFocus;
      end;
  if stgrInst.Row = stgrInst.RowCount - 1 then
    MessageDlg('You have reached the limit of 50 lines (You can change it ' +
      'to what ever, if you need more).', mtError, [mbCancel], 0);
end;

procedure Tform1.btnLoadClick(Sender: TObject);
var
  Container: string;
  od: TOpenDialog;
  i: integer;
  Node: TNvpNode;
  function GetXmlHead(): boolean;
  begin
    Result := false;
    while fXmlParser.Scan() do
    begin
      if fXmlParser.CurPartType = ptXmlProlog then
      begin
        Result := true;
        exit;
      end;
    end;
  end;

  function GetXmlTag(const TagName: string): boolean;
  begin
    Result := false;
    while fXmlParser.Scan() do
    begin
      if ((fXmlParser.CurPartType = ptStartTag)
        or (fXmlParser.CurPartType = ptEmptyTag))
        and (fXmlParser.CurName = TagName) then
      begin
        Result := true;
        Exit;
      end;
    end;
  end;

  function GetXmlData(): boolean;
  begin
    Result := false;
    while fXmlParser.Scan() do
    begin
      if ((fXmlParser.CurPartType = ptContent) or (fXmlParser.CurPartType = ptCData)) then
      begin
        Result := true;
        exit;
      end
    end;
  end;

begin
  OD := TOpenDialog.Create(Self);
  with OD do
  begin
    Title := 'Load XML file';
    Filter := 'XML files (*.xml)|*.XML';
    DefaultExt := 'xml';
  end;
  if OD.Execute then
  begin
    edtPath.Text := OD.Filename;
    Caption := 'XML creator : ' + OD.Filename;
    UpdateComponents();
    ParseInit(OD.Filename);
    try
      if not GetXmlHead() then exit;
      if not GetXmlTag('Updates') then Exit;
      if not GetXmlTag('Details') then Exit;
      if not GetXmlTag('ApplicationName') then Exit;
      if not GetXmlData() then Exit;
      edtName.Text := fXmlParser.CurContent;
      if not GetXmlTag('Author') then Exit;
      if not GetXmlData() then Exit;
      edtAuthor.Text := fXmlParser.CurContent;
      if not GetXmlTag('Company') then Exit;
      if not GetXmlData() then Exit;
      edtCompany.Text := fXmlParser.CurContent;

      if not GetXmlTag('MajorVersion') then Exit;
      if not GetXmlData() then Exit;
      EdtMajor.Text := fXmlParser.CurContent;

      if not GetXmlTag('MinorVersion') then Exit;
      if not GetXmlData() then Exit;
      EdtMinor.Text := fXmlParser.CurContent;

      if not GetXmlTag('ReleaseVersion') then Exit;
      if not GetXmlData() then Exit;
      EdtRelease.Text := fXmlParser.CurContent;

      if not GetXmlTag('BuildVersion') then Exit;
      if not GetXmlData() then Exit;
      EdtBuild.Text := fXmlParser.CurContent;


      if not GetXmlTag('ChangeLog') then Exit;
      while GetXmlTag('Info') do
      begin
        for i := 0 to fXmlParser.CurAttr.Count - 1 do
        begin
          Node := TNvpNode(fXmlParser.CurAttr[i]);
          if Node.Name = 'Text' then
            memInfo.Lines.Add(Node.Value)
        end;
      end;
      fXmlParser.StartScan;
      i := 0;
      if not GetXmlTag('Instructions') then Exit;
      while GetXmlTag('File') do
      begin
        if (FXmlParser.CurAttr.Count > 0) then
        begin
          inc(i);
          SetAttr('Name', Container);
          stgrInst.Cells[1, i] := Container;
          SetAttr('Destination', Container);
          stgrInst.Cells[2, i] := Container;
          SetAttr('Terminate', Container);
          stgrInst.Cells[3, i] := Container;
        end;
      end;
    finally
      UpdateControls(OD.FileName);
      OD.Free;
    end;
  end;
end;

procedure Tform1.ParseInit(XmlFile: string);
begin
  fXmlParser := TXmlParser.Create;
  with fXmlParser do
  begin
    LoadFromFile(PChar(XmlFile));
    Normalize := True;
    StartScan;
  end;
end;

procedure Tform1.btnCreateXMLClick(Sender: TObject);
var
  SD: TSaveDialog;
  i: integer;
  MS: TMemoryStream;
  st: string;
  procedure WriteString(const str: string);
  var
    Temp: Utf8String;
  begin
    if str <> '' then
    begin
      Temp := Utf8Encode(Str);
      MS.Write(Pointer(Temp)^, Length(Temp));
    end;
  end;

begin
  edtPath.Text := '';
  MS := TMemoryStream.Create;
  MS.Position := 0;
  try
    WriteString('<?xml version="1.0" encoding="UTF-8"?>'#13#10);
  //  WriteString('<?xml version="1.0" encoding="windows-1252"?>'#13#10);
    WriteString('<Updates>'#13#10);
    WriteString('   <Details>'#13#10);
    WriteString(#9'<ApplicationName>' + edtName.Text + '</ApplicationName>'#13#10);
    WriteString(#9'<Author>' + edtAuthor.Text + '</Author>'#13#10);
    WriteString(#9'<Company>' + edtCompany.Text + '</Company>'#13#10);

    WriteString(#9'<MajorVersion>' + edtMajor.Text + '</MajorVersion>'#13#10);
    WriteString(#9'<MinorVersion>' + edtMinor.Text + '</MinorVersion>'#13#10);
    WriteString(#9'<ReleaseVersion>' + edtRelease.Text + '</ReleaseVersion>'#13#10);
    WriteString(#9'<BuildVersion>' + edtBuild.Text + '</BuildVersion>'#13#10);

    WriteString('   </Details>'#13#10);
    WriteString('   <ChangeLog>'#13#10);
    for i := 1 to memInfo.Lines.Count - 1 do
    begin
      if cbNumerator.Checked then
        st := IntToStr(i) + '. ' + memInfo.Lines.Strings[i] + ''
      else
        st := memInfo.Lines.Strings[i] + '';
      WriteString(#9 + '<Info Text=" ' + st + '"/>' + #13#10);
    end;
    WriteString('   </ChangeLog>'#13#10);
    WriteString('   <Instructions>'#13#10);
    for i := 1 to stgrInst.RowCount - 1 do
    begin
      if stgrInst.Cells[1, 1] <> '' then
      begin
        if stgrInst.Cells[1, i] <> '' then
        begin
          if ((stgrInst.Cells[3, i] = 'yes') or (stgrInst.Cells[3, i] = 'no')) then
          begin
            WriteString(#9'<File Name=" ' + stgrInst.Cells[1, i] + '" ' +
              'Destination="' + stgrInst.Cells[2, i] + '" ' +
              'Terminate="' + stgrInst.Cells[3, i] + '"' + '/>'#13#10)
          end
          else
          begin
            MessageDlg('The Terminame field must contain "yes" or "no" only.'
              , mtError, [mbCancel], 0);
          end;
        end;
      end
      else
      begin
        MessageDlg('You must enter at least one file to update :).'
          , mtError, [mbCancel], 0);
        Exit;
      end;
    end;
    WriteString('   </Instructions>'#13#10);
    WriteString('</Updates>'#13#10);

    SD := TSaveDialog.Create(Self);
    with SD do
    begin
      Title := 'Save XML file';
      DefaultExt := 'xml';
      Filter := 'XML files (*.xml)|*.XML';
      FileName := 'NewUpdate.xml';
      if cbOverWrite.Checked then
        Options := [ofHideReadOnly, ofEnableSizing, ofOverWritePrompt];
    end;
    if SD.Execute then
    begin
      ms.SaveToFile(SD.FileName);
      UpdateControls(SD.FileName);
      UpdateComponents;
      ShowMessage('You have ceated the proper XML file.' + #10 + #13 +
        'The file is stored in: ' + #10 + #13 + edtPath.Text + #10 + #13 +
        'Now, upload the file to the web site remote folder using ftp.');
    end;
  finally
    EmbeddedWB1.Go(edtPath.Text);
    ms.Free;
  end;
end;

procedure Tform1.SetAttr(AttrName: string; var st: string);
var
  Node: TNvpNode;
begin
  Node := FXmlParser.CurAttr.Node(AttrName);
  if Node <> nil then
    st := Node.Value;
end;

procedure Tform1.XmlScanner1CData(Sender: TObject; Content: string);
begin
  Content := StringReplace(Content, #13, ' ', [rfReplaceAll]);
  Content := StringReplace(Content, #10, '', [rfReplaceAll]);
  TreeView.Items.AddChild(CurNode, Content);
end;

procedure Tform1.XmlScanner1Comment(Sender: TObject; Comment: string);
begin
  TreeView.Items.AddChild(CurNode, 'Comment');
end;

procedure Tform1.XmlScanner1Content(Sender: TObject; Content: string);
begin
  Content := StringReplace(Content, #13, ' ', [rfReplaceAll]);
  Content := StringReplace(Content, #10, '', [rfReplaceAll]);
  TreeView.Items.AddChild(CurNode, Content);
end;

procedure Tform1.XmlScanner1DtdRead(Sender: TObject; RootElementName: string);
begin
  TreeView.Items.AddChild(CurNode, 'DTD: ' + RootElementName);
end;

procedure Tform1.XmlScanner1EmptyTag(Sender: TObject; TagName: string;
  Attributes: TAttrList);
var
  i: integer;
begin
  CurNode := TreeView.Items.AddChild(CurNode, 'Element "' + TagName + '" (Empty)');
  for i := 0 to Attributes.Count - 1 do
    TreeView.Items.AddChild(CurNode, '  * Attribute ' + Attributes.Name(i) + '=' + Attributes.Value(i));
  CurNode := CurNode.Parent;
end;

procedure Tform1.XmlScanner1EndTag(Sender: TObject; TagName: string);
begin
  if CurNode <> nil then
    CurNode := CurNode.Parent;
end;

procedure Tform1.XmlScanner1PI(Sender: TObject; Target, Content: string;
  Attributes: TAttrList);
begin
  TreeView.Items.AddChild(CurNode, 'Processing Instruction: ' + Content);
end;

procedure Tform1.XmlScanner1StartTag(Sender: TObject; TagName: string;
  Attributes: TAttrList);
var
  i: integer;
begin
  CurNode := TreeView.Items.AddChild(CurNode, 'Element "' + TagName + '"');
  for i := 0 to Attributes.Count - 1 do
    TreeView.Items.AddChild(CurNode, '  * Attribute ' + Attributes.Name(i) + '=' + Attributes.Value(i));
end;

procedure TForm1.XmlScanner1XmlProlog(Sender: TObject; XmlVersion,
  Encoding: string; Standalone: Boolean);
begin
  TreeView.Items.AddChild(CurNode, 'XML Prolog: Version=' + XmlVersion + ' Encoding=' + Encoding);
end;

procedure Tform1.Button1Click(Sender: TObject);
begin
  TreeView.FullExpand;
end;

procedure Tform1.Button2Click(Sender: TObject);
begin
  TreeView.FullCollapse;
end;

procedure Tform1.FormResize(Sender: TObject);
begin
  with stgrInst do
  begin
    ColWidths[1] := Round(100 * Self.Width / 580);
    ColWidths[2] := Round(335 * Self.Width / 580);
  end;
end;

procedure Tform1.SpeedButton3Click(Sender: TObject);
begin
  edtName.Text := '';
  edtMajor.Text := '';
  edtMinor.Text := '';
  edtRelease.Text := '';
  edtBuild.Text := '';
  edtCompany.Text := '';
  edtAuthor.Text := '';
  HideControls();
end;

procedure Tform1.SpeedButton1Click(Sender: TObject);
begin
  memInfo.Lines.Clear;
  HideControls();
end;

procedure Tform1.SpeedButton2Click(Sender: TObject);
var
  i, j: integer;
begin
  for i := 1 to stgrInst.RowCount - 1 do
    for j := 1 to stgrInst.ColCount - 1 do
      stgrInst.Cells[j, i] := '';
  HideControls();
end;



end.

