(*
  Name:             sysinfo_main
  Copyright:        Copyright (C) SIL International.
  Documentation:    km4.1
  Description:      Main form for system information
  Create Date:      13 May 2005

  Modified Date:    15 Apr 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          13 May 2005 - mcdurdin - Integrated into kmshell from tsysinfo
                    15 Apr 2015 - mcdurdin - I4659 - V9.0 - Add more detailed keyboard diagnostics
*)
unit sysinfo_main; // I3309

interface

{$MESSAGE HINT 'Hook up remainder of CEF'}

uses
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Contnrs, Menus, si_base, ExtCtrls,
  Keyman.UI.UframeCEFHost,
  msxml;

type
  TfrmDiagnostics = class(TForm)
    pages: TPageControl;
    menu: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    N1: TMenuItem;
    mnuExit: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuOptions: TMenuItem;
    mnuOptionsAdvancedView: TMenuItem;
    N2: TMenuItem;
    mnuSendToKeyman: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    N3: TMenuItem;
    mnuFileRefresh: TMenuItem;
    panSimple: TPanel;
    lblTitle: TLabel;
    cmdSendToKeyman: TButton;
    lblSendToKeyman: TLabel;
    panNoIssuesFound: TPanel;
    lblNoIssuesFound: TLabel;
    panIssuesFound: TPanel;
    lblIssuesFound: TLabel;
    lblSendSuggestion: TLabel;
    lbIssues: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mnuOptionsXMLView: TMenuItem;
    Reloadpage1: TMenuItem;
    mnuOptionsDebugTests: TMenuItem;
    mnuOptionsSentryExceptionTest: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuFileSaveAsClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuSendToKeymanClick(Sender: TObject);
    procedure mnuFileRefreshClick(Sender: TObject);
    procedure mnuOptionsAdvancedViewClick(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure mnuOptionsXMLViewClick(Sender: TObject);
    procedure mnuOptionsSentryExceptionTestClick(Sender: TObject);
    procedure mnuOptionsClick(Sender: TObject);
  private
    FSIList: TSIList;
    FGlobalXMLDocument: IXMLDOMDocument;
    // procedure EnumerateDlls;
    { Private declarations }
    procedure SINewParent(Sender: TSIList; const Caption: string;
      var Parent: TWinControl);
    procedure GetDiagFiles;
    procedure PrepareTabs; // I3556
    procedure ShowFiles; // I3556
    procedure LoadPage(FPageIndex: Integer); // I3556
    procedure webLoadEnd(ASender: TObject);
    procedure WMUserLoad(var Message: TMessage); message WM_USER_Load;
    procedure SaveDiagnosticsAndExit(FileName: string);
    procedure LoadGlobalData;
    // I3180   // I3542   // I3556   // I439   // I4390
  public
    function CollectDiagnostics: string;
    procedure DeleteDiagFiles; // I3778
    { Public declarations }
  end;

var
  frmDiagnostics: TfrmDiagnostics;

procedure KeymanDiag;

implementation

uses
  System.Win.ComObj,
  Winapi.ActiveX,
  Winapi.shlobj,

  ErrorControlledRegistry,
  Keyman.System.KeymanSentryClient,
  KeymanPaths,
  RegistryKeys,
  sysinfo_Util,
  UframeAttachedFiles,
  UfrmEmail,
  utilexecute,
  VersionInfo;

{$R *.dfm}

const
  SCaption = 'Keyman System Information';

procedure KeymanDiag;
begin
  Application.Icon.ReleaseHandle;
  Application.Icon.Handle := LoadIcon(HInstance, MAKEINTRESOURCE(5));
  Application.Title := SCaption;
  Application.CreateForm(TfrmDiagnostics, frmDiagnostics);
  Application.Run;
end;

procedure TfrmDiagnostics.mnuOptionsSentryExceptionTestClick(Sender: TObject);
begin
  TKeymanSentryClient.Validate(True);
end;

function TfrmDiagnostics.CollectDiagnostics: string;
begin
  FSIList.Collect;
  GetDiagFiles;
  Result := TempFileName('.tsi');
  FSIList.Save(Result);
end;

procedure TfrmDiagnostics.FormCreate(Sender: TObject);
begin
  Icon.ReleaseHandle;
  Icon.Handle := LoadIcon(HInstance, MAKEINTRESOURCE(5));

  FSIList := TSIList.Create(Self);
  FSIList.OnNewParent := SINewParent;

  if (ParamStr(1) = '-d') and (ParamCount = 2) then
  begin
    SaveDiagnosticsAndExit(ParamStr(2));
    Exit
  end;

  if (ParamCount = 1) then
  begin
    Caption := SCaption + ' - ' + ExtractFileName(ParamStr(1));
    FSIList.Load(ParamStr(1));
    ShowFiles; // I4559
    PrepareTabs; // I3180   // I3542   // I3556
    mnuOptionsAdvancedViewClick(nil);
  end
  else
  begin
    Caption := SCaption + ' - Local Machine';
    FSIList.Collect;
    GetDiagFiles;
    ShowFiles; // I3556
    PrepareTabs; // I3180   // I3542   // I3556
    // mnuOptionsAdvancedViewClick(nil);   // I4439
    panIssuesFound.Visible := False; // lbIssues.Items.Count > 0;
    panNoIssuesFound.Visible := False; // lbIssues.Items.Count = 0;
  end;
end;

procedure TfrmDiagnostics.SaveDiagnosticsAndExit(FileName: string);
begin
  FSIList.Collect;
  GetDiagFiles;
  FSIList.Save(FileName);

  Application.ShowMainForm := False;
  Application.Terminate;
end;

procedure TfrmDiagnostics.FormDestroy(Sender: TObject);
begin
  FSIList.Free;
end;

procedure TfrmDiagnostics.mnuFileOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    while pages.PageCount > 0 do
      pages.pages[0].Free;
    FSIList.Load(dlgOpen.FileName);
    PrepareTabs; // I3180   // I3542   // I3556
  end;
end;

procedure TfrmDiagnostics.SINewParent(Sender: TSIList; const Caption: string;
  var Parent: TWinControl);
var
  tab: TTabSheet;
begin
  tab := TTabSheet.Create(Self);
  tab.Parent := pages;
  tab.TabVisible := True;
  tab.PageControl := pages;
  tab.Caption := Caption;
  Parent := tab;
end;

procedure TfrmDiagnostics.mnuFileSaveAsClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    FSIList.Save(dlgSave.FileName);
  end;
end;

procedure TfrmDiagnostics.mnuHelpAboutClick(Sender: TObject);
begin
  ShowMessage(SCaption + ' Version ' + GetVersionString);
end;

procedure TfrmDiagnostics.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmDiagnostics.GetDiagFiles;
var
  f: TSearchRec;
  FPath: string;
  str: TStrings;
  i: Integer;
  m: Integer;
begin
  FSIList.Files.Clear;
  FPath := TKeymanPaths.ErrorLogPath;
  if FindFirst(FPath + '*', 0, f) = 0 then
  begin
    i := 0;
    repeat
      FSIList.Files.Add(FPath + f.Name);
      Inc(i);
      if i > 25 then
        Break;
    until FindNext(f) <> 0;
    FindClose(f);
  end;

  str := TStringList.Create;
  with TRegistryErrorControlled.Create do // I2890
    try
      if OpenKeyReadOnly(SRegKey_KeymanEngineDiag_CU) then
      begin
        GetValueNames(str);
        m := str.Count;
        if m > 25 then
          m := 25;

        for i := 0 to m - 1 do
          if FileExists(str[i]) then
            FSIList.Files.Add(str[i]);
      end;
    finally
      str.Free;
      Free;
    end;

  FPath := GetFolderPath(CSIDL_DESKTOPDIRECTORY) + 'keymanlog\';
  if FindFirst(FPath + '*', 0, f) = 0 then
  begin
    i := 0;
    repeat
      FSIList.Files.Add(FPath + f.Name);
      Inc(i);
      if i > 25 then
        Break;
    until FindNext(f) <> 0;
    FindClose(f);
  end;
end;

procedure TfrmDiagnostics.DeleteDiagFiles;
var
  FHasKey: Boolean;
  i: Integer;
begin
  GetDiagFiles;

  with TRegistryErrorControlled.Create do // I2890
    try
      FHasKey := OpenKey(SRegKey_KeymanEngineDiag_CU, False);

      for i := 0 to FSIList.Files.Count - 1 do
      begin
        DeleteFile(FSIList.Files[i]);
        if FHasKey and ValueExists(FSIList.Files[i]) then
          DeleteValue(FSIList.Files[i]);
      end;

    finally
      Free;
    end;

  FSIList.Files.Clear;
end;

procedure TfrmDiagnostics.ShowFiles; // I3556
var
  tab: TTabSheet;
  filespage: TframeAttachedFiles;
begin
  tab := TTabSheet.Create(Self);
  tab.Parent := pages;
  tab.TabVisible := True;
  tab.PageControl := pages;
  tab.Caption := 'Attached Files';

  filespage := TframeAttachedFiles.Create(Self);
  filespage.Align := alClient;
  filespage.Parent := tab;
  filespage.Visible := True;
  filespage.AddFiles(FSIList.Files); // I3672
end;

procedure TfrmDiagnostics.mnuSendToKeymanClick(Sender: TObject);
var
  ffilename: string;
  i: Integer;
  f: TSearchRec;
  FTotalSize: Int64;
  FSavedFileList: string;
  FDeleteFiles: Boolean;
begin
  ffilename := TempFileName('.tsi');

  FTotalSize := 0;

  for i := 0 to FSIList.Files.Count - 1 do
  begin
    if FindFirst(FSIList.Files[i], 0, f) = 0 then
    begin
      FTotalSize := FTotalSize + f.Size;
      FindClose(f);
    end
  end;

  FSavedFileList := FSIList.Files.Text;

  FDeleteFiles := False; // I2240

  if FTotalSize > 1024 * 1024 * 8 then // 8MB
  begin
    case MessageDlg('The attached files are very large (' +
      FileSizeKB(FTotalSize) +
      ').  Do you want to send them - this may take some time?', mtConfirmation,
      mbYesNoCancel, 0) of
      mrYes:
        FDeleteFiles := True;
      mrNo:
        FSIList.Files.Text := '';
      mrCancel:
        Exit;
    end;
  end
  else
    FDeleteFiles := True; // I2240

  FSIList.Save(ffilename);

  FSIList.Files.Text := FSavedFileList;

  with TfrmEmail.Create(Self) do
    try
      AttachFile := ffilename;
      if ShowModal = mrOk then
        if FDeleteFiles then
          DeleteDiagFiles; // I2240
    finally
      DeleteFile(ffilename);
      Free;
    end;
end;

procedure TfrmDiagnostics.mnuFileRefreshClick(Sender: TObject);
var
  i: Integer;
begin
  Caption := SCaption + ' - Local Machine';
  for i := pages.PageCount - 1 downto 0 do
    pages.pages[i].Free;
  FSIList.Collect;
  GetDiagFiles;
  PrepareTabs; // I3180   // I3542   // I3556
end;

procedure TfrmDiagnostics.LoadGlobalData;
var
  s: WideString;
  i: Integer;
begin
  s := '<?xml version="1.0" encoding="utf-8" ?>'#13#10;  // I2919
  s := s + '<TavultesoftSystemInformation>';
  for i := 0 to FSIList.Count - 1 do
    s := s + '<'+FSIList[i].ClassName+'>'+FSIList[i].XMLData+'</'+FSIList[i].ClassName+'>'#13#10;
  s := s + '</TavultesoftSystemInformation>';
  FGlobalXMLDocument := CreateOleObject('MSXML.DOMDocument') as IXMLDOMDocument;
  FGlobalXMLDocument.loadXML(s);
end;
procedure TfrmDiagnostics.PrepareTabs; // I3180   // I3542   // I3556
var
  i: Integer;
  web: TframeCEFHost;
  si: TSI_Base;
begin
  LoadGlobalData;
  for i := 0 to FSIList.Count - 1 do
  begin
    si := FSIList[i];
    web := TframeCEFHost.Create(si.Parent);
    web.Parent := si.Parent as TWinControl;
    //(si.Parent as TWinControl).InsertControl(web);
    (si.Parent as TTabSheet).Caption := si.Caption;
    (si.Parent as TTabSheet).TabVisible := True;
    web.Align := alClient;
    web.Visible := True;
    LoadPage(i);
    // pages.Pages[i].TabVisible := (pages.Pages[i].ControlCount > 0) and pages.Pages[i].Controls[0].Visible;
  end;
end;

procedure TfrmDiagnostics.mnuOptionsAdvancedViewClick(Sender: TObject);
begin
  mnuOptionsAdvancedView.Checked := not mnuOptionsAdvancedView.Checked;
  panSimple.Visible := not mnuOptionsAdvancedView.Checked;
  pages.Visible := mnuOptionsAdvancedView.Checked;
end;

procedure TfrmDiagnostics.mnuOptionsClick(Sender: TObject);
begin
  mnuOptionsDebugTests.Visible := (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_SHIFT) < 0);
end;

procedure TfrmDiagnostics.mnuOptionsXMLViewClick(Sender: TObject); // I3766
var
  i: Integer;
begin
  mnuOptionsXMLView.Checked := not mnuOptionsXMLView.Checked;
  for i := 0 to FSIList.Count - 1 do
    LoadPage(i);
end;

procedure TfrmDiagnostics.LoadPage(FPageIndex: Integer); // I3556
begin
  PostMessage(Handle, WM_USER_Load, 0, FPageIndex);
end;

procedure TfrmDiagnostics.webLoadEnd(ASender: TObject); // I3556
begin
  // Cleanup;
end;

procedure TfrmDiagnostics.WMUserLoad(var Message: TMessage); // I3556
// const
// UTF16BOM: WideString = #$FEFF;
var
  xml, xsl: IXMLDOMDocument;
  buf: WideString;
  web: TframeCEFHost;
  afilename: string;
begin
  web := (FSIList[Message.LParam].Parent as TTabSheet).Controls[0]
    as TframeCEFHost;
  web.OnLoadEnd := webLoadEnd;

  if not mnuOptionsXMLView.Checked and FSIList[Message.LParam].HasXSLT then
  begin
    afilename := TempFileName('.html');
    if FSIList[Message.LParam].UseGlobalData then
      xml := FGlobalXMLDocument
    else
    begin
      xml := CreateOleObject('MSXML.DOMDocument') as IXMLDOMDocument;
      xml.loadXML(FSIList[Message.LParam].XMLData);
    end;

    xsl := CreateOleObject('MSXML.DOMDocument') as IXMLDOMDocument;
    xsl.Load(FSIList[Message.LParam].XSLTFile);
    buf := xml.transformNode(xsl);

    with TStringStream.Create(buf, TEncoding.UTF8) do
    try
      SaveToFile(afilename);
    finally
      Free;
    end;
  end
  else
  begin
    afilename := TempFileName('.xml');
    xml := CreateOleObject('MSXML.DOMDocument') as IXMLDOMDocument;
    xml.loadXML(FSIList[Message.LParam].XMLData);
    xml.Save(afilename);
  end;

  web.Navigate(afilename);
end;

end.
