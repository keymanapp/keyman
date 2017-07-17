(*
  Name:             UfrmPackageEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Support tab child format
                    23 Aug 2006 - mcdurdin - Rework UI with new Unicode controls
                    23 Aug 2006 - mcdurdin - Integrate source editor
                    28 Sep 2006 - mcdurdin - Added Open Containing Folder buttons and Edit File button
                    28 Sep 2006 - mcdurdin - Added on-screen keyboard to the package when autopackaging a .kmx
                    06 Oct 2006 - mcdurdin - Warn if package is being upgraded to version 7.0
                    04 Dec 2006 - mcdurdin - Add upgrade warning; upload to website; default ext
                    04 Dec 2006 - mcdurdin - Fix enabled controls and filetype info
                    12 Dec 2006 - mcdurdin - Fix FocusTab
                    04 Jan 2007 - mcdurdin - Fix start menu path text and clear listbox when loading
                    15 Jan 2007 - mcdurdin - Add (Start Product) and other shortcuts targets, remove $KEYMAN\keyman.exe, $KEYMAN\kmshell.exe (deprecated)
                    22 Jan 2007 - mcdurdin - Fix the package image availability for Standard Edition
                    30 Jan 2007 - mcdurdin - Remove pro edition watermark
                    21 Mar 2007 - mcdurdin - I709 - Changes to source editor do not save when Save is clicked
                    16 May 2007 - mcdurdin - I233 - Ensure all text is stored as Unicode
                    16 May 2007 - mcdurdin - I333 - Additional widestring support
                    30 May 2007 - mcdurdin - I817 - Build bootstrap installer
                    23 Aug 2007 - mcdurdin - I962 - Fix for uploading package and pressing Cancel
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    16 Jan 2009 - mcdurdin - I1730 - Add label suggesting version format for online update support
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    18 Mar 2011 - mcdurdin - I2178 - Compile Product Installer should prompt to save first
                    18 Mar 2011 - mcdurdin - I1756 - Adding shortcuts to package does not mark package as dirty
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    02 Feb 2012 - mcdurdin - I3100 - Rare crash when adding files to a package
                    03 Nov 2012 - mcdurdin - I3506 - V9.0 - Merge of I3100 - Rare crash when adding files to a package
                    27 Feb 2014 - mcdurdin - I4081 - V9.0 - Trace compile errors in subfiles in Keyman Developer
                    04 May 2015 - mcdurdin - I4689 - V9.0 - Redesign package editor as left tabbed
                    04 May 2015 - mcdurdin - I4690 - V9.0 - Pull keyboard version into package version when adding a keyboard
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    06 May 2015 - mcdurdin - I4702 - Package actions crash if package not part of a project [CrashID:tike.exe_9.0.496.0_008033BD_EAccessViolation]
                    27 May 2015 - mcdurdin - I4253 - Developer crashes when upload to website fails [CrashID:tike.exe_9.0.449.0_008FED3C_Exception]
                    27 May 2015 - mcdurdin - I4618 - Developer crashes when upload to website fails [CrashID:tike.exe_9.0.481.0_0088D9A8_Exception]
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    03 Aug 2015 - mcdurdin - I4814 - Image preview in package editor doesn't refresh after changes
*)
unit UfrmPackageEditor;  // I3306   // I4796

interface

uses
  System.UITypes,
  Windows, SysUtils, Messages, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, Dialogs, CommDlg, kmxfile, kpsfile,
  ImgList, UfrmMDIChild, ProjectFile, PackageInfo,
  UfrmMDIEditor, Grids, dmActionsMain,
  UserMessages,
  UframeTextEditor, LeftTabbedPageControl, ProjectFileUI;

type
  TfrmPackageEditor = class(TfrmTikeEditor)   // I4689
    dlgFiles: TOpenDialog;
    dlgNewCustomisation: TSaveDialog;
    dlgOpenProductInstaller: TOpenDialog;
    pages: TLeftTabbedPageControl;
    pageFiles: TTabSheet;
    pageDetails: TTabSheet;
    pageShortcuts: TTabSheet;
    pageSource: TTabSheet;
    pageCompile: TTabSheet;
    Panel1: TPanel;
    imgFileIcon: TImage;
    Label1: TLabel;
    Label8: TLabel;
    lbFiles: TListBox;
    cmdAddFile: TButton;
    cmdRemoveFile: TButton;
    editFileType: TEdit;
    editFilePath: TEdit;
    lblFileDetails: TLabel;
    lblFilePath: TLabel;
    lblFileType: TLabel;
    memoFileDetails: TMemo;
    cmdOpenFile: TButton;
    cmdOpenContainingFolder: TButton;
    Panel2: TPanel;
    lblKMPImageFile: TLabel;
    lblKMPImageSize: TLabel;
    lblReadme: TLabel;
    lblStep2b: TLabel;
    lblStep2c: TLabel;
    lblStep2d: TLabel;
    lblStep2e: TLabel;
    lblStep2f: TLabel;
    lblStep2g: TLabel;
    lblStep2a: TLabel;
    lblPackageDetails: TLabel;
    lblPackageRequiredInformation: TLabel;
    lblVersionHint: TLabel;
    cbReadMe: TComboBox;
    editInfoName: TEdit;
    editInfoVersion: TEdit;
    editInfoCopyright: TEdit;
    editInfoAuthor: TEdit;
    editInfoEmail: TEdit;
    editInfoWebSite: TEdit;
    cmdInsertCopyright: TButton;
    cbKMPImageFile: TComboBox;
    panKMPImageSample: TPanel;
    imgKMPSample: TImage;
    Panel3: TPanel;
    lblStartMenuOptions: TLabel;
    lblStartMenuDescription: TLabel;
    lblStartMenuParameters: TLabel;
    lblStartMenuProgram: TLabel;
    lblStartMenuEntries: TLabel;
    lblShortcuts: TLabel;
    chkStartMenuUninstall: TCheckBox;
    chkCreateStartMenu: TCheckBox;
    editStartMenuPath: TEdit;
    lbStartMenuEntries: TListBox;
    cmdNewStartMenuEntry: TButton;
    cmdDeleteStartMenuEntry: TButton;
    editStartMenuDescription: TEdit;
    editStartMenuParameters: TEdit;
    cbStartMenuProgram: TComboBox;
    Panel4: TPanel;
    Label13: TLabel;
    lblCompilePackage: TLabel;
    lblInstallHint: TLabel;
    lblBootstrap: TLabel;
    lblBootstrapMSI: TLabel;
    cmdBuildPackage: TButton;
    cmdInstall: TButton;
    editOutPath: TEdit;
    cmdOpenContainingFolder2: TButton;
    cmdAddToProject: TButton;
    cmdUninstall: TButton;
    cmdCompileInstaller: TButton;
    cmdInstallWith: TButton;
    editInstallerOutputFilename: TEdit;
    procedure cmdCloseClick(Sender: TObject);
    procedure cmdAddFileClick(Sender: TObject);
    procedure cmdRemoveFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbFilesClick(Sender: TObject);
    procedure cmdBuildPackageClick(Sender: TObject);
    procedure editInfoNameChange(Sender: TObject);
    procedure editInfoVersionChange(Sender: TObject);
    procedure editInfoCopyrightChange(Sender: TObject);
    procedure editInfoAuthorChange(Sender: TObject);
    procedure editInfoEmailChange(Sender: TObject);
    procedure editInfoWebSiteChange(Sender: TObject);
    procedure editCmdLineChange(Sender: TObject);
    procedure cmdInstallClick(Sender: TObject);
    procedure cmdInsertCopyrightClick(Sender: TObject);
    procedure chkStartMenuUninstallClick(Sender: TObject);
    procedure chkCreateStartMenuClick(Sender: TObject);
    procedure cbReadMeClick(Sender: TObject);
    procedure lbStartMenuEntriesClick(Sender: TObject);
    procedure editStartMenuDescriptionChange(Sender: TObject);
    procedure cbStartMenuProgramChange(Sender: TObject);
    procedure editStartMenuParametersChange(Sender: TObject);
    procedure cmdNewStartMenuEntryClick(Sender: TObject);
    procedure cmdDeleteStartMenuEntryClick(Sender: TObject);
    procedure editStartMenuPathChange(Sender: TObject);
    procedure cbKMPImageFileClick(Sender: TObject);
    procedure cmdUninstallClick(Sender: TObject);
    procedure cmdOpenContainingFolder2Click(Sender: TObject);
    procedure cmdOpenContainingFolderClick(Sender: TObject);
    procedure cmdOpenFileClick(Sender: TObject);
    procedure cmdCompileInstallerClick(Sender: TObject);
    procedure cmdInstallWithClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pagesChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure pagesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    pack: TKPSFile;
    FSetup: Integer;
    frameSource: TframeTextEditor;

    procedure UpdateOutPath;
    procedure UpdateData;

    procedure UpdateInfo(nm, desc, url: WideString);
    procedure WMUserFormShown(var Message: TMessage); message WM_User_FormShown;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure EnableStartMenuControls;
    function DoAction(action: TProjectFileAction): Boolean;
    procedure UpdateReadme;
    procedure UpdateImageFiles;
    procedure GetStartMenuEntries(var AName, AProg, AParams: WideString);
    procedure SetStartMenuEntries(AName, AProg, AParams: WideString);
    procedure UpdateStartMenuPrograms;
    procedure FillFileList(combo: TComboBox; obj: TObject);
    procedure UpdateImagePreviews;
    procedure AddFile(FileName: WideString);
    procedure SourceChanged(Sender: TObject);
    procedure MovePackageToSource;
    function MoveSourceToPackage: Boolean;

  protected
    function DoOpenFile: Boolean; override;
    function DoSaveFile: Boolean; override;
    function GetFileNameFilter: string; override;
    function GetDefaultExt: string; override;

    procedure FocusTab;
    function GetProjectFile: TProjectFile; override;   // I4702

  public
    function GetPack: TKPSFile;

    procedure CreateFromKMX(FFileName: WideString);

    procedure FindError(const Filename: string; s: string); override;   // I4081

    function CanChangeTab(FForward: Boolean): Boolean; override;
    procedure ChangeTab(FForward: Boolean); override;
  end;

implementation

uses
  CharMapDropTool,
  CharMapInsertMode,
  CompilePackageInstaller,
  kpsProjectFile,
  OnlineConstants,
  Project,
  ProjectFileType,
  ShellApi,
  TextFileFormat,
  TTInfo,
  utilfiletypes,
  utilsystem,
  UfrmMain,
  UfrmMessages,
  xmldoc;

{$R *.DFM}

{-------------------------------------------------------------------------------
 - Form and general routines                                                   -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.FocusTab;
begin
  if not CanFocus then
    Exit;
  if pages.ActivePage = pageFiles then
    lbFiles.SetFocus
  else if pages.ActivePage = pageDetails then
    editInfoName.SetFocus
  else if pages.ActivePage = pageShortcuts then
    chkCreateStartMenu.SetFocus
  else if pages.ActivePage = pageSource then
    frameSource.SetFocus
  else if pages.ActivePage = pageCompile then
    cmdBuildPackage.SetFocus;
end;

procedure TfrmPackageEditor.FormCreate(Sender: TObject);
begin
  Inc(FSetup);
  try
    pages.ActivePage := pageFiles;
    pack := TKPSFile.Create;
    EnableStartMenuControls;
    UpdateStartMenuPrograms;
    UpdateReadme;
    UpdateImageFiles;
    UpdateImagePreviews;

    frameSource := TframeTextEditor.Create(Self);
    frameSource.Parent := pageSource;
    frameSource.Align := alClient;
    frameSource.EditorFormat := efXML;
    frameSource.Visible := True;
    frameSource.OnChanged := SourceChanged;
    frameSource.TextFileFormat := tffUTF8;

    GetCharMapDropTool.Handle(Self, cmimText);

    cmdAddToProject.Caption := '&Add to Project';

    inherited;
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.SourceChanged(Sender: TObject);
begin
  if FSetup = 0 then Modified := True;
end;

procedure TfrmPackageEditor.cmdCompileInstallerClick(Sender: TObject);
begin
  if Untitled or Modified then   // I2178
    ShowMessage('You must save the package before you can build it.')
  else
  begin
    frmMessages.Clear;
    DoAction(pfaCompileInstaller);
  end;
end;

procedure TfrmPackageEditor.FormDestroy(Sender: TObject);
begin
  inherited;
  pack.Free;
end;

procedure TfrmPackageEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Shift = []) and (Key = VK_F7) then
  begin
    Key := 0;
    cmdBuildPackageClick(Self);
  end;
end;

procedure TfrmPackageEditor.FormShow(Sender: TObject);
begin
  inherited;
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

function TfrmPackageEditor.DoSaveFile: Boolean;
begin
  Result := False;

  if pages.ActivePage = pageSource then
  begin
    if not MoveSourceToPackage then
    begin
      Result := False;
      Exit;
    end;
  end;

  if pack.WasIni then
  begin
    if MessageDlg('The file '+FileName+' was originally created in Keyman Developer 6.2 or an earlier version.  '+
        'Saving it in Keyman Developer 7 changes the file format and it will no longer be editable in '+
        'Keyman Developer 6.2.'#13#10#13#10+'Continue save?', mtWarning, mbOkCancel, 0) = mrCancel then
      Exit;
  end;
  pack.FileName := FileName;
  pack.SaveXML;
  Modified := False;
  Result := True;
end;

procedure TfrmPackageEditor.cmdCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPackageEditor.WMSysCommand(var Message: TWMSysCommand);
begin
  if ((Message.CmdType = SC_NEXTWINDOW) or (Message.CmdType = SC_PREVWINDOW)) and (Message.XPos = 9)
    then PostMessage(Handle, CM_DIALOGKEY, VK_TAB, 0)
    else inherited;
end;

procedure TfrmPackageEditor.WMUserFormShown(var Message: TMessage);
begin
  inherited;
  FocusTab;
end;

function TfrmPackageEditor.GetPack: TKPSFile;
begin
  Result := pack;
end;

function TfrmPackageEditor.GetProjectFile: TProjectFile;   // I4702
begin
  Result := inherited GetProjectFile;
  if not Assigned(Result) then
  begin
    FStandaloneProjectFile := TkpsProjectFile.Create(nil, FileName, nil);
    Result := FStandaloneProjectFile;
  end;
end;

{-------------------------------------------------------------------------------
 - Interface routines                                                          -
 -------------------------------------------------------------------------------}

function TfrmPackageEditor.GetDefaultExt: string;
begin
  Result := 'kps';
end;

function TfrmPackageEditor.GetFileNameFilter: string;
begin
  Result := 'Package source files (*.kps)|*.kps|All files (*.*)|*.*';
end;

function TfrmPackageEditor.DoOpenFile: Boolean;
begin
  Result := True;
  pack.FileName := FileName;

  try
    pack.LoadXML;
  except
    on E:Exception do
    begin
      ShowMessage('Unable to load package file - opening in source editor: '+E.Message);
      pages.ActivePage := pageSource;
      frameSource.LoadFromFile(FileName);
      Exit;
    end;
  end;

  UpdateData;
  Result := True;
end;

procedure TfrmPackageEditor.FindError(const Filename: string; s: string);   // I4081
begin
  SetFocus;
end;

{-------------------------------------------------------------------------------
 - Info page                                                                   -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.editInfoNameChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['Name'] := Trim(editInfoName.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.editInfoVersionChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['Version'] := Trim(editInfoVersion.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.editInfoCopyrightChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['Copyright'] := Trim(editInfoCopyright.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.cmdInsertCopyrightClick(Sender: TObject);
begin
  Modified := True;
  editInfoCopyright.SelLength := 0;
  editInfoCopyright.SelText := '�';
end;

procedure TfrmPackageEditor.editInfoAuthorChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['Author'] := Trim(editInfoAuthor.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.editInfoEmailChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  if Trim(editInfoEmail.Text) = ''
    then pack.Info.URL['Author'] := ''
    else pack.Info.URL['Author'] := 'mailto:'+Trim(editInfoEmail.Text);
  Modified := True;
end;

procedure TfrmPackageEditor.editInfoWebSiteChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.Info.Desc['WebSite'] := Trim(editInfoWebSite.Text);
  pack.Info.URL['WebSite'] := Trim(editInfoWebSite.Text);
  Modified := True;
end;

{-------------------------------------------------------------------------------
 - Files page                                                                  -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.AddFile(FileName: WideString);
var
  f: TPackageContentFile;
  ki: TKeyboardInfo;
  j: Integer;
  viskbdname: WideString;
begin
  if pack.Files.FromFileNameEx(FileName) <> nil then Exit; // Already added

  f := TPackageContentFile.Create(pack);
  f.FileName := FileName;

  case f.FileType of
    ftFont:
      try
        with TTTInfo.Create(FileName, [tfNames]) do
        try
          f.Description := 'Font '+FullName;
        finally
          Free;
        end;
      except
        f.Description := 'Unreadable font ' + ChangeFileExt(ExtractFileName(FileName), '');
      end;
    ftKeymanFile:
      begin
        try
          GetKeyboardInfo(FileName, true, ki);
          try
            f.Description := 'Keyboard '+ki.KeyboardName;
            // Fill in some info stuff as well...
            if pack.Info.Desc[PackageInfo_Version] = '' then   // I4690
            begin
              pack.Info.Desc[PackageInfo_Version] := ki.KeyboardVersion;
            end;
            if pack.Info.Desc[PackageInfo_Name] = '' then
              pack.Info.Desc[PackageInfo_Name] := ki.KeyboardName;
            if (pack.Info.Desc[PackageInfo_Copyright] = '') and (ki.CopyrightString <> '') then
              pack.Info.Desc[PackageInfo_Copyright] := ki.CopyrightString;
            for j := 0 to pack.Info.Count - 1 do
              UpdateInfo(pack.Info[j].Name, pack.Info[j].Description, pack.Info[j].URL);
            if GetSystemStore(ki.MemoryDump.Memory, TSS_VISUALKEYBOARD, viskbdname) then
            begin
              viskbdname := ExpandFileNameEx(FileName, viskbdname);
              if FileExists(viskbdname) then
                AddFile(viskbdname);
            end;
          finally
            ki.MemoryDump.Free;
          end;
        except
          f.Description := 'Damaged keyboard '+ChangeFileExt(ExtractFileName(FileName), '');
        end;
      end;
    else
      f.Description := 'File '+ExtractFileName(FileName);
  end;

  pack.Files.Add(f);
  lbFiles.ItemIndex := lbFiles.Items.AddObject(ExtractFileName(FileName), f);
  lbFilesClick(lbFiles);
  UpdateReadme;
  UpdateImageFiles;
  UpdateStartMenuPrograms;
////  UpdateCustomisationFile;
  Modified := True;
end;

procedure TfrmPackageEditor.cmdAddFileClick(Sender: TObject);
var
  i, j: Integer;
  Found: Boolean;
begin
  with dlgFiles do
    if Execute then
      for i := 0 to Files.Count - 1 do
      begin
        Found := False;
        for j := 0 to pack.Files.Count - 1 do
          if LowerCase(pack.Files[j].FileName) = LowerCase(Files[i]) then
          begin
            Found := True;
            Break;
          end;
        if Found then Continue; // Don't add the file again

        AddFile(Files[i]);
      end;
  if lbFiles.CanFocus then lbFiles.SetFocus;  // I3100   // I3506
end;

procedure TfrmPackageEditor.cmdRemoveFileClick(Sender: TObject);
var
  n: Integer;
begin
  if lbFiles.ItemIndex > -1 then
  begin
    n := pack.Files.IndexOf(lbFiles.Items.Objects[lbFiles.ItemIndex] as TPackageContentFile);
    pack.Files.Delete(n);
    lbFiles.Items.Delete(lbFiles.ItemIndex);
    if lbFiles.Items.Count > 0 then lbFiles.ItemIndex := 0;
    lbFilesClick(lbFiles);
    UpdateReadme;
    UpdateImageFiles;
    UpdateStartMenuPrograms;
    Modified := True;
  end;
end;

{$EXTERNALSYM SHGetFileInfoW}
function SHGetFileInfoW(pszPath: PWideChar; dwFileAttributes: DWORD;
  var psfi: TSHFileInfoW; cbFileInfo, uFlags: UINT): DWORD; stdcall; external shell32;

procedure TfrmPackageEditor.lbFilesClick(Sender: TObject);
var
  e: Boolean;
  FIcon: TIcon;
  FFileInfo: TShFileInfoW;
  r: TRect;
begin
  Inc(FSetup);
  try
    e := lbFiles.ItemIndex >= 0;
    editFileType.Enabled := e;
    editFilePath.Enabled := e;
    memoFileDetails.Enabled := e;
    lblFileType.Enabled := e;
    lblFilePath.Enabled := e;
    lblFileDetails.Enabled := e;
    cmdRemoveFile.Enabled := e;
    cmdOpenFile.Enabled := e;
    cmdOpenContainingFolder.Enabled := e;

    imgFileIcon.Picture := nil;
    imgFileIcon.Visible := False;
    r := imgFileIcon.BoundsRect;
    InvalidateRect(imgFileIcon.Parent.Handle, @r, True);
    imgFileIcon.Parent.Update;
    imgFileIcon.Visible := True;
    if e then
      with lbFiles.Items.Objects[lbFiles.ItemIndex] as TPackageContentFile do
      begin
        FillChar(FFileInfo, SizeOf(TShFileInfoW), 0);
        if SHGetFileInfoW(PWideChar(FileName), 0, FFileInfo, SizeOf(TShFileInfoW), SHGFI_TYPENAME or SHGFI_ICON or SHGFI_SMALLICON) <> 0 then
        begin
          if FFileInfo.hIcon <> 0 then
          begin
            FIcon := TIcon.Create;
            try
              FIcon.ReleaseHandle;
              FIcon.Handle := FFileInfo.hIcon;
              imgFileIcon.Picture.Icon := FIcon;
              //DestroyIcon(FFileInfo.hIcon);
            finally
              FIcon.Free;
            end;
          end
          else
            imgFileIcon.Picture := nil;
          editFileType.Text := FFileInfo.szTypeName;
        end
        else
        begin
          imgFileIcon.Picture := nil;
          editFileType.Text := ExtractFileExt(lbFiles.Items[lbFiles.ItemIndex]) + ' File';
        end;

        editFilePath.Text := FileName;
        if FileType <> ftOther then
          editFileType.Text := FileTypeNames[FileType];

        if FileType = ftFont then
        try
          with TTTInfo.Create(FileName, [tfNames]) do
          try
            memoFileDetails.Text := 'Font Name: '+FullName+#13#10+
              'Copyright: '+Copyright;
          finally
            Free;
          end;
        except
          memoFileDetails.Text := 'Font details were not read successfully.';
        end
        else
        begin
          memoFileDetails.Text := '';
        end;
      end
    else
    begin
      memoFileDetails.Text := '';
      editFileType.Text := '';
      editFilePath.Text := '';
    end;
  finally
    Dec(FSetup);
  end;
end;

{-------------------------------------------------------------------------------
 - Options page                                                                -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.cbReadMeClick(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  if cbReadMe.ItemIndex <= 0
    then pack.Options.ReadmeFile := nil
    else pack.Options.ReadmeFile := cbReadMe.Items.Objects[cbReadMe.ItemIndex] as TPackageContentFile;
  Modified := True;
end;

procedure TfrmPackageEditor.editCmdLineChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  Modified := True;
end;

{-------------------------------------------------------------------------------
 - Start Menu page                                                             -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.EnableStartMenuControls;
var
  e: Boolean;
begin
  e := pack.StartMenu.DoCreate;
  chkStartMenuUninstall.Enabled := e;
  editStartMenuPath.Enabled := e;
  lblStartMenuEntries.Enabled := e;
  cmdNewStartMenuEntry.Enabled := e;
  lbStartMenuEntries.Enabled := e;
  e := e and (lbStartMenuEntries.ItemIndex >= 0);
  cmdDeleteStartMenuEntry.Enabled := e;
  lblStartMenuDescription.Enabled := e;
  editStartMenuDescription.Enabled := e;
  lblStartMenuProgram.Enabled := e;
  cbStartMenuProgram.Enabled := e;
  lblStartMenuParameters.Enabled := e;
  editStartMenuParameters.Enabled := e;
end;

procedure TfrmPackageEditor.chkCreateStartMenuClick(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.StartMenu.DoCreate := chkCreateStartMenu.Checked;
  EnableStartMenuControls;
  if chkCreateStartMenu.Checked then
  begin
    if editStartMenuPath.Text = '' then
      editStartMenuPath.Text := editInfoName.Text;
    editStartMenuPath.SetFocus;
  end
  else
    editStartMenuPath.Text := '';
  Modified := True;
end;

procedure TfrmPackageEditor.chkStartMenuUninstallClick(Sender: TObject);
begin
  pack.StartMenu.AddUninstallEntry := chkStartMenuUninstall.Checked;
  Modified := True;
end;

procedure TfrmPackageEditor.GetStartMenuEntries(var AName, AProg, AParams: WideString);
begin
  AName := '';
  AProg := '';
  AParams := '';
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  with lbStartMenuEntries.Items.Objects[lbStartMenuEntries.ItemIndex] as TPackageStartMenuEntry do
  begin
    AName := Name;
    AProg := Prog;
    AParams := Params;
  end;
end;

procedure TfrmPackageEditor.SetStartMenuEntries(AName, AProg, AParams: WideString);
begin
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  with lbStartMenuEntries.Items.Objects[lbStartMenuEntries.ItemIndex] as TPackageStartMenuEntry do
  begin
    Name := AName;
    Prog := AProg;
    Params := AParams;
  end;
end;

procedure TfrmPackageEditor.cmdUninstallClick(Sender: TObject);
begin
  DoAction(pfaUninstall);
end;

procedure TfrmPackageEditor.lbStartMenuEntriesClick(Sender: TObject);
var
  desc, prog, params: WideString;
begin
  Inc(FSetup);  // I1756
  GetStartMenuEntries(desc, prog, params);
  editStartMenuDescription.Text := desc;
  cbStartMenuProgram.Text := prog;
  editStartMenuParameters.Text := params;
  EnableStartMenuControls;
  Dec(FSetup);  // I1756
end;

procedure TfrmPackageEditor.pagesChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;

  if pages.ActivePage = pageSource then
    MovePackageToSource;

  FocusTab;
end;

procedure TfrmPackageEditor.pagesChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if FSetup > 0 then Exit;

  if pages.ActivePage = pageSource then
    AllowChange := MoveSourceToPackage
  else
    AllowChange := True;
end;

procedure TfrmPackageEditor.editStartMenuDescriptionChange(Sender: TObject);
var
  desc, prog, params: WideString;
begin
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  if FSetup = 0 then Modified := True;  // I1756
  GetStartMenuEntries(desc, prog, params);
  desc := editStartMenuDescription.Text;
  lbStartMenuEntries.Items[lbStartMenuEntries.ItemIndex] := desc;
  SetStartMenuEntries(desc, prog, params);
end;

procedure TfrmPackageEditor.cbStartMenuProgramChange(Sender: TObject);
var
  desc, prog, params: WideString;
begin
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  if FSetup = 0 then Modified := True;  // I1756
  GetStartMenuEntries(desc, prog, params);
  prog := cbStartMenuProgram.Text;
  SetStartMenuEntries(desc, prog, params);
end;

procedure TfrmPackageEditor.ChangeTab(FForward: Boolean);
begin
  pages.SelectNextPage(FForward);
end;

procedure TfrmPackageEditor.editStartMenuParametersChange(Sender: TObject);
var
  desc, prog, params: WideString;
begin
  if lbStartMenuEntries.ItemIndex < 0 then Exit;
  if FSetup = 0 then Modified := True;  // I1756
  GetStartMenuEntries(desc, prog, params);
  params := editStartMenuParameters.Text;
  SetStartMenuEntries(desc, prog, params);
end;

procedure TfrmPackageEditor.cmdNewStartMenuEntryClick(Sender: TObject);
var
  sme: TPackageStartMenuEntry;
begin
  Modified := True;  // I1756
  sme := TPackageStartMenuEntry.Create(pack);
  sme.Name := '(new)';
  pack.StartMenu.Entries.Add(sme);
  lbStartMenuEntries.ItemIndex := lbStartMenuEntries.Items.AddObject('(new)', sme);
  lbStartMenuEntriesClick(lbStartMenuEntries);
end;

procedure TfrmPackageEditor.cmdOpenContainingFolder2Click(Sender: TObject);
begin
  OpenContainingFolder(FileName);
end;

procedure TfrmPackageEditor.cmdOpenContainingFolderClick(Sender: TObject);
begin
  OpenContainingFolder((lbFiles.Items.Objects[lbFiles.ItemIndex] as TPackageContentFile).FileName);
end;

procedure TfrmPackageEditor.cmdOpenFileClick(Sender: TObject);   // I4687
var
  s: WideString;
  f: TProjectFile;
  fui: TProjectFileUI;
begin
  s := (lbFiles.Items.Objects[lbFiles.ItemIndex] as TPackageContentFile).FileName;
  f := FGlobalProject.FindFile(s);
  if Assigned(f) then
  begin
    fui := f.UI as TProjectFileUI;
    fui.DefaultEvent(Self);
  end
  else
  begin
    f := CreateProjectFile(s, ProjectFile);
    try
      fui := f.UI as TProjectFileUI;
      fui.DefaultEvent(Self);
    finally
      f.Free;
    end;
  end;
end;

procedure TfrmPackageEditor.cmdDeleteStartMenuEntryClick(Sender: TObject);
var
  n: Integer;
  sme: TPackageStartMenuEntry;
begin
  sme := lbStartMenuEntries.Items.Objects[lbStartMenuEntries.ItemIndex] as TPackageStartMenuEntry;
  pack.StartMenu.Entries.Delete(pack.StartMenu.Entries.IndexOf(sme));

  n := lbStartMenuEntries.ItemIndex;
  lbStartMenuEntries.Items.Delete(lbStartMenuEntries.ItemIndex);
  if n < lbStartMenuEntries.Items.Count
    then lbStartMenuEntries.ItemIndex := n
    else lbStartMenuEntries.ItemIndex := n - 1;

  lbStartMenuEntriesClick(lbStartMenuEntries);

  Modified := True;  // I1756
end;

{-------------------------------------------------------------------------------
 - Build page                                                                  -
 -------------------------------------------------------------------------------}

function TfrmPackageEditor.DoAction(action: TProjectFileAction): Boolean;
begin
  Result := (ProjectFile.UI as TProjectFileUI).DoAction(action, False);   // I4687   // I4702
end;

procedure TfrmPackageEditor.cmdBuildPackageClick(Sender: TObject);
begin
  if Untitled or Modified then
    ShowMessage('You must save the package before you can build it.')
  else
  begin
    frmMessages.Clear;
    DoAction(pfaCompile);
  end;
  UpdateOutPath;
end;

procedure TfrmPackageEditor.cmdInstallClick(Sender: TObject);
begin
  DoAction(pfaInstall);
end;

procedure TfrmPackageEditor.cmdInstallWithClick(Sender: TObject);
begin
  dlgOpenProductInstaller.FileName := pack.KPSOptions.MSIFileName;
  if dlgOpenProductInstaller.Execute then
  begin
    pack.KPSOptions.MSIFileName := dlgOpenProductInstaller.FileName;
    Modified := True;
    UpdateData;
  end;
end;

{-------------------------------------------------------------------------------
 - Display refresh routines                                                    -
 -------------------------------------------------------------------------------}

procedure TfrmPackageEditor.UpdateStartMenuPrograms;
begin
  cbStartMenuProgram.Clear;
  cbStartMenuProgram.Items.Assign(lbFiles.Items);
  cbStartMenuProgram.Items.Insert(0, '(Start Product)');
  cbStartMenuProgram.Items.Insert(1, '(Product Configuration)');
  cbStartMenuProgram.Items.Insert(2, '(Product Help)');
  cbStartMenuProgram.Items.Insert(3, '(About Product)');
end;

procedure TfrmPackageEditor.UpdateReadme;
begin
  FillFileList(cbReadme, pack.Options.ReadmeFile);
end;

procedure TfrmPackageEditor.FillFileList(combo: TComboBox; obj: TObject);
begin
  combo.Clear;
  combo.Items.Assign(lbFiles.Items);
  combo.Items.InsertObject(0, '(none)', nil);
  if Assigned(obj)
    then combo.ItemIndex := cbReadme.Items.IndexOfObject(obj)
    else combo.ItemIndex := 0;
end;

procedure TfrmPackageEditor.UpdateData;
var
  i: Integer;
begin
  Inc(FSetup);
  try
    lbFiles.Clear;

    for i := 0 to pack.Files.Count - 1 do
      lbFiles.Items.AddObject(ExtractFileName(pack.Files[i].FileName), pack.Files[i]);

    UpdateOutPath;
    UpdateReadme;
    UpdateImageFiles;
    UpdateStartMenuPrograms;

    for i := 0 to pack.Info.Count - 1 do
      UpdateInfo(pack.Info[i].Name, pack.Info[i].Description, pack.Info[i].URL);

    lbStartMenuEntries.Clear;
    
    for i := 0 to pack.StartMenu.Entries.Count - 1 do
      lbStartMenuEntries.Items.AddObject(pack.StartMenu.Entries[i].Name, pack.StartMenu.Entries[i]);

    chkCreateStartMenu.Checked := pack.StartMenu.DoCreate;
    chkStartMenuUninstall.Checked := pack.StartMenu.AddUninstallEntry;
    editStartMenuPath.Text := pack.StartMenu.Path;
    EnableStartMenuControls;

    editOutPath.Text := (ProjectFile as TkpsProjectFile).TargetFilename;   // I4688
    lblBootstrapMSI.Caption := pack.KPSOptions.MSIFileName;
    if pack.KPSOptions.MSIFileName = ''
      then editInstallerOutputFilename.Text := ''
      else editInstallerOutputFilename.Text := (ProjectFile as TkpsProjectFile).TargetInstallerFileName;   // I4688

    if lbFiles.Items.Count > 0 then lbFiles.ItemIndex := 0;
    lbFilesClick(lbFiles);

    UpdateImagePreviews;   // I4814
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.UpdateInfo(nm, desc, url: WideString);
var
  nmlc: WideString;
begin
  nmlc := LowerCase(nm);
  Inc(FSetup);
  try
    if nmlc = 'name' then editInfoName.Text := desc
    else if nmlc = 'copyright' then editInfoCopyright.Text := desc
    else if nmlc = 'version' then editInfoVersion.Text := desc                                    // less 'mailto:'
    else if nmlc = 'author' then begin editInfoAuthor.Text := desc; editInfoEmail.Text := Copy(url,8,1024); end
    else if nmlc = 'website' then editInfoWebSite.Text := desc
  finally
    Dec(FSetup);
  end;
end;

procedure TfrmPackageEditor.UpdateOutPath;
begin
  if Untitled then
  begin
    editOutPath.Text := '';
    cmdInstall.Enabled := False;
  end
  else
  begin
    editOutPath.Text := (ProjectFile as TkpsProjectFile).TargetFileName;   // I4688
    cmdInstall.Enabled := FileExists(editOutPath.Text);   // I4688
  end;
end;

procedure TfrmPackageEditor.editStartMenuPathChange(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  pack.StartMenu.Path := Trim(editStartMenuPath.Text);
  Modified := True;
end;


{-------------------------------------------------------------------------------
 - Package image page                                                          -
 -------------------------------------------------------------------------------}

{
  Columns:
     [TYPE] X, Y, [W,H -- not req.], [Caption|Graphic File], URL/Command
}

function TfrmPackageEditor.CanChangeTab(FForward: Boolean): Boolean;
begin
  Result := True;
end;

procedure TfrmPackageEditor.cbKMPImageFileClick(Sender: TObject);
begin
  if FSetup > 0 then Exit;
  if cbKMPImageFile.ItemIndex <= 0
    then pack.Options.GraphicFile := nil
    else pack.Options.GraphicFile := cbKMPImageFile.Items.Objects[cbKMPImageFile.ItemIndex] as TPackageContentFile;
  UpdateImagePreviews;
  Modified := True;
end;

procedure TfrmPackageEditor.UpdateImageFiles;
begin
  FillFileList(cbKMPImageFile, pack.Options.GraphicFile);
end;

procedure TfrmPackageEditor.UpdateImagePreviews;
var
  filename: WideString;
begin
  filename := '';

  if cbKMPImageFile.ItemIndex > 0 then
    filename := (cbKMPImageFile.Items.Objects[cbKMPImageFile.ItemIndex] as TPackageContentFile).FileName;

  if filename <> '' then
  begin
    try
      imgKMPSample.Picture.LoadFromFile(filename);
      lblKMPImageSize.Caption := Format('Image size: (%d x %d)',
        [imgKMPSample.Picture.Bitmap.Width, imgKMPSample.Picture.Bitmap.Height]);
    except
      imgKMPSample.Picture := nil;
      lblKMPImageSize.Caption := '(Unknown image format)';
    end;
  end
  else
  begin
    imgKMPSample.Picture := nil;
    lblKMPImageSize.Caption := '(No image)';
  end;
end;

procedure TfrmPackageEditor.MovePackageToSource;
begin
  frameSource.EditorText := FormatXMLData(pack.SaveXMLToText);
end;

function TfrmPackageEditor.MoveSourceToPackage: Boolean;
begin
  try
    pack.LoadXMLFromText(frameSource.EditorText);
    UpdateData;
    Result := True;
  except
    on E:Exception do
    begin
      ShowMessage('Unable to parse package source: '+E.Message);
      Result := False;
    end;
  end;
end;

procedure TfrmPackageEditor.CreateFromKMX(FFileName: WideString);
begin
  AddFile(FFileName);
end;


end.

