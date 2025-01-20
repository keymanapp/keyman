(*
  Name:             UfrmNew
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    10 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    23 Aug 2006 - mcdurdin - Include 'add to project' checkbox
                    23 Aug 2006 - mcdurdin - Refactor to encourage naming file on creation rather than later
                    14 Sep 2006 - mcdurdin - Fix selection of items in list view
                    28 Sep 2006 - mcdurdin - Require a non-blank file name for the target
                    04 Dec 2006 - mcdurdin - Add default ext
                    04 Dec 2006 - mcdurdin - Rework file types listed
                    04 Jan 2007 - mcdurdin - Hide branding and distribution if branding pack not activated
                    14 Jun 2008 - mcdurdin - I1420 - Save as .htm
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    24 Jul 2015 - mcdurdin - I4798 - Split Path and Name boxes in New File dialogs
                    03 Aug 2015 - mcdurdin - I4821 - Update icons for version 9.0 in New File dialog
                    10 Aug 2015 - mcdurdin - I4852 - Create directories when creating new file
*)
unit UfrmNew;  // I3306   // I4796   // I4798

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, utilfiletypes, Menus, shellapi, UfrmTike,
  System.ImageList;

type
  TfrmNew = class(TTikeForm)
    lvItems: TListView;
    cmdOK: TButton;
    cmdCancel: TButton;
    ilLarge: TImageList;
    ilSmall: TImageList;
    mnuItems: TPopupMenu;
    mnuShowIcons: TMenuItem;
    mnuShowDetails: TMenuItem;
    chkAddToProject: TCheckBox;
    editFileName: TEdit;
    lblFileName: TLabel;
    cmdBrowse: TButton;
    dlgSave: TSaveDialog;
    editPath: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure mnuShowIconsClick(Sender: TObject);
    procedure mnuShowDetailsClick(Sender: TObject);
    procedure editFileNameChange(Sender: TObject);
    procedure cmdBrowseClick(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure lvItemsDblClick(Sender: TObject);
    procedure lvItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FRootPath: string;
    FCanAddToProject: Boolean;
    function GetFileType: TKMFileType;
    procedure GetIcon(s: string; index: Integer);   // I4821
    procedure UpdateFileName;
    procedure EnableControls;
    function GetAddToProject: Boolean;
    function GetFileName: string;
    function GetDefaultExt: string;
    function CheckFilenameConventions: Boolean;
    function FileIsAppropriateForProject: Boolean;
    procedure SetCanAddToProject(const Value: Boolean);
  protected
    function GetHelpTopic: string; override;
  public
    property FileType: TKMFileType read GetFileType;
    property AddToProject: Boolean read GetAddToProject;
    property FileName: string read GetFileName;
    property CanAddToProject: Boolean read FCanAddToProject write SetCanAddToProject;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.System.KeyboardUtils,
  Keyman.System.LexicalModelUtils,
  KeymanDeveloperOptions,
  utilsystem;

{$R *.DFM}

procedure TfrmNew.cmdBrowseClick(Sender: TObject);
var
  FDefaultExt: string;
begin
  Assert(Assigned(lvItems.Selected));

  dlgSave.Filter := GetFileTypeFilter(FileType, FDefaultExt);
  FDefaultExt := lvItems.Selected.SubItems[0];

  if dlgSave.Filter = '' then
    dlgSave.Filter := lvItems.Selected.Caption + ' (*'+FDefaultExt+')|'+FDefaultExt;

  dlgSave.FileName := IncludeTrailingPathDelimiter(editPath.Text) + editFileName.Text;   // I4798
  dlgSave.DefaultExt := Copy(FDefaultExt, 2, 100);
  if dlgSave.Execute then
  begin
    editPath.Text := ExtractFileDir(dlgSave.FileName);   // I4798
    editFileName.Text := ExtractFileName(dlgSave.FileName);   // I4798
  end;
end;

function TfrmNew.CheckFilenameConventions: Boolean;
begin
  if not FGlobalProject.Options.CheckFilenameConventions then
    Exit(True);

  if FileType in [ftKeymanSource, ftPackageSource] then
  begin
    if TKeyboardUtils.DoesKeyboardFilenameFollowConventions(FileName) then
      Exit(True);

    Result := MessageDlg(Format(TKeyboardUtils.SKeyboardNameDoesNotFollowConventions_Prompt, [editFileName.Text]),
      mtConfirmation, mbOkCancel, 0) = mrOk;
  end
  else if FileType = ftModelSource then
  begin
    if TLexicalModelUtils.DoesTSFilenameFollowLexicalModelConventions(FileName) then
      Exit(True);

    Result := MessageDlg(Format(TLexicalModelUtils.SModelFileNameDoesNotFollowConventions_Message, [editFileName.Text]),
      mtConfirmation, mbOkCancel, 0) = mrOk;
  end
  else
  begin
    if TKeyboardUtils.DoesFilenameFollowConventions(FileName) then
      Exit(True);

    Result := MessageDlg(Format(TKeyboardUtils.SFilenameDoesNotFollowConventions_Prompt, [editFileName.Text]),
      mtConfirmation, mbOkCancel, 0) = mrOk;
  end;
end;

function TfrmNew.FileIsAppropriateForProject: Boolean;
begin
  if (GetFileType = ftModelSource) and (FGlobalProject.Options.ProjectType = ptKeyboard) then
  begin
    ShowMessage('You cannot add a lexical model to keyboard project.');
    Exit(False);
  end;

  if (GetFileType = ftKeymanSource) and (FGlobalProject.Options.ProjectType = ptLexicalModel) then
  begin
    ShowMessage('You cannot add a keyboard to a lexical model project.');
    Exit(False);
  end;

  Result := True;
end;

procedure TfrmNew.cmdOKClick(Sender: TObject);
begin
  if not CheckFilenameConventions then
    Exit;

  if not FileIsAppropriateForProject then
    Exit;

  if FileExists(FileName) then
  begin
    if MessageDlg('The file '+FileName+' already exists.  Overwrite it and create a new file?',
      mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;
  end
  else
  begin
    try
      ForceDirectories(editPath.Text);   // I4852
      TFileStream.Create(FileName, fmCreate).Free;
    except
      on E:Exception do
      begin
        ShowMessage('Unable to create the file "'+FileName+'".  Please check that the path name is correct and that no illegal characters are '+
          'used in the filename.  The error returned was: '+E.Message);
        Exit;
      end;
    end;
    DeleteFile(FileName);
  end;
  ModalResult := mrOk;
end;

procedure TfrmNew.editFileNameChange(Sender: TObject);
begin
  EnableControls;
end;

procedure TfrmNew.EnableControls;
var
  e: Boolean;
begin
  chkAddToProject.Visible := FCanAddToProject;
  e := Assigned(lvItems.Selected);
  editFileName.Enabled := e;
  cmdBrowse.Enabled := e;
  chkAddToProject.Enabled := e and
    (TLexicalModelUtils.ChangeFileExt(ExtractFileName(editFileName.Text),'') <> '');   // I4798
  cmdOK.Enabled := e and (GetFileName <> '');   // I4798
end;

procedure TfrmNew.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  inherited;
  FRootPath := FGlobalProject.ResolveSourcePath;

  lvItems.Selected := lvItems.Items[0];
  lvItems.ItemFocused := lvItems.Items[0];

  for i := 0 to lvItems.Items.Count - 1 do   // I4821
  begin
    GetIcon(lvItems.Items[i].SubItems[0], i);
  end;
//  GetIcon('.txt');
//  GetIcon('.html');
//  GetIcon('.htm');

  UpdateFileName;
  EnableControls;
end;

procedure TfrmNew.GetIcon(s: string; index: Integer);   // I4821
var
  shfi: TSHFileInfo;
  ico: TIcon;
  hico: HIcon;
begin
  { Load the HTML icon from the Shell }
  ico := TIcon.Create;
  hico := ico.ReleaseHandle;
  if hico <> 0 then DestroyIcon(hico);

  SHGetFileInfo(PChar(s), FILE_ATTRIBUTE_NORMAL, shfi, sizeof(TSHFileInfo), SHGFI_USEFILEATTRIBUTES or SHGFI_ICON or
    SHGFI_LARGEICON);
  ico.Handle := shfi.hIcon;
  lvItems.Items[index].ImageIndex := ilLarge.AddIcon(ico);   // I4821

  hico := ico.ReleaseHandle;
  if hico <> 0 then DestroyIcon(hico);

  SHGetFileInfo(PChar(s), FILE_ATTRIBUTE_NORMAL, shfi, sizeof(TSHFileInfo), SHGFI_USEFILEATTRIBUTES or SHGFI_ICON or
    SHGFI_SMALLICON);
  ico.Handle := shfi.hIcon;
  ilSmall.AddIcon(ico);

  ico.Free;


end;

function TfrmNew.GetAddToProject: Boolean;
begin
  Result := chkAddToProject.Checked;
end;

function TfrmNew.GetFileName: string;
var
  s: string;
begin
  if not editFileName.Enabled then   // I4798
    Exit('');

  if Pos('\', editFileName.Text) > 0 then   // I4798
    Exit('');

  s := TLexicalModelUtils.ExtractFileExt(editFileName.Text);
  if (CompareText(GetDefaultExt, '.html') = 0) or (CompareText(GetDefaultExt, '.htm') = 0) then
  begin  // I1420 - support .htm extension
    if (CompareText(s, '.htm') = 0) or (CompareText(s, '.html') = 0)
      then Result := IncludeTrailingPathDelimiter(editPath.Text) + editFileName.Text   // I4798
      else Result := IncludeTrailingPathDelimiter(editPath.Text) + TLexicalModelUtils.ChangeFileExt(editFileName.Text, GetDefaultExt);   // I4798
  end
  else
    Result := IncludeTrailingPathDelimiter(editPath.Text) + TLexicalModelUtils.ChangeFileExt(editFileName.Text, GetDefaultExt);   // I4798
end;

function TfrmNew.GetFileType: TKMFileType;
begin
  if not Assigned(lvItems.Selected) then
    Result := ftOther
  else if lvItems.Selected.Caption = 'Keyboard' then
    Result := ftKeymanSource
  else if lvItems.Selected.Caption = 'Package' then
    Result := ftPackageSource
  else if lvItems.Selected.Caption = 'Text' then
    Result := ftTextFile
  else if lvItems.Selected.Caption = 'XML' then
    Result := ftXMLFile
  else if lvItems.Selected.Caption = 'HTML' then
    Result := ftHTMLFile
  else if lvItems.Selected.Caption = 'HTM' then
    Result := ftHTMLFile
  else if lvItems.Selected.Caption = 'Model' then
    Result := ftModelSource
  else
    Result := ftOther;
end;

function TfrmNew.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_New;
end;

procedure TfrmNew.mnuShowIconsClick(Sender: TObject);
begin
  lvItems.ViewStyle := vsIcon;
end;

procedure TfrmNew.SetCanAddToProject(const Value: Boolean);
begin
  FCanAddToProject := Value;
  if not Value then
    chkAddToProject.Checked := False;
  EnableControls;
end;

procedure TfrmNew.UpdateFileName;
var
  FFileName: string;
begin
  if editFileName.Text <> '' then
  begin
    FRootPath := editPath.Text;   // I4798
    FFileName := TLexicalModelUtils.ChangeFileExt(ExtractFileName(editFileName.Text), '');
  end
  else
    FFileName := 'Untitled';
  editPath.Text := FRootPath;   // I4798
  editFileName.Text := FFileName + GetDefaultExt;   // I4798
  editFileName.SelStart := 0;   // I4798
  editFileName.SelLength := Length(FFileName);   // I4798
end;

function TfrmNew.GetDefaultExt: string;
begin
  if not Assigned(lvItems.Selected)
    then Result := ''
    else Result := lvItems.Selected.SubItems[0];
end;

procedure TfrmNew.mnuShowDetailsClick(Sender: TObject);
begin
  lvItems.ViewStyle := vsReport;
end;

procedure TfrmNew.lvItemsDblClick(Sender: TObject);
begin
  if cmdOK.Enabled then cmdOKClick(cmdOK)
end;

procedure TfrmNew.lvItemsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Assigned(Item) then
    UpdateFileName;
  EnableControls;
end;

end.
