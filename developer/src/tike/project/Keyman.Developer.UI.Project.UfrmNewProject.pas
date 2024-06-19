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
unit Keyman.Developer.UI.Project.UfrmNewProject;  // I3306   // I4796   // I4798

interface

uses
  System.Classes,
  System.ImageList,
  System.SysUtils,
  System.UITypes,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  utilfiletypes,
  UfrmTike;

type
  TfrmNewProject = class(TTikeForm)
    lvItems: TListView;
    cmdOK: TButton;
    cmdCancel: TButton;
    ilLarge: TImageList;
    ilSmall: TImageList;
    lblDescription: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lvItemsDblClick(Sender: TObject);
    procedure lvItemsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    procedure EnableControls;
    function GetProjectType: TKeymanProjectType;
    procedure UpdateDescription;
  protected
    function GetHelpTopic: string; override;
  public
    property ProjectType: TKeymanProjectType read GetProjectType;
  end;

function ShowNewProjectForm(Owner: TComponent): Boolean;

implementation

uses
  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.UI.Project.ProjectUI,
  Keyman.Developer.UI.Project.UfrmNewLDMLKeyboardProjectParameters,
  Keyman.Developer.UI.Project.UfrmNewProjectParameters,
  Keyman.Developer.UI.Project.UfrmNewModelProjectParameters,
  Keyman.Developer.UI.ImportWindowsKeyboardDialogManager,
  dmActionsMain,
  utilsystem;

function ShowNewProjectForm(Owner: TComponent): Boolean;
var
  p: TfrmNewProject;
  pt: TKeymanProjectType;
begin
  Result := False;

  p := TfrmNewProject.Create(Owner);
  try
    if p.ShowModal = mrCancel then
      Exit;
    pt := p.ProjectType;
  finally
    p.Free;
  end;

  case pt of
    kptUnknown:
      Assert(False, 'Should not be able to have a project type of unknown here');
    kptBasic:
      Result := ShowNewProjectParameters(Owner);
    kptLDMLKeyboard:
      Result := ShowNewLDMLKeyboardProjectParameters(Owner);
    kptWordlistLexicalModel:
      Result := ShowNewModelProjectParameters(Owner);
    kptImportWindowsKeyboard:
      Result := ShowImportWindowsKeyboard(Owner);
  end;
end;

{$R *.DFM}

procedure TfrmNewProject.EnableControls;
var
  e: Boolean;
begin
  e := Assigned(lvItems.Selected);
  cmdOK.Enabled := e;
end;

procedure TfrmNewProject.FormCreate(Sender: TObject);
begin
  inherited;
  lvItems.Selected := lvItems.Items[0];
  lvItems.ItemFocused := lvItems.Items[0];

  UpdateDescription;
  EnableControls;
end;

function TfrmNewProject.GetProjectType: TKeymanProjectType;
begin
  // Overly wordy but clarifies that we may have other types later
  if not Assigned(lvItems.Selected) then
    Result := kptUnknown
  else if lvItems.Selected.Caption = 'Keyman Keyboard' then
    Result := kptBasic
  else if lvItems.Selected.Caption = 'LDML Keyboard' then
    Result := kptLDMLKeyboard
  else if lvItems.Selected.Caption = 'Import Windows Keyboard' then
    Result := kptImportWindowsKeyboard
  else if lvItems.Selected.Caption = 'Wordlist Lexical Model' then
    Result := kptWordlistLexicalModel
  else
    Result := kptUnknown;
end;

function TfrmNewProject.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_NewProject;
end;

procedure TfrmNewProject.lvItemsDblClick(Sender: TObject);
begin
  if cmdOK.Enabled then
    cmdOK.Click;
end;

procedure TfrmNewProject.lvItemsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateDescription;
  EnableControls;
end;

procedure TfrmNewProject.UpdateDescription;
begin
  case ProjectType of
    kptUnknown: lblDescription.Caption := '';
    kptBasic: lblDescription.Caption :=
      'Creates a keyboard project that matches the folder structure used in '+
      'the Keyman keyboards repository and includes all basic files '+
      'needed for a keyboard project.';
    kptLDMLKeyboard: lblDescription.Caption :=
      'Creates a keyboard project that matches the folder structure used in '+
      'the Keyman keyboards repository and includes all basic files '+
      'needed for a LDML keyboard project.';
    kptImportWindowsKeyboard: lblDescription.Caption :=
      'Creates a new keyboard project, importing from a Windows system keyboard '+
      'and generating all the basic files needed for a keyboard project.';
    kptWordlistLexicalModel: lblDescription.Caption :=
      'Creates a new lexical model project that matches the folder structure used in '+
      'the Keyman lexical-models repository and includes all basic files '+
      'needed for a wordlist lexical model.';
  end;
end;

end.
