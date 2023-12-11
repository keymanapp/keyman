(*
  Name:             Keyman.Developer.System.Project.ProjectSaver
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Add MRU IDs for right-click options
                    04 Jan 2007 - mcdurdin - Add activated module information to project
                    30 May 2007 - mcdurdin - I762 - Fixed crash saving project files when folder deleted
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    26 Jul 2010 - mcdurdin - I2467 - 8.0 renumber
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
                    05 May 2015 - mcdurdin - I4698 - V9.0 - Split project and user preferences files
                    06 May 2015 - mcdurdin - I4700 - V9.0 - <Modules> section should be in project .user file
                    06 May 2015 - mcdurdin - I4704 - V9.0 - Project files should be formatted XML
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project

*)
unit Keyman.Developer.System.Project.ProjectSaver;  // I3306

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  Xml.XMLDoc,
  Xml.XMLIntf,

  Keyman.Developer.System.Project.ProjectFile,
  utilsystem;

type
  EProjectSaver = class(Exception);

  TProjectSaver = class
  private
    FFileName: string;
    FProject: TProject;
    FXML: string;
    FXMLUser: string;
  public
    constructor Create(AProject: TProject; AFileName: string);
    procedure Execute;
    procedure SaveUser;   // I4698
    property XML: string read FXML;
    property XMLUser: string read FXMLUser;
  end;

implementation

uses
  System.Win.ComObj,

  Keyman.Developer.System.Project.ProjectFiles,
  utilfiletypes;

{ TProjectSaver }

constructor TProjectSaver.Create(AProject: TProject; AFileName: string);
begin
  inherited Create;
  FProject := AProject;
  FFileName := AFileName;
end;

procedure TProjectSaver.Execute;   // I4698
var
  i: Integer;
  doc: IXMLDocument;
  filenode, node, root: IXMLNode;
  defopts: TProjectOptionsRecord;
begin
  defopts := DefaultProjectOptions[FProject.Options.Version];

  doc := NewXMLDocument();
  doc.Options := doc.Options + [doNodeAutoIndent];   // I4704
  doc.Encoding := 'utf-8';

  root := doc.CreateElement('KeymanDeveloperProject', '');
  doc.DocumentElement := root;

  // options

  node := root.AddChild('Options');   // I4688
  if FProject.Options.Version = pv20 then
    // Only v2.0 projects have a version number
    node.AddChild('Version').NodeValue := ProjectVersionToString(FProject.Options.Version);

  if FProject.Options.BuildPath <> defopts.BuildPath then
    node.AddChild('BuildPath').NodeValue := FProject.Options.BuildPath;

  if FProject.Options.SourcePath <> defopts.SourcePath then
    node.AddChild('SourcePath').NodeValue := FProject.Options.SourcePath;

  if FProject.Options.CompilerWarningsAsErrors <> defopts.CompilerWarningsAsErrors then
    node.AddChild('CompilerWarningsAsErrors').NodeValue := FProject.Options.CompilerWarningsAsErrors;   // I4866

  if FProject.Options.WarnDeprecatedCode <> defopts.WarnDeprecatedCode then
    node.AddChild('WarnDeprecatedCode').NodeValue := FProject.Options.WarnDeprecatedCode;   // I4865

  if FProject.Options.CheckFilenameConventions <> defopts.CheckFilenameConventions then
    node.AddChild('CheckFilenameConventions').NodeValue := FProject.Options.CheckFilenameConventions;   // I4866

  if FProject.Options.SkipMetadataFiles <> defopts.SkipMetadataFiles then
    node.AddChild('SkipMetadataFiles').NodeValue := FProject.Options.SkipMetadataFiles;
    
  if FProject.Options.ProjectType <> defopts.ProjectType then
    node.AddChild('ProjectType').NodeValue := ProjectTypeToString(FProject.Options.ProjectType);

  // files

  if (FProject.Options.Version = pv10) or (FFileName = '') then
  begin
    node := root.AddChild('Files');
    for i := 0 to FProject.Files.Count - 1 do
    begin
      filenode := node.AddChild('File');

      // For xsl renderer, we have additional metadata we provide for v2.0
      // projects, at least until we replace the project view with a tree
      // structure
      if (FFileName = '') then
      begin
        if FProject.Files[i].IsSourceFile
          then filenode.AddChild('IsInSourcePath').NodeValue := 'true'
          else filenode.AddChild('IsInSourcePath').NodeValue := 'false';
        if FProject.Files[i].IsCompilable
          then filenode.AddChild('IsCompilable').NodeValue := 'true'
          else filenode.AddChild('IsCompilable').NodeValue := 'false';
      end;

      FProject.Files[i].Save(filenode);
    end;
  end;

  if FFileName <> '' then
  begin
    doc.SaveToFile(FFileName);
  end
  else
    doc.SaveToXML(FXML);

  SaveUser;
end;

procedure TProjectSaver.SaveUser;   // I4698
var
  i: Integer;
  doc: IXMLDocument;
  node, root: IXMLNode;
  viewState: IXMLNode;
  state: IXMLDocument;
begin
  // Save user options

  doc := NewXMLDocument();
  doc.Options := doc.Options + [doNodeAutoIndent];   // I4704
  doc.Encoding := 'utf-8';

  root := doc.CreateElement('KeymanDeveloperProjectUser', '');
  doc.DocumentElement := root;

  if FProject.DisplayState <> '' then
  begin
    state := LoadXMLData(FProject.DisplayState);
    viewState := root.AddChild('ViewState');
    for i := 0 to state.DocumentElement.ChildNodes.Count - 1 do
      viewState.ChildNodes.Add(state.DocumentElement.ChildNodes[i].CloneNode(True));
  end;

  // file states

  node := root.AddChild('FileStates');
  for i := 0 to FProject.Files.Count - 1 do
    FProject.Files[i].SaveState(node.AddChild('FileState'));

  // mru

  node := root.AddChild('MRU');
  for i := 0 to FProject.MRU.FileCount - 1 do
  begin
    with node.AddChild('File') do
    begin
      AddChild('ID').NodeValue := 'id_MRU'+IntToStr(i);
      AddChild('Filename').NodeValue := ExtractFileName(FProject.MRU.Files[i]);
      AddChild('FileType').NodeValue := ExtractFileExt(FProject.MRU.Files[i]);
      AddChild('FullPath').NodeValue := FProject.MRU.Files[i];
    end;
  end;

  if FFileName <> '' then
  begin
    doc.SaveToFile(ChangeFileExt(FFileName, Ext_ProjectSourceUser));
  end
  else
    doc.SaveToXML(FXMLUser);
end;

end.
