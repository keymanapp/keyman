(*
  Name:             ProjectLoader
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
                    19 Mar 2007 - mcdurdin - I708 - Files disappearing from project
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    10 Jun 2014 - mcdurdin - I4212 - Crash opening project in read-only location [CrashID:tike.exe_9.0.443.0_005E202D_EOleException]
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
                    05 May 2015 - mcdurdin - I4698 - V9.0 - Split project and user preferences files
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile
                    
*)
unit ProjectLoader;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  Xml.XMLDoc,
  Xml.XMLIntf,

  ProjectFile,
  utilsystem;

type
  EProjectLoader = class(Exception);
  
  TProjectLoader = class
  private
    FFileName: string;
    FProject: TProject;
    procedure LoadUser;   // I4698
  public
    constructor Create(AProject: TProject; AFileName: string);
    procedure Execute;
  end;

implementation

uses
  System.Variants,
  System.Win.ComObj,

  kmnProjectFile,
  kpsProjectFile,
  ProjectFiles,
  ProjectFileType,
  utilhttp;

{ TProjectLoader }

constructor TProjectLoader.Create(AProject: TProject; AFileName: string);
begin
  inherited Create;
  FProject := AProject;
  FFileName := AFileName;
end;

procedure TProjectLoader.Execute;   // I4698
var
  n, i: Integer;
  doc: IXMLDocument;
  node, root: IXMLNode;
  pf: TProjectFile;
begin
  try
    doc := LoadXMLDocument(FFileName);
  except
    on E:Exception do
      raise EProjectLoader.Create('Error loading project file: '+E.Message);
  end;

  root := doc.DocumentElement;
  if root.NodeName <> 'KeymanDeveloperProject' then
    raise EProjectLoader.Create('Not a Keyman Developer project file');

  { Load options }

  node := root.ChildNodes.FindNode('Options');   // I4688
  if node <> nil then
  begin
    FProject.Options.BuildPath := VarToStr(node.ChildValues['BuildPath']);

    if VarIsNull(node.ChildValues['CompilerWarningsAsErrors'])   // I4865
      then FProject.Options.CompilerWarningsAsErrors := False
      else FProject.Options.CompilerWarningsAsErrors := node.ChildValues['CompilerWarningsAsErrors'];

    if VarIsNull(node.ChildValues['WarnDeprecatedCode'])   // I4866
      then FProject.Options.WarnDeprecatedCode := True
      else FProject.Options.WarnDeprecatedCode := node.ChildValues['WarnDeprecatedCode'];
  end;

  { Load root nodes first - I708 }

  for i := 0 to root.ChildNodes['Files'].ChildNodes.Count - 1 do
  begin
    node := root.ChildNodes['Files'].ChildNodes[i];
    if node.NodeName <> 'File' then Continue;
    if node.ChildNodes.FindNode('ParentFileID') = nil then //  ChildValues['ParentFileID'] then
    begin
      if not VarIsNull(node.ChildValues['Filepath']) then
      begin
        // I1152 - Avoid crashes when .kpj file is invalid
        pf := CreateProjectFile(FProject, ExpandFileNameClean(FFileName, node.ChildValues['Filepath']), nil);
        pf.Load(node, True);
      end;
    end;
  end;

  { Load child nodes }

  for i := 0 to root.ChildNodes['Files'].ChildNodes.Count - 1 do
  begin
    node := root.ChildNodes['Files'].ChildNodes[i];
    if node.NodeName <> 'File' then Continue;

    if node.ChildNodes.FindNode('ParentFileID') <> nil then //  ChildValues['ParentFileID'] then
    begin
      n := FProject.Files.IndexOfID(node.ChildValues['ParentFileID']);
      if n < 0 then Continue;
      pf := CreateProjectFile(FProject, ExpandFileNameClean(FFileName, node.ChildValues['Filepath']), FProject.Files[n]);
      pf.Load(node, True);
    end;
  end;

  { Load MRU from old project files }

  node := root.ChildNodes['MRU'];
  if Assigned(node) then
  begin
    for i := 0 to node.ChildNodes.Count - 1 do
    begin
      if node.ChildNodes[i].NodeName <> 'File' then Continue;
      FProject.MRU.Append(node.ChildNodes[i].ChildNodes['FullPath'].NodeValue);
    end;
  end;

  LoadUser;
end;

procedure TProjectLoader.LoadUser;   // I4698
var
  doc: IXMLDocument;
  i: Integer;
  node, root: IXMLNode;
  fsroot: IXMLNode;
  n: Integer;
  state: IXMLDocument;
  viewState: IXMLNode;
begin
  if not FileExists(FFileName + '.user') then Exit;

  doc := LoadXMLDocument(FFileName + '.user'); //TXMLDocument.Create(nil);

  root := doc.DocumentElement;
  if root.NodeName <> 'KeymanDeveloperProjectUser' then
    raise EProjectLoader.Create('Not a Keyman Developer project .user file');

  { Convert 9.0-era <state> node to <ViewState> }

  fsroot := root.ChildNodes.FindNode('state');
  if Assigned(fsroot) and (root.ChildNodes.FindNode('ViewState') = nil) then
  begin
    state := LoadXMLData(fsroot.NodeValue);
    viewState := root.AddChild('ViewState');
    for i := 0 to state.DocumentElement.ChildNodes.Count - 1 do
      viewState.ChildNodes.Add(state.DocumentElement.ChildNodes[i].CloneNode(True));

    try
      doc.SaveToFile(FFileName + '.user');
    except
      on E:EOleException do   // I4212
      begin
        ;
      end;
    end;
  end;

  { Load root nodes first - I708 }

  fsroot := root.ChildNodes.FindNode('FileStates');
  if Assigned(fsroot) then
  begin
    for i := 0 to fsroot.ChildNodes.Count - 1 do
    begin
      node := fsroot.ChildNodes[i];
      if node.NodeName <> 'FileState' then Continue;
      if node.ChildNodes.FindNode('ID') = nil then Continue;
      n := FProject.Files.IndexOfID(node.ChildValues['ID']);
      if n >= 0 then
        FProject.Files[n].LoadState(node);
    end;
  end;

  node := root.ChildNodes['MRU'];
  if Assigned(node) then
  begin
    for i := 0 to node.ChildNodes.Count - 1 do
    begin
      if node.ChildNodes[i].NodeName <> 'File' then Continue;
      FProject.MRU.Append(node.ChildNodes[i].ChildNodes['FullPath'].NodeValue);
    end;
  end;

end;

end.
