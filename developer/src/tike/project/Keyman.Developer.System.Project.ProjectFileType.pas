(*
  Name:             Keyman.Developer.System.Project.ProjectFileType
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    4 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Initial version
                    23 Aug 2006 - mcdurdin - Stop top-level files being added multiple times
                    19 Mar 2007 - mcdurdin - I708 - Fix files disappearing from project when added as subfiles
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
*)
unit Keyman.Developer.System.Project.ProjectFileType;

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,

  Keyman.Developer.System.Project.ProjectFile;

type
  TProjectFileType = class
  private
    FExtension: string;
    FProjectFileClass: TProjectFileClass;
  public
    property Extension: string read FExtension write FExtension;
    property ProjectFileClass: TProjectFileClass read FProjectFileClass write FProjectFileClass;
  end;

  TProjectFileTypeList = class(TObjectList)
  protected
    function Get(Index: Integer): TProjectFileType;
    procedure Put(Index: Integer; Item: TProjectFileType);
  public
    property Items[Index: Integer]: TProjectFileType read Get write Put; default;
    function IndexOf(Item: TProjectFileType): Integer;
    function Add(Item: TProjectFileType): Integer;
  end;

function CreateProjectFile(AProject: TProject; AFileName: string; AParent: TProjectFile): TProjectFile;
procedure RegisterProjectFileType(AExtension: string; AProjectFileClass: TProjectFileClass);

type
  TDoCreateProjectFileUI = function(AProjectFile: TProjectFile): Boolean;   // I4687
var
  FDoCreateProjectFileUI: TDoCreateProjectFileUI = nil;


implementation

uses
  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFiles;

var
  FRegisteredFileTypes: TProjectFileTypeList = nil;
  FInit: Boolean = True;

function CreateProjectFile(AProject: TProject; AFileName: string; AParent: TProjectFile): TProjectFile;
var
  Ext: string;
  ni, i: Integer;
begin
  if not FInit then
  begin
    Result := TShellProjectFile.Create(AProject, AFileName, AParent);
    if Assigned(FDoCreateProjectFileUI) then
      FDoCreateProjectFileUI(Result);   // I4687
    Exit;
  end;
  Ext := LowerCase(ExtractFileExt(AFileName));

  { Do not allow top-level files to be added more than once }
  if not Assigned(AParent) then
  begin
    i := AProject.Files.IndexOfFileNameAndParent(AFileName, nil);
    if i >= 0 then
    begin
      Result := AProject.Files[i];
      Exit;
    end;
  end;

  ni := -1;
  for i := 0 to FRegisteredFileTypes.Count - 1 do
    if FRegisteredFileTypes[i].Extension = '*' then ni := i
    else if (FRegisteredFileTypes[i].Extension = Ext) and
      FRegisteredFileTypes[i].ProjectFileClass.IsFileTypeSupported(AFileName) then
    begin
      Result := FRegisteredFileTypes[i].ProjectFileClass.Create(AProject, AFileName, AParent);

      if Assigned(AParent) then
      begin
        if Assigned(AParent.Project) then
          AParent.Project.Files.Add(Result);
      end
      else
        AProject.Files.Add(Result);

      Exit;
    end;

  if ni = -1 then
    raise Exception.Create('Could not find appropriate TProjectFile for file '''+AFileName+'''.');
  Result := FRegisteredFileTypes[ni].ProjectFileClass.Create(AProject, AFileName, AParent);

  if Assigned(AParent) then
  begin
    if Assigned(AParent.Project) then
      AParent.Project.Files.Add(Result);
  end
  else
    AProject.Files.Add(Result);
end;

procedure RegisterProjectFileType(AExtension: string; AProjectFileClass: TProjectFileClass);
var
  ft: TProjectFileType;
begin
  if not Assigned(FRegisteredFileTypes) then
    FRegisteredFileTypes := TProjectFileTypeList.Create;

  // We can have ProjectFileAction classes that inherit from ProjectFile, so in those
  // circumstances we want to capture the bottom-most class registration and obviously
  // we don't want multiple classes registered for the same extension.
  for ft in FRegisteredFileTypes do
  begin
    if ft.Extension = AExtension then
    begin
      if AProjectFileClass.InheritsFrom(ft.ProjectFileClass) then
        ft.ProjectFileClass := AProjectFileClass;
      Exit;
    end;
  end;

  ft := TProjectFileType.Create;
  ft.Extension := AExtension;
  ft.ProjectFileClass := AProjectFileClass;
  FRegisteredFileTypes.Add(ft);
end;

{ TProjectFileTypeList }

function TProjectFileTypeList.Add(Item: TProjectFileType): Integer;
begin
  Result := inherited Add(Item);
end;

function TProjectFileTypeList.Get(Index: Integer): TProjectFileType;
begin
  Result := TProjectFileType(inherited Get(Index));
end;

function TProjectFileTypeList.IndexOf(Item: TProjectFileType): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TProjectFileTypeList.Put(Index: Integer; Item: TProjectFileType);
begin
  inherited Put(Index, Item);
end;

initialization
finalization
  FRegisteredFileTypes.Free;
  FInit := False;
end.
