(*
  Name:             Keyman.Developer.UI.Project.ProjectUIFileType
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 May 2015

  Modified Date:    4 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    
*)
unit Keyman.Developer.UI.Project.ProjectUIFileType;   // I4687

interface

uses
  System.Classes,
  System.Contnrs,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.UI.Project.ProjectFileUI;

type
  TProjectFileUIType = class
  private
    FProjectFileUIClass: TProjectFileUIClass;
    FProjectFileClass: TProjectFileClass;
  public
    property ProjectFileClass: TProjectFileClass read FProjectFileClass write FProjectFileClass;
    property ProjectFileUIClass: TProjectFileUIClass read FProjectFileUIClass write FProjectFileUIClass;
  end;

  TProjectFileUITypeList = class(TObjectList)
  protected
    function Get(Index: Integer): TProjectFileUIType;
    procedure Put(Index: Integer; Item: TProjectFileUIType);
  public
    property Items[Index: Integer]: TProjectFileUIType read Get write Put; default;
    function IndexOf(Item: TProjectFileUIType): Integer;
    function Add(Item: TProjectFileUIType): Integer;
  end;

function CreateProjectFileUI(AProjectFile: TProjectFile): Boolean;
procedure RegisterProjectFileUIType(AProjectFileClass: TProjectFileClass; AProjectFileUIClass: TProjectFileUIClass);

implementation

uses
  System.SysUtils,

  Keyman.Developer.System.Project.Project,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType;

var
  FRegisteredFileUITypes: TProjectFileUITypeList = nil;

{ TProjectFileUITypeList }

function TProjectFileUITypeList.Add(Item: TProjectFileUIType): Integer;
begin
  Result := inherited Add(Item);
end;

function TProjectFileUITypeList.Get(Index: Integer): TProjectFileUIType;
begin
  Result := TProjectFileUIType(inherited Get(Index));
end;

function TProjectFileUITypeList.IndexOf(Item: TProjectFileUIType): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TProjectFileUITypeList.Put(Index: Integer; Item: TProjectFileUIType);
begin
  inherited Put(Index, Item);
end;

function CreateProjectFileUI(AProjectFile: TProjectFile): Boolean;
var
  i: Integer;
begin
  for i := 0 to FRegisteredFileUITypes.Count - 1 do
    if AProjectFile.ClassNameIs(FRegisteredFileUITypes[i].FProjectFileClass.ClassName) then
    begin
      AProjectFile.SetUI(FRegisteredFileUITypes[i].FProjectFileUIClass.Create(AProjectFile));
      Exit(True);
    end;

  for i := 0 to FRegisteredFileUITypes.Count - 1 do
    if FRegisteredFileUITypes[i].ProjectFileClass.ClassNameIs('TShellProjectFile') then
    begin
      AProjectFile.SetUI(FRegisteredFileUITypes[i].FProjectFileUIClass.Create(AProjectFile));
      Exit(True);
    end;

  Result := False;
end;

procedure RegisterProjectFileUIType(AProjectFileClass: TProjectFileClass; AProjectFileUIClass: TProjectFileUIClass);
var
  ft: TProjectFileUIType;
begin
  ft := TProjectFileUIType.Create;
  ft.ProjectFileClass := AProjectFileClass;
  ft.ProjectFileUIClass := AProjectFileUIClass;
  if not Assigned(FRegisteredFileUITypes) then
    FRegisteredFileUITypes := TProjectFileUITypeList.Create;
  FRegisteredFileUITypes.Add(ft);
end;

initialization
  FDoCreateProjectFileUI := CreateProjectFileUI;

finalization
  FreeAndNil(FRegisteredFileUITypes);
  FDoCreateProjectFileUI := nil;
end.
