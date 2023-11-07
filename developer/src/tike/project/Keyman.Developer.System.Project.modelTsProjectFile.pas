(*
  Name:             Keyman.Developer.System.Project.tsProjectFile
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
  History:
*)
unit Keyman.Developer.System.Project.modelTsProjectFile;  // I3306   // I4687   // I4688   // I4692

interface

uses
  System.SysUtils,
  Xml.XMLIntf,

  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType,
  UKeymanTargets;

type
  TmodelTsProjectFile = class;

  TmodelTsProjectFile = class(TOpenableProjectFile)
  private
    FDebug: Boolean;
    FTestKeyboard: string;   // I4706

    function GetTargetFilename: string;
  protected
    function GetRelativeOrder: Integer; override;
    procedure GetFileParameters; override;

    property IsDebug: Boolean read FDebug;
  public
    function IsCompilable: Boolean; override;
    procedure LoadState(node: IXMLNode); override;   // I4698
    procedure SaveState(node: IXMLNode); override;   // I4698

    property Debug: Boolean read FDebug write FDebug;
    property TestKeyboard: string read FTestKeyboard write FTestKeyboard;

    property TargetFilename: string read GetTargetFilename;
  end;

implementation

uses
  System.Classes,
  System.Variants,
  Winapi.Windows,

  utilsystem;

{-------------------------------------------------------------------------------
 - TmodelTsProjectFile                                                             -
 -------------------------------------------------------------------------------}

procedure TmodelTsProjectFile.SaveState(node: IXMLNode);   // I4698
begin
  inherited SaveState(node);
  node.AddChild('Debug').NodeValue := FDebug;
  node.AddChild('TestKeyboard').NodeValue := FTestKeyboard;
end;

procedure TmodelTsProjectFile.LoadState(node: IXMLNode);   // I4698
begin
  inherited LoadState(node);
  try
    if (node.ChildNodes.IndexOf('Debug') >= 0) and not VarIsNull(node.ChildValues['Debug'])
      then FDebug := node.ChildValues['Debug']
      else FDebug := False;
    if (node.ChildNodes.IndexOf('TestKeyboard') >= 0) and not VarIsNull(node.ChildValues['TestKeyboard'])
      then FTestKeyboard := node.ChildValues['TestKeyboard']
      else FTestKeyboard := '';
  except
    FDebug := False;
    FTestKeyboard := '';
  end;
end;

function TmodelTsProjectFile.GetRelativeOrder: Integer;
begin
  Result := 20;
end;

function TmodelTsProjectFile.GetTargetFilename: string;
var
  OutputFileName, FTempFileVersion: string;
begin
  OutputFileName := ChangeFileExt(FileName, '.js');
  // https://github.com/keymanapp/keyman/issues/631
  // This appears to be a Delphi compiler bug (RSP-20457)
  // Workaround is to make a copy of the parameter locally
  // which fixes the reference counting.
  FTempFileVersion := FileVersion;
  Result := OwnerProject.GetTargetFilename(OutputFileName, FileName, FTempFileVersion);
end;

function TmodelTsProjectFile.IsCompilable: Boolean;
begin
  Result := True;
end;

procedure TmodelTsProjectFile.GetFileParameters;
begin
  SetFileVersion('1.0');   // I4701
  // TODO: Consider adding external references, e.g. model.tsv etc
end;

initialization
  RegisterProjectFileType('.model.ts', TmodelTsProjectFile);
end.

