(*
  Name:             kpsProjectFile
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    6 Jun 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add loading and saving from XML
                    23 Aug 2006 - mcdurdin - Refactor to use actions
                    23 Aug 2006 - mcdurdin - Save to XML only (load from XML or INI)
                    28 Sep 2006 - mcdurdin - Install package and uninstall package support
                    14 Dec 2006 - mcdurdin - Fix repeating sub files in project
                    16 May 2007 - mcdurdin - I791 - Unicode characters not rendering correctly in project
                    30 May 2007 - mcdurdin - I817 - Build package installer
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    19 Nov 2007 - mcdurdin - I1153 - Crash closing file (project)
                    27 Mar 2008 - mcdurdin - I1221 - Fixup files added to project unnecessarily
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    04 May 2015 - mcdurdin - I4692 - V9.0 - Add Clean as project action
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
                    05 May 2015 - mcdurdin - I4698 - V9.0 - Split project and user preferences files
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    06 Jun 2015 - mcdurdin - I4737 - Clean package should also clean installer
*)
unit kpsProjectFile;   // I4687   // I4688   // I4692

interface

uses
  System.SysUtils,
  Xml.XMLIntf,

  KPSFile,
  ProjectLog,
  ProjectFile,
  ProjectFiles,
  ProjectFileType;

type
  TkpsProjectFile = class(TOpenableProjectFile)
  private
    FHeader_Name: string;
    FHeader_Copyright: string;
    FHeader_Version: string;
    FMSIFileName: WideString;
    FWarnAsError: Boolean;   // I4706
    function GetOutputFilename: string;
    function GetTargetFilename: string;
    function GetTargetInstallerFilename: string;
    procedure SelfMessage(Sender: TObject; msg: string; State: TProjectLogState);   // I4706
  protected
    function GetRelativeOrder: Integer; override;
    procedure GetFileParameters; override;
  public
    procedure Load(node: IXMLNode; LoadState: Boolean); override;   // I4698
    procedure Save(node: IXMLNode; SaveState: Boolean); override;   // I4698

    function CompilePackageInstaller(APack: TKPSFile; FSilent: Boolean): Boolean;
    function CompilePackage(APack: TKPSFile; FSilent: Boolean): Boolean;
    function Clean: Boolean;

    property WarnAsError: Boolean read FWarnAsError write FWarnAsError;   // I4706

    property OutputFilename: string read GetOutputFilename;
    property TargetFilename: string read GetTargetFilename;
    property TargetInstallerFilename: string read GetTargetInstallerFilename;
    property Header_Name: string read FHeader_Name write FHeader_Name;
    property Header_Copyright: string read FHeader_Copyright write FHeader_Copyright;
    property Header_Version: string read FHeader_Version write FHeader_Version;
  end;

implementation

uses
  System.Variants,
  Winapi.ShellApi,
  Winapi.Windows,

  Project,

  CompilePackage,
  CompilePackageInstaller,
  PackageInfo,
  utilexecute;

{-------------------------------------------------------------------------------
 - TkmnProjectFile                                                             -
 -------------------------------------------------------------------------------}

function TkpsProjectFile.CompilePackage(APack: TKPSFile; FSilent: Boolean): Boolean;
var
  pack: TKPSFile;
begin
  HasCompileWarning := False;   // I4706
  if APack = nil then
  begin
    pack := TKPSFile.Create;
    pack.FileName := FileName;
    pack.LoadXML;
  end
  else
    pack := APack;

  try
    try
      Result := DoCompilePackage(pack, SelfMessage, FSilent, TargetFilename);
      if HasCompileWarning and WarnAsError then   // I4706
        Result := False;
    except
      on E:Exception do
      begin
        Log(plsError, E.Message);
        Result := False;
      end;
    end;

  finally
    if APack = nil then
      pack.Free;
  end;
end;

function TkpsProjectFile.CompilePackageInstaller(APack: TKPSFile; FSilent: Boolean): Boolean;
var
  pack: TKPSFile;
begin
  HasCompileWarning := False;   // I4706

  if APack = nil then
  begin
    pack := TKPSFile.Create;
    pack.FileName := FileName;
    pack.LoadXML;
  end
  else
    pack := APack;

  try
    try
      Result := DoCompilePackageInstaller(pack, SelfMessage, FSilent, '', TargetInstallerFilename, False);
      if HasCompileWarning and WarnAsError then   // I4706
        Result := False;
    except
      on E:Exception do
      begin
        Log(plsError, E.Message);
        Result := False;
      end;
    end;

  finally
    if APack = nil then
      pack.Free;
  end;
end;

procedure TkpsProjectFile.GetFileParameters;
var
  i: Integer;
  pack: TKPSFile;
begin
  if not FileExists(FileName) then Exit;
  if not Assigned(Project) then Exit;

  Project.Busy := True;
  Project.MustSave := False;
  try

    for i := Project.Files.Count - 1 downto 0 do
      if Project.Files[i].Parent = Self then // ParentFileID = ID then
        Project.Files.Delete(i);
  //  for i := FGlobalProject.Files.Count - 1 downto 0 do
    //  if FGlobalProject.Files[i].ParentFileID = ID then
      //  FGlobalProject.Files.Delete(i);

    pack := TKPSFile.Create;
    try
      pack.FileName := FileName;
      pack.LoadXML;
      for i := 0 to pack.Files.Count - 1 do
        if Project.Files.IndexOfFileName(pack.Files[i].FileName) < 0 then
          CreateProjectFile(pack.Files[i].FileName, Self);
      FHeader_Name := pack.Info.Desc[PackageInfo_Name];
      FHeader_Copyright := pack.Info.Desc[PackageInfo_Copyright];
      FHeader_Version := pack.Info.Desc[PackageInfo_Version];
      FMSIFileName := pack.KPSOptions.MSIFileName;
      SetFileVersion(FHeader_Version);
    finally
      pack.Free;
    end;
  finally
    Project.Busy := False;
    if Project.MustSave then
    begin
      Project.Save;
      Project.Refresh;
    end;
  end;
end;

function TkpsProjectFile.GetOutputFilename: string;
begin
  Result := ChangeFileExt(FileName, '.kmp');
end;

function TkpsProjectFile.GetRelativeOrder: Integer;
begin
  Result := 20;
end;

function TkpsProjectFile.GetTargetFilename: string;
begin
  Result := OwnerProject.GetTargetFilename(OutputFilename, Filename, FileVersion);
end;

function TkpsProjectFile.GetTargetInstallerFilename: string;
begin
  Result := OwnerProject.GetTargetFilename(ChangeFileExt(ExtractFileName(FMSIFileName),'') + '-' + ChangeFileExt(ExtractFileName(OutputFilename), '') + '.exe', Filename, FileVersion);
end;

procedure TkpsProjectFile.Load(node: IXMLNode; LoadState: Boolean);   // I4698
begin
  inherited Load(node, LoadState);

  if node.ChildNodes.IndexOf('Details') < 0 then Exit;
  node := node.ChildNodes['Details'];
  if node.ChildNodes.IndexOf('Name') >= 0 then FHeader_Name := VarToWideStr(node.ChildValues['Name']);
  if node.ChildNodes.IndexOf('Copyright') >= 0 then FHeader_Copyright := VarToWideStr(node.ChildValues['Copyright']);
  if node.ChildNodes.IndexOf('Version') >= 0 then FHeader_Version := VarToWideStr(node.ChildValues['Version']);
end;

procedure TkpsProjectFile.Save(node: IXMLNode; SaveState: Boolean);   // I4698
begin
  inherited Save(node, SaveState);
  node := node.AddChild('Details');
  if FHeader_Name <> '' then node.AddChild('Name').NodeValue := FHeader_Name;
  if FHeader_Copyright <> '' then node.AddChild('Copyright').NodeValue := FHeader_Copyright;
  if FHeader_Version <> '' then node.AddChild('Version').NodeValue := FHeader_Version;
end;

procedure TkpsProjectFile.SelfMessage(Sender: TObject; msg: string; State: TProjectLogState);   // I4706
begin
  if State = plsWarning then
    HasCompileWarning := True;
  Log(State, msg);
end;

function TkpsProjectFile.Clean: Boolean;
begin
  CleanFile(OutputFileName);
  CleanFile(TargetInstallerFilename);   // I4737
  Result := True;
end;

initialization
  RegisterProjectFileType('.kps', TkpsProjectFile);
end.

