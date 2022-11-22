(*
  Name:             kccompilepackage
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    11 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - LoadIni instead of Load
                    30 Apr 2007 - mcdurdin - Load from XML instead of from INI for compiling packages
                    30 May 2007 - mcdurdin - I817 - Build bootstrap installer
                    04 Jun 2007 - mcdurdin - Remove KeymanPath reference, Add AInstallerMSI, AUpdate
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
*)
unit kccompilepackage;

interface

uses
  Windows,
  SysUtils,
  CompilePackage,
  CompilePackageInstaller,
  PackageInfo,
  kpsfile;

function DoKCCompilePackage(FileName: string; AWarnAsError, ACheckFilenameConventions, AInstaller: Boolean;
  const AInstallerMSI: string; AUpdateInstaller: Boolean; const ASchemaPath: string): Boolean;   // I4706

implementation

uses
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.System.Project.ProjectLogConsole,
  Keyman.Developer.System.ValidateKpsFile;

type
  TKCCompilePackage = class
    pack: TKPSFile;
    procedure SelfMessage(Sender: TObject; msg: string; State: TProjectLogState);
  end;

procedure TKCCompilePackage.SelfMessage(Sender: TObject; msg: string; State: TProjectLogState);   // I4706
begin
  TProjectLogConsole.Instance.Log(State, pack.Filename, Msg, 0, 0);
end;

function DoKCCompilePackage(FileName: string; AWarnAsError, ACheckFilenameConventions, AInstaller: Boolean;
  const AInstallerMSI: string; AUpdateInstaller: Boolean; const ASchemaPath: string): Boolean;   // I4706
var
  tcp: TKCCompilePackage;
  pack: TKPSFile;
  buf: array[0..260] of char;
  pbuf: PChar;
begin
  Result := False;
  pack := TKPSFile.Create;
  try
    GetFullPathName(PChar(FileName), 260, buf, pbuf); FileName := buf;
    if not FileExists(FileName) then
    begin
      TProjectLogConsole.Instance.Log(plsFatal, FileName, 'Package file does not exist', 0, 0);
      Exit;
    end;

    if (ASchemaPath <> '') and (FileExists(ASchemaPath + 'kps.xsd')) then
    begin
      if not TValidateKpsFile.Execute(FileName, ASchemaPath + 'kps.xsd',
        TProjectLogConsole.Instance.Log) then
      begin
        TProjectLogConsole.Instance.Log(plsFailure, FileName, 'Package '+FileName+' had validation errors.', 0, 0);
        Exit;
      end;
    end;

    pack.FileName := FileName;
    pack.LoadXML;

    tcp := TKCCompilePackage.Create;
    try
      tcp.pack := pack;
      Result := DoCompilePackage(pack, tcp.SelfMessage, False, ACheckFilenameConventions, ChangeFileExt(pack.FileName, '.kmp'));   // I4694
      if AWarnAsError and TProjectLogConsole.Instance.HasWarning then Result := False;

      if AInstaller and Result then
      begin
        Result := DoCompilePackageInstaller(pack, tcp.SelfMessage, False, AInstallerMSI,   // I4694
          ChangeFileExt(pack.FileName, '.exe'), '', AUpdateInstaller, True, '', '', '', False, False);
        if AWarnAsError and TProjectLogConsole.Instance.HasWarning then Result := False;
      end;

      if Result
        then TProjectLogConsole.Instance.Log(plsSuccess, FileName, 'Package '+FileName+' compiled successfully.', 0, 0)
        else TProjectLogConsole.Instance.Log(plsFailure, FileName, 'Package '+FileName+' could not be compiled.', 0, 0);
    finally
      tcp.Free;
    end;
  finally
    pack.Free;
  end;
end;

end.
