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

function DoKCCompilePackage(FileName: string; AFullySilent, ASilent, AWarnAsError, AInstaller: Boolean; const AInstallerMSI: string; AUpdateInstaller: Boolean): Boolean;   // I4706

implementation

uses
  ProjectLog;

type
  TKCCompilePackage = class
    FSilent: Boolean;
    FFullySilent: Boolean;
    FHasWarning: Boolean;
    pack: TKPSFile;
    procedure SelfMessage(Sender: TObject; msg: string; State: TProjectLogState);
  end;

procedure TKCCompilePackage.SelfMessage(Sender: TObject; msg: string; State: TProjectLogState);   // I4706
begin
  case State of
    plsInfo: if not FSilent then writeln(ExtractFileName(pack.FileName) + ': ' + msg);
    plsWarning: begin FHasWarning := True; if not FFullySilent then writeln(ExtractFileName(pack.FileName) + ': warning: ' + msg); end;
    plsError: if not FFullySilent then writeln(ExtractFileName(pack.FileName) + ': error: ' + msg);
    plsFatal: if not FFullySilent then writeln(ExtractFileName(pack.FileName) + ': fatal: ' + msg);
  end;
end;

function DoKCCompilePackage(FileName: string; AFullySilent, ASilent, AWarnAsError, AInstaller: Boolean; const AInstallerMSI: string; AUpdateInstaller: Boolean): Boolean;   // I4706
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
      writeln(ExtractFileName(FileName) + ': fatal: Package file does not exist.');
      Exit;
    end;
    pack.FileName := FileName;
    pack.LoadXML;

    tcp := TKCCompilePackage.Create;
    try
      tcp.FFullySilent := AFullySilent;
      tcp.FSilent := ASilent;
      tcp.pack := pack;
      Result := DoCompilePackage(pack, tcp.SelfMessage, ASilent, ChangeFileExt(pack.FileName, '.kmp'));   // I4694
      if AWarnAsError and tcp.FHasWarning then Result := False;

      if AInstaller then
        Result := Result or DoCompilePackageInstaller(pack, tcp.SelfMessage, ASilent, AInstallerMSI,   // I4694
          ChangeFileExt(pack.FileName, '.exe'), AUpdateInstaller);
    finally
      tcp.Free;
    end;
  finally
    pack.Free;
  end;
end;

end.
