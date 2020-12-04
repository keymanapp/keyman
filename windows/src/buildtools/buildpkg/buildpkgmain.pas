(*
  Name:             buildpkgmain
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      23 Aug 2007

  Modified Date:    22 Jun 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          23 Aug 2007 - mcdurdin - Initial version
                    17 Sep 2007 - mcdurdin - Support output path parameter
                    30 Dec 2010 - mcdurdin - I2562 - EULA as part of setup bootstrapper
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls
                    23 Feb 2015 - mcdurdin - I4598 - V9.0 - Keyman installer does not show EULA when bundled with a keyboard
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    22 Jun 2015 - mcdurdin - I4763 - Package compiler (buildpkg) needs to specify username and password on command line
                    22 Jun 2015 - mcdurdin - I4764 - Buildpkg needs to support different paths for msi and kmp files

*)
unit buildpkgmain;  // I3306

interface

procedure Run;

implementation

uses
  Winapi.Windows,

  CompilePackageInstaller,
  kpsfile,
  Keyman.Developer.System.Project.ProjectLog,
  SysUtils;

type
  TParams = class
  private
    TooManyParams: Boolean;
  public
    OutputPath, RedistPath, MSIFileName, KMPFileName, KMPName, AppName, LicenseFileName, TitleImageFileName: WideString;  // I2562   // I4763
    StartDisabled, StartWithConfiguration: Boolean;
    constructor Create;
    function Validate: Boolean;
    function Completed: Boolean;

    procedure CompilerMessage(Sender: TObject; msg: string; State: TProjectLogState);   // I4706
  end;

procedure Run;
var
  Params: TParams;
  pack: TKPSFile;
begin
  Params := TParams.Create;

  if not Params.Completed then
  begin
    writeln('buildpkg.exe 1.0');
    writeln('Usage: '+ExtractFileName(ParamStr(0))+' -m <msi filename> [-l <License filename>] [-i <TitleImage filename>] [-a <app name>] (-s <setup.exe folder location>)|(-n <kmp description> <kmp filename>)');   // I2562   // I4763
    writeln('If setup-redist.exe is in setup folder location, it will be used in preference to setup.exe');
    ExitCode := 3;
    Exit;
  end;

  if not Params.Validate then
  begin
    writeln('buildpkg.exe 1.0');
    ExitCode := 4;
    Exit;
  end;

  if Params.KMPFileName <> '' then
  begin
    pack := TKPSFile.Create;
    pack.KPSOptions.MSIFileName := Params.MSIFileName;
    pack.FileName := ChangeFileExt(Params.KMPFileName, '.kps');
    pack.Info.Desc['Name'] := Params.KMPName;

    try
      if not DoCompilePackageInstaller(pack, Params.CompilerMessage, False, Params.MSIFileName,
          ChangeFileExt(Params.MSIFileName,'')+'-'+ChangeFileExt(ExtractFileName(Params.KMPFileName),'')+'.exe', Params.RedistPath,
          False, False, Params.LicenseFileName, Params.TitleImageFileName, Params.AppName,
          Params.StartDisabled, Params.StartWithConfiguration) then   // I4598   // I4694   // I4764
        ExitCode := 1
      else
        ExitCode := 0;
    except
      on E:Exception do
      begin
        writeln(E.Message);
        ExitCode := 2;
      end;
    end;
  end
  else
  begin
    try

      if Params.OutputPath = '' then
        Params.OutputPath := ChangeFileExt(Params.MSIFileName, '.exe');

      if not DoCompileMSIInstaller(Params.CompilerMessage, False, Params.MSIFileName, Params.OutputPath, Params.RedistPath,
        Params.LicenseFileName, Params.TitleImageFileName, Params.AppName,
        Params.StartDisabled, Params.StartWithConfiguration) then  // I2562
        ExitCode := 1
      else
        ExitCode := 0;
    except
      on E:Exception do
      begin
        writeln(E.Message);
        ExitCode := 2;
      end;
    end;
  end;
end;

{ TParams }

procedure TParams.CompilerMessage(Sender: TObject; msg: string;
  State: TProjectLogState);   // I4706
begin
  if State in [plsError, plsFatal]
    then raise Exception.Create(msg)
    else writeln(msg);
end;

function TParams.Completed: Boolean;
begin
  Result :=
    not TooManyParams and
    (MSIFileName <> '') and
    (
      ((KMPFileName = '') and (RedistPath <> '')) or
      ((KMPFileName <> '') and (KMPName <> ''))
    );
end;

constructor TParams.Create;
var
  i: Integer;
  Flag: WideString;
begin
  { Takes a .kmp file and turns it into a .msi installer }
  i := 1;
  while i <= ParamCount do
  begin
    Flag := ParamStr(i); Inc(i);
    if Flag = '-m' then
      MSIFileName := ParamStr(i)
    else if Flag = '-n' then
      KMPName := ParamStr(i)
    else if Flag = '-s' then
      RedistPath := IncludeTrailingPathDelimiter(ParamStr(i))
    else if Flag = '-o' then
      OutputPath := ParamStr(i)
    else if Flag = '-l' then  // I2562
      LicenseFileName := ParamStr(i)
    else if Flag = '-i' then
      TitleImageFileName := ParamStr(i)
    else if Flag = '-a' then
      AppName := ParamStr(i)
    else if SameText(Flag, '-startDisabled') then
    begin
      Dec(i); StartDisabled := True;
    end
    else if SameText(Flag, '-startWithConfiguration') then
    begin
      Dec(i); StartWithConfiguration := True;
    end
    else
    begin
      KMPFileName := Flag;
      Break;
    end;
    Inc(i);
  end;
  TooManyParams := i <= ParamCount;
end;

function TParams.Validate: Boolean;
begin
  Result := True;
  if not FileExists(MSIFileName) then
  begin
    Result := False;
    writeln('File '+MSIFileName+' does not exist.');
  end;
  if (KMPFileName <> '') and not FileExists(KMPFileName) then
  begin
    Result := False;
    writeln('File '+KMPFileName+' does not exist.');
  end;
  if (LicenseFileName <> '') and not FileExists(LicenseFileName) then  // I2562
  begin
    Result := False;
    writeln('License File '+LicenseFileName+' does not exist.');
  end;
  if (RedistPath <> '') and (not FileExists(RedistPath + 'setup.exe') or not FileExists(RedistPath + 'setup-redist.exe')) then
  begin
    Result := False;
    writeln('File '+RedistPath+'setup.exe and '+RedistPath+'setup-redist.exe do not exist.');
  end;
end;

end.
