(*
  Name:             bootstrapmain
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jun 2007

  Modified Date:    12 Aug 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jun 2007 - mcdurdin - Initial version
                    05 Jun 2007 - mcdurdin - I817 - Fix -x option
                    19 Jun 2007 - mcdurdin - I817 - Translate to Unicode and remove Forms dependence
                    23 Aug 2007 - mcdurdin - Initial version from Desktop
                    17 Sep 2007 - mcdurdin - I1071 - Add -o option to work offline (no online update checks, no downloads of redists)
                    31 Mar 2011 - mcdurdin - I2849 - Developer online update check fails because it looks for a patch update
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    15 Jun 2012 - mcdurdin - I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    03 Nov 2012 - mcdurdin - I3500 - V9.0 - Merge of I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    12 Aug 2014 - mcdurdin - I4366 - V9.0 - Installer still doesn't enforce Win7 or later
*)
unit bootstrapmain;  // I3306

interface

uses
  Classes,
  SysUtils,
  SetupStrings,
  WideStrings;

type
  EInstallInfo = class(Exception);


  TInstallInfo = class
  private
    FEditionTitle: WideString;
    FMSIFileName: WideString;
    FVersion: WideString;
    FStrings: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    function Text(const Name: TInstallInfoText): WideString; overload;
    function Text(const Name: TInstallInfoText; const Args: array of const): WideString; overload;
    property Strings: TStrings read FStrings;
    property EditionTitle: WideString read FEditionTitle;
    property MSIFileName: WideString read FMSIFileName;
    property Version: WideString read FVersion write FVersion;
  end;

var
  FInstallInfo: TInstallInfo = nil;

procedure Run;

var
  ExtPath: string = '';

implementation

uses
  TntDialogHelp,
  TypInfo,
  UfrmRun,
  ActiveX,
  GetOsVersion,
  SetupForm,
  CommonControls,
  Unicode,
  SFX,
  //dialogs,
  Windows;

const
  ICC_PROGRESS_CLASS     = $00000020; // progress

var
  TempPath: string = '';

procedure DoExtractOnly(FSilent: Boolean; const FExtractOnly_Path: string); forward;

procedure CreateTempDir;
var
  buf: array[0..260] of WideChar;
begin
  GetTempPath(MAX_PATH-1, buf);
  ExtPath := ExcludeTrailingPathDelimiter(buf);  // I3310
  GetTempFileName(PWideChar(ExtPath), 'kmt', 0, buf);  // I3310
  ExtPath := buf;  // I3310
  if FileExists(PWideChar(ExtPath)) then SysUtils.DeleteFile(ExtPath);  // I331  // I33100
  SysUtils.CreateDir(ExtPath);
  TempPath := ExtPath;
end;

procedure DeletePath(const path: WideString);
var
  fd: TWin32FindData;
  n: DWord;
begin
  n := FindFirstFile(PWideChar(path + '\*.*'), fd);
  if n = INVALID_HANDLE_VALUE then Exit;
  repeat
    DeleteFile(PWideChar(path + '\' + fd.cFileName));
  until not FindNextFile(n, fd);
  FindClose(n);
  RemoveDirectory(PWideChar(path));
end;

procedure RemoveTempDir;
begin
  if TempPath <> '' then
    DeletePath(TempPath);  // I3310
end;

procedure ProcessCommandLine(var FPromptForReboot, FSilent, FOffline, FExtractOnly, FContinueSetup: Boolean; var FExtractPath: WideString);  // I3355   // I3500
var
  i: Integer;
begin
  FPromptForReboot := True;  // I3355   // I3500
  FSilent := False;
  FOffline := False;
  FExtractOnly := False;
  FContinueSetup := False;
  i := 1;
  while i <= ParamCount do
  begin
    if WideSameText(ParamStr(i), '-c') then
      FContinueSetup := True
    else if WideSameText(ParamStr(i), '-au') then // I2849
    begin
      FSilent := True;
      FOffline := True;
    end
    else if WideSameText(ParamStr(i), '-s') then
    begin
      FPromptForReboot := False;  // I3355   // I3500
      FSilent := True;
    end
    else if WideSameText(ParamStr(i), '-o') then
      FOffline := True
    else if WideSameText(ParamStr(i), '-x') then
    begin
      Inc(i);
      FExtractOnly := True;
      FExtractPath := ParamStr(i);
    end;
    Inc(i);
  end;
end;

procedure LogError(const s: WideString);
begin
  ShowMessageW(s);
end;

procedure Run;
var
  FExtractOnly: Boolean;
  FContinueSetup: Boolean;
  FPromptForReboot: Boolean;  // I3355   // I3500
  FSilent: Boolean;
  FOffline: Boolean;
  FExtractOnly_Path: WideString;
BEGIN
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    try

    {if GetOS in [osLegacy, osVista] then   // I4366
    begin
      ShowMessageW(STR_E_OLDOSVERSION);
      Exit;
    end;}

    FInstallInfo := TInstallInfo.Create;

    try
      CreateTempDir;
      try
        InitCommonControl(ICC_PROGRESS_CLASS);

        { Display the dialog }
        //Cancel_From_InfoWin := False;

        ProcessCommandLine(FPromptForReboot, FSilent, FOffline, FExtractOnly, FContinueSetup, FExtractOnly_Path);  // I3355   // I3500

        if FExtractOnly then
        begin
          DoExtractOnly(FSilent, FExtractOnly_Path);
          Exit;
        end;

        if not ProcessArchive(ExtPath) then
        begin
          { The files must be in the current directory.  Use them instead }
          ExtPath := ExtractFilePath(ParamStr(0));  // I3310
        end;

        ExtPath := IncludeTrailingPathDelimiter(ExtPath);  // I3310

        if ParamStr(1) <> '--test-dialog' then
        begin
          if not SysUtils.FileExists(ExtPath + 'setup.inf') then  // I3310
          begin
            LogError('The file "setup.inf" is missing.  Setup cannot continue.');
            Exit;
          end;

          FInstallInfo.Load;
        end;

        with TfrmRun.Create(nil) do
        try
          ContinueSetup := FContinueSetup;
          Offline := FOffline;
          if FSilent
            then DoInstall(True, FPromptForReboot)  // I3355   // I3500
            else ShowModal;
        finally
          Free;
        end;

      finally
        RemoveTempDir;
      end;
    finally
      ExitCode := 1;
      FInstallInfo.Free;
    end;
    except
      on e:Exception do ShowMessageW(e.message);
    end;
  finally
    CoUninitialize;
  end;
end;

{ TInstallInfo }

constructor TInstallInfo.Create;
begin
  inherited Create;
  FStrings := TStringList.Create;
end;

destructor TInstallInfo.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

function TInstallInfo.Text(const Name: TInstallInfoText): WideString;
var
  s: WideString;
begin
  s := GetEnumName(TypeInfo(TInstallInfoText), Ord(Name));
  Result := FStrings.Values[s];
  if Result = '' then
    Result := FDefaultStrings[Name];
  //ssWelcome_Keyboards
end;

procedure TInstallInfo.Load;
var
  i: Integer;
  FInSetup: Boolean;
  FInStrings: Boolean;
  val, nm: WideString;
  s: WideString;
begin
  FInSetup := False;
  FInStrings := False;
  with TStringList.Create do
  try
    LoadFromFile(ExtPath + 'setup.inf');  // we'll use the preamble encoding  // I3337

    for i := 0 to Count - 1 do
    begin
      s := Trim(Strings[i]);
      if s = '' then Continue;

      if Copy(s, 1, 1) = '[' then
      begin
        FInSetup := WideSameText(s, '[Setup]');
        FInStrings := WideSameText(s, '[Strings]');
      end
      else if FInSetup then
      begin
        nm := Trim(Names[i]); val := Trim(ValueFromIndex[i]);
        if WideSameText(nm, 'Version') then FVersion := val
        else if WideSameText(nm, 'Title') then FEditionTitle := val
        else if WideSameText(nm, 'MSIFileName') then FMSIFileName := val;
      end
      else if FInStrings then
        FStrings.Add(Strings[i]);
    end;

    //FGraphic := ReadString('Setup', 'Graphic', '');

    if (FVersion = '') then
      raise EInstallInfo.Create('setup.inf is corrupt (code 1).  Setup cannot continue.');

    if not SysUtils.FileExists(ExtPath + FMSIFileName) then  // I3310
      raise EInstallInfo.Create('Installer '+FMSIFileName+' does not exist (code 3).  Setup cannot continue.');
  finally
    Free;
  end;
end;

function TInstallInfo.Text(const Name: TInstallInfoText;
  const Args: array of const): WideString;
begin
  Result := WideFormat(Text(Name), Args);
end;

procedure DoExtractOnly(FSilent: Boolean; const FExtractOnly_Path: string);
var
  path: string;
begin
  path := FExtractOnly_Path;  // I3476

  if path = '' then
    path := '.';

  if (path <> '.') and (path <> '.\') and not DirectoryExists(path) then  // I3081  // I3476
  begin
    if not CreateDir(path) then  // I3476
    begin
      LogError('Setup could not create the target folder '+path);  // I3476
      ExitCode := Integer(GetLastError);
      Exit;
    end;
  end;

  if not ProcessArchive(path) then
  begin
    LogError('This file was not a valid self-extracting archive.  The files should already be in the same folder as the archive.');
    ExitCode := ERROR_BAD_FORMAT;
    Exit;
  end;

  if not FSilent then
    LogError('All files extracted from the archive to '+path+'\.');  // I3476

  ExitCode := ERROR_SUCCESS;
end;


end.
