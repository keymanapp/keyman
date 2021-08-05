(*
  Name:             CompilePackageInstaller
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      4 Jun 2007

  Modified Date:    6 Jun 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          04 Jun 2007 - mcdurdin - Initial version
                    05 Jun 2007 - mcdurdin - I817 - Fix Unicode .inf file (not available on 9x)
                    19 Jun 2007 - mcdurdin - I817 - Use Unicode .inf file again
                    20 Jun 2007 - mcdurdin - Support compiling package installer from a .kmp file
                    23 Aug 2007 - mcdurdin - Support building an exe installer without any embedded kmp files
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    28 Jul 2008 - mcdurdin - I1558 - Embed graphic file into setup.exe
                    28 Jul 2008 - mcdurdin - I1559 - Support custom setup.exe strings
                    05 Aug 2008 - mcdurdin - Don't crash if FPack is not assigned
                    30 Dec 2010 - mcdurdin - I2562 - EULA as part of setup bootstrapper
                    04 Nov 2011 - mcdurdin - I3126 - Add flag to allow install with full msi UI
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    06 Feb 2012 - mcdurdin - I2971 - TIKE maintains an open handle to .msi when compiling package installer
                    04 Nov 2012 - mcdurdin - I3543 - V9.0 - Merge of I2971 - TIKE maintains an open handle to .msi when compiling package installer
                    23 Feb 2015 - mcdurdin - I4598 - V9.0 - Keyman installer does not show EULA when bundled with a keyboard
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    06 Jun 2015 - mcdurdin - I4741 - Package installer compiler references wrong path for compiled package file
*)
unit CompilePackageInstaller;  // I3306

interface

uses Windows, kpsfile, kmpinffile, PackageInfo, CompilePackage,
  Keyman.Developer.System.Project.Projectlog,
  jwaMsi, jwaMsiQuery, SysUtils;

function DoCompilePackageInstaller(pack: TKPSFile; FMessageEvent: TCompilePackageMessageEvent; FSilent: Boolean;
  AInstallerMSI, AOutputFilename, ARedistSetupPath: string;
  AUpdateInstaller: Boolean; ABuildPackage: Boolean; ALicense, ATitleImage, AAppName: string;
  AStartDisabled, AStartWithConfiguration: Boolean): Boolean;   // I4598   // I4688
function DoCompileMSIInstaller(FMessageEvent: TCompilePackageMessageEvent; FSilent: Boolean;
  AInstallerMSI, AOutputFilename, ARedistSetupPath, ALicense, ATitleImage, AAppName: string;
  AStartDisabled, AStartWithConfiguration: Boolean): Boolean;  // I2562

implementation

uses
  System.Classes,
  System.Zip,
  RedistFiles,
  OnlineConstants,
  utildir,
  utilsystem;

type
  ECompilePackageInstaller = class(Exception);

  TCompilePackageInstaller = class
  private
    FOnMessage: TCompilePackageMessageEvent;
    FPack: TKPSFile;
    FSilent: Boolean;
    FVersion: WideString;
    FTempPath: WideString;
    FProductName: WideString;
    FRedistSetupPath: string;
    FInstallerMSI: WideString;
    FUpdateInstaller: Boolean;
    FBuildPackage: Boolean;
    FOutputFilename: string;
    FLicense: string;  // I2562
    FTitleImage: string;
    FAppName: string;
    FStartDisabled: Boolean;
    FStartWithConfiguration: Boolean;
    procedure FatalMessage(msg: string);
    procedure WriteMessage(msg: string);
    function GetMSIOnlineProductID: Boolean;
  public
    constructor Create(APack: TKPSFile; ASilent: Boolean; AInstallerMSI: string; AUpdateInstaller, ABuildPackage: Boolean; AOutputFilename, ARedistSetupPath, ALicense, ATitleImage, AAppName: string; AStartDisabled, AStartWithConfiguration: Boolean);  // I2562
    procedure Run;
    property OnMessage: TCompilePackageMessageEvent read FOnMessage write FOnMessage;
  end;

function DoCompilePackageInstaller(pack: TKPSFile; FMessageEvent: TCompilePackageMessageEvent; FSilent: Boolean;
  AInstallerMSI, AOutputFilename, ARedistSetupPath: string; AUpdateInstaller: Boolean; ABuildPackage: Boolean;
  ALicense, ATitleImage, AAppName: string; AStartDisabled, AStartWithConfiguration: Boolean): Boolean;   // I4598   // I4688
begin
  with TCompilePackageInstaller.Create(pack, FSilent, AInstallerMSI, AUpdateInstaller, ABuildPackage,
    AOutputFilename, ARedistSetupPath, ALicense, ATitleImage, AAppName,
    AStartDisabled, AStartWithConfiguration) do  // I2562   // I4598   // I4688
  try
    OnMessage := FMessageEvent;
    Run;
    Result := True;
  finally
    Free;
  end;
end;

function DoCompileMSIInstaller(FMessageEvent: TCompilePackageMessageEvent; FSilent: Boolean; AInstallerMSI: string;
  AOutputFilename, ARedistSetupPath, ALicense, ATitleImage, AAppName: string;
  AStartDisabled, AStartWithConfiguration: Boolean): Boolean;  // I2562
begin
  with TCompilePackageInstaller.Create(nil, FSilent, AInstallerMSI, False, False, AOutputFileName, ARedistSetupPath,
    ALicense, ATitleImage, AAppName, AStartDisabled, AStartWithConfiguration) do  // I2562
  try
    OnMessage := FMessageEvent;
    Run;
    Result := True;
  finally
    Free;
  end;
end;

{ TCompilePackageInstaller }

constructor TCompilePackageInstaller.Create(APack: TKPSFile; ASilent: Boolean; AInstallerMSI: string; AUpdateInstaller, ABuildPackage: Boolean; AOutputFileName, ARedistSetupPath, ALicense, ATitleImage, AAppName: string; AStartDisabled, AStartWithConfiguration: Boolean);  // I2562
begin
  inherited Create;
  FPack := APack;
  FSilent := ASilent;
  FInstallerMSI := AInstallerMSI;
  FUpdateInstaller := AUpdateInstaller;
  FBuildPackage := ABuildPackage;
  FOutputFilename := AOutputFilename;
  FRedistSetupPath := ARedistSetupPath;
  FLicense := ALicense;  // I2562
  FTitleImage := ATitleImage;
  FAppName := AAppName;
  FStartDisabled := AStartDisabled;
  FStartWithConfiguration := AStartWithConfiguration;

  if (FLicense <> '') and not FileExists(FLicense) then
    raise ECompilePackageInstaller.CreateFmt('License file %s could not be found.', [FLicense]);

  if (FTitleImage <> '') and not FileExists(FTitleImage) then
    raise ECompilePackageInstaller.CreateFmt('Title iamge file %s could not be found.', [FTitleImage]);

  if not Assigned(FPack) then
  begin
    if (FInstallerMSI = '') or (FUpdateInstaller) or (FBuildPackage) or (FOutputFilename = '') then
      raise ECompilePackageInstaller.Create('Invalid arguments - if Pack is nil then FUpdateInstaller, FBuildPackage must be false and FInstallerMSI and FOutputFilename must not be empty.');
  end
  else if FOutputFilename = '' then   // I4688
    raise ECompilePackageInstaller.Create('Invalid arguments - if Pack is not nil then FOutputFilename must not be empty.');
end;

procedure TCompilePackageInstaller.FatalMessage(msg: string);
begin
  FOnMessage(Self, msg, plsFatal);   // I4706
end;

function TCompilePackageInstaller.GetMSIOnlineProductID: Boolean;
var
  hDatabase: MSIHANDLE;

  procedure CheckResult(v: UINT);
  begin
    if v <> ERROR_SUCCESS then RaiseLastOSError;
  end;

  function GetMSIProperty(PropertyName: WideString): WideString;
  const
    SQLPropertyQuery: WideString = 'SELECT Value FROM Property WHERE Property = ''%0:s''';
  var
    hView, hRecord: MSIHANDLE;
    buf: array[0..260] of WideChar;
    sz: Cardinal;
    FResult: Cardinal;
  begin
    CheckResult(MsiDatabaseOpenViewW(hDatabase, PWideChar(WideFormat(SQLPropertyQuery, [PropertyName])), hView));

    try
      CheckResult(MsiViewExecute(hView, 0));
      try
        hRecord := 0;
        FResult := MsiViewFetch(hView, hRecord);  // I2971   // I3543
        try
          case FResult of
        ERROR_NO_MORE_ITEMS:
          begin
            Result := '';
          end;
        ERROR_SUCCESS:
          begin
            sz := 260;
            CheckResult(MsiRecordGetStringW(hRecord, 1, buf, sz));
            Result := Copy(buf,1,sz);
          end;
        else
          RaiseLastOSError;
      end;
    finally
          if hRecord <> 0 then MsiCloseHandle(hRecord);  // I2971   // I3543
        end;
      finally
      MsiViewClose(hView);
    end;
    finally
      MsiCloseHandle(hView);  // I2971   // I3543
  end;
  end;

begin
  Result := False;
  CheckResult(MsiOpenDatabaseW(PWideChar(FInstallerMSI), nil, hDatabase));

  try
    FVersion := GetMSIProperty('ProductVersion');
    if FVersion = '' then
    begin
      FatalMessage('No ProductVersion found in the MSI file.');
      Exit;
    end;

    FProductName := GetMSIProperty('ProductName');
    if FProductName = '' then
    begin
      FatalMessage('No ProductName found in the MSI file.');
      Exit;
    end;

    Result := True;
  finally
    MsiCloseHandle(hDatabase);
  end;
end;

procedure TCompilePackageInstaller.Run;
var
  FDestFileName: WideString;
  fs: TFileStream;
  s: WideString;
  i: Integer;
  FMSIOptions: WideString;
  FRedistSetupFile, FPackageOutputFileName: string;  // I3126
begin
  { Check that the .msi is setup for the package }

  FTempPath := CreateTempPath;
  try
    try
      if Assigned(FPack) then
      begin
        if (FPack.FileName = '') and FBuildPackage then
        begin
          FatalMessage('You need to save the package before building.');
          Exit;
        end;

        WriteMessage('Compiling package ' + ExtractFileName(FPack.FileName) + '...');

        if FPack.Info.Desc['Name'] = '' then
        begin
          FatalMessage('You need to fill in the package name before building.');
          Exit;
        end;

        if FInstallerMSI <> '' then
          FPack.KPSOptions.MSIFileName := ExpandFileName(FInstallerMSI);

        FInstallerMSI := FPack.KPSOptions.MSIFileName;
        FMSIOptions := FPack.KPSOptions.MSIOptions;  // I3126

//        FOutputFileName := FPack.InstallerFileName;
      end
      else
      begin
        WriteMessage('Compiling installer '+ExtractFileName(FOutputFileName)+'...');
        FInstallerMSI := ExpandFileName(FInstallerMSI);
        FMSIOptions := '';  // I3126
      end;

      if not FileExists(FInstallerMSI) then
      begin
        FatalMessage('The installer could not be built because the MSI file was missing.');
        Exit;
      end;

      if (FInstallerMSI <> '') and FUpdateInstaller and Assigned(FPack) then
        FPack.SaveXML;

      if FRedistSetupPath = '' then
      begin
        FRedistSetupPath := GetRedistSetupPath;
        WriteMessage('Redist path not specified, loading default ('+FRedistSetupPath+')');
      end;

      if not FileExists(FRedistSetupPath + 'setup.exe') and
         not FileExists(FRedistSetupPath + 'setup-redist.exe') then
      begin
        FatalMessage('Neither setup.exe nor setup-redist.exe are present in redist ('+FRedistSetupPath+').');
        Exit;
      end;

      { Check the product ID for the package }
      if not GetMSIOnlineProductID then Exit;

      { TODO: if the package consists of *only* kmp files, then just add those to the installer }

      { Build the package }
      if Assigned(FPack) then
      begin
        FPackageOutputFileName := ExtractFilePath(FOutputFilename) + ChangeFileExt(ExtractFileName(FPack.FileName), '.kmp');   // I4741
        if FBuildPackage then
        begin
          if not DoCompilePackage(FPack, FOnMessage, True, False, FPackageOutputFileName) then Exit;
        end
        else if not FileExists(FPackageOutputFileName) then
        begin
          FatalMessage('The package file '+FPackageOutputFileName+' does not exist');
          Exit;
        end;

        { Test if the keyboards in the package need to be encrypted for the installer }

        FDestFileName := FPackageOutputFileName;
      end
      else
        FDestFileName := '';

      { Build setup.inf }
      with TStringList.Create do
      try
        s :=    '[Setup]'#13#10 +
                'Version=' + FVersion + #13#10 +
                'MSIFileName='+ExtractFileName(FInstallerMSI) + #13#10 +
                'MSIOptions='+FMSIOptions + #13#10;  // I3126

        if FAppName <> '' then
          s := s + 'AppName='+FAppName + #13#10;

        if FLicense <> '' then  // I2562
          s := s + 'License='+ExtractFileName(FLicense) + #13#10;

        if FTitleImage <> '' then
          s := s + 'TitleImage='+ExtractFileName(FTitleImage) + #13#10;


        if Assigned(FPack) and (FPack.KPSOptions.GraphicFile <> nil) and
          SameText(ExtractFileExt(FPack.KPSOptions.GraphicFile.FileName), '.bmp') then
        begin
          s := s + 'BitmapFileName='+ExtractFileName(FPack.KPSOptions.GraphicFile.FileName);
        end;

        if FStartDisabled then
          s := s + 'StartDisabled=True'#13#10;

        if FStartWithConfiguration then
          s := s + 'StartWithConfiguration=True'#13#10;

        s := s + #13#10 +
                '[Packages]'#13#10;

        if Assigned(FPack) then
          s := s + ExtractFileName(FDestFileName)+'='+FPack.Info.Desc[PackageInfo_Name];

        { Iterate through the strings }

        if Assigned(FPack) then
        begin
          s := s + #13#10#13#10'[Strings]'#13#10;
          for i := 0 to FPack.Strings.Count - 1 do
            s := s + FPack.Strings[i] + #13#10;
        end;

        Text := s;

        SaveToFile(FTempPath + '\setup.inf', TEncoding.UTF8);  // We want UTF-8 so Title can be Unicode  // I3337
      finally
        Free;
      end;

      { Build zip file }

      try
        with TZipFile.Create do
        try
          Open(FTempPath + '\setup.zip', TZipMode.zmWrite);
          Add(FTempPath + '\setup.inf');
          Add(FInstallerMSI);
          if FLicense <> '' then Add(FLicense);  // I2562
          if FTitleImage <> '' then Add(FTitleImage);
          if Assigned(FPack) and (FPack.KPSOptions.GraphicFile <> nil) and
              SameText(ExtractFileExt(FPack.KPSOptions.GraphicFile.FileName), '.bmp') then
            Add(FPack.KPSOptions.GraphicFile.FileName);
          if FDestFileName <> '' then Add(FDestFileName);
          Close;
        finally
          Free;
        end;
      except
        on E:EZipException do
        begin
          FatalMessage(E.Message);
          Exit;
        end;
      end;

      { Create the self-extracting archive }

      with TFileStream.Create(FOutputFileName, fmCreate) do
      try
        // A separate version of setup.exe which doesn't include a digital signature
        // is included here, so that it can be bundled with a zip and the result signed.
        if FileExists(FRedistSetupPath + 'setup-redist.exe')
          then FRedistSetupFile := FRedistSetupPath + 'setup-redist.exe'
          else FRedistSetupFile := FRedistSetupPath + 'setup.exe';
        fs := TFileStream.Create(FRedistSetupFile, fmOpenRead or fmShareDenyWrite);
        try
          CopyFrom(fs, 0);
        finally
          fs.Free;
        end;

        fs := TFileStream.Create(FTempPath + '\setup.zip', fmOpenRead or fmShareDenyWrite);
        try
          CopyFrom(fs, 0);
        finally
          fs.Free;
        end;
      finally
        Free;
      end;
    except
      on E:EOSError do
        FatalMessage(E.Message);
    end;
  finally
    DeleteTempPath(FTempPath);
  end;
end;

procedure TCompilePackageInstaller.WriteMessage(msg: string);
begin
  FOnMessage(Self, msg, plsInfo);   // I4706
end;

end.
