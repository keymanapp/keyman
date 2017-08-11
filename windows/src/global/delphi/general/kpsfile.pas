(*
  Name:             kpsfile
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    4 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Support loading from ini
                    23 Aug 2006 - mcdurdin - Load and save from XML
                    30 May 2007 - mcdurdin - I817 - Added MSIFileName to package options
                    28 Jul 2008 - mcdurdin - I1559 - Support custom setup.exe strings
                    04 Nov 2011 - mcdurdin - I3126 - Add flag to allow install with full msi UI
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    26 Jun 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
*)
unit kpsfile;  // I3306

interface

uses
  System.Classes,
  System.IniFiles,
  System.SysUtils,
  Winapi.Windows,
  Xml.XMLIntf,

  PackageInfo,
  PackageFileFormats;

type
  EKPSFile = class(Exception);

  TKPSOptions = class(TPackageOptions)
  private
    FMSIFileName: WideString;
    FMSIOptions: WideString;  // I3126
  public
    procedure Assign(Source: TPackageOptions); override;
    procedure LoadXML(ARoot: IXMLNode); override;
    procedure SaveXML(ARoot: IXMLNode); override;
    property MSIFileName: WideString read FMSIFileName write FMSIFileName;
    property MSIOptions: WideString read FMSIOptions write FMSIOptions;  // I3126
  end;

  TKPSFile = class(TPackage)
  private
    FStrings: TStrings;
  protected
    procedure Import(AIni: TIniFile); override;
    procedure DoLoadXML(ARoot: IXMLNode); override;
    procedure DoSaveXML(ARoot: IXMLNode); override;
  public
    function KPSOptions: TKPSOptions;
    constructor Create;
    destructor Destroy; override;
    procedure LoadIni; override;
    procedure SaveIni; override;
    procedure DoExport(FFileName: string);
    property Strings: TStrings read FStrings;
  end;

implementation

uses
  System.TypInfo,
  System.Variants,
  System.WideStrings,

  KeymanVersion,
  utilfiletypes,
  utilsystem;

{ TKPSFile }

constructor TKPSFile.Create;
begin
  FStrings := TStringList.Create;
  Options := TKPSOptions.Create(Self);
  inherited Create;
end;

function TKPSFile.KPSOptions: TKPSOptions;
begin
  Result := Options as TKPSOptions;
end;

procedure TKPSFile.LoadIni;
var
  i: Integer;
begin
  inherited;
  for i := 0 to Files.Count - 1 do
    Files[i].FileName := ExpandFileNameEx(FileName, Files[i].FileName);
  for i := 0 to Options.Buttons.Count - 1 do
    Options.Buttons[i].UpdateButtonSize;
end;

procedure TKPSFile.DoLoadXML(ARoot: IXMLNode);
var
  i: Integer;
  ANode: IXMLNode;
begin
  inherited;
  if FileName <> '' then
    for i := 0 to Files.Count - 1 do
      Files[i].FileName := ExpandFileNameEx(FileName, Files[i].FileName);
  for i := 0 to Options.Buttons.Count - 1 do
    Options.Buttons[i].UpdateButtonSize;

  FStrings.Clear;
  ANode := ARoot.ChildNodes['Strings'];
  if ANode <> nil then
    for i := 0 to ANode.ChildNodes.Count - 1 do
      with ANode.ChildNodes[i] do
        FStrings.Add(Attributes['Name']+'='+Attributes['Value']);
end;

procedure TKPSFile.SaveIni;
var
  i: Integer;
begin
  for i := 0 to Files.Count - 1 do
    Files[i].FileName := ExtractRelativePath(FileName, Files[i].FileName);
  inherited;
  for i := 0 to Files.Count - 1 do
    Files[i].FileName := ExpandFileNameEx(FileName, Files[i].FileName);
end;

procedure TKPSFile.DoSaveXML(ARoot: IXMLNode);
var
  i: Integer;
  ANode: IXMLNode;
begin
  Options.FileVersion := SKeymanVersion70;

  if FileName <> '' then
    for i := 0 to Files.Count - 1 do
      Files[i].FileName := ExtractRelativePath(FileName, Files[i].FileName);
  inherited;
  if FileName <> '' then
    for i := 0 to Files.Count - 1 do
      Files[i].FileName := ExpandFileNameEx(FileName, Files[i].FileName);

  ANode := ARoot.AddChild('Strings');
  for i := 0 to FStrings.Count - 1 do
    with ANode.AddChild('String') do
    begin
      Attributes['Name'] := FStrings.Names[i];
      Attributes['Value'] := FStrings.ValueFromIndex[i];
    end;
end;

{ Importing and Exporting }

destructor TKPSFile.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

procedure TKPSFile.DoExport(FFileName: string);
var
  ini: TIniFile;
  i: Integer;
  s: string;
begin
  if FileExists(FFileName) then System.SysUtils.DeleteFile(FFileName);
  ini := TIniFile.Create(FFileName);
  try
    ini.WriteString('Package', 'FileType', 'ftPackageSource');
    ini.WriteString('Package', 'BuildRedist', '0');
    ini.WriteString('Package', 'ExecuteProgram', Options.ExecuteProgram);
    if StartMenu.AddUninstallEntry
      then ini.WriteString('Package', 'AddUninstallEntry', '1')
      else ini.WriteString('Package', 'AddUninstallEntry', '0');
    if StartMenu.DoCreate
      then ini.WriteString('Package', 'CreateStartMenu', '1')
      else ini.WriteString('Package', 'CreateStartMenu', '0');
    ini.WriteString('Package', 'OutPath', ChangeFileExt(FFileName, '.kmp'));

    for i := 0 to Info.Count - 1 do
    begin
      s := '""'+Info[i].Description+'"';
      if Info[i].URL = ''
        then s := s + '"'
        else s := s + ',"'+Info[i].URL+'""';
      ini.WriteString('Info', Info[i].Name, s);
    end;

    for i := 0 to Files.Count - 1 do
    begin
      ini.WriteString('Files', IntToStr(i), '');

      ini.WriteString('File-'+IntToStr(i), 'FileName', ExtractRelativePath(FFileName, Files[i].FileName));
      ini.WriteString('File-'+IntToStr(i), 'FileNameAbsolute', Files[i].FileName);
      ini.WriteString('File-'+IntToStr(i), 'FileType', GetEnumName(TypeInfo(TKMFileType), Ord(GetFileTypeFromFileName(Files[i].FileName))) );
      ini.WriteString('File-'+IntToStr(i), 'Description', Files[i].Description);
      ini.WriteString('File-'+IntToStr(i), 'AddStartMenuIcon', '0');
      ini.WriteString('File-'+IntToStr(i), 'AddUninstallEntry', '0');
    end;
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TKPSFile.Import(AIni: TIniFile);
var
  str: TStringList;
  n, i: Integer;
  s: string;
  f: TPackageContentFile;
  sme: TPackageStartMenuEntry;
begin
  str := TStringList.Create;
  try
    { Read from [Files] }
    AIni.ReadSection('Files', str);

    for i := 0 to str.Count - 1 do
    begin
      f := TPackageContentFile.Create(Self);
      Files.Add(f);
      f.FileName := AIni.ReadString('File-'+str[i], 'FileName', '');
      f.Description := AIni.ReadString('File-'+str[i], 'Description', '');
      if AIni.ReadString('File-'+str[i], 'AddStartMenuIcon', '0') = '1' then
      begin
        sme := TPackageStartMenuEntry.Create(Self);
        StartMenu.Entries.Add(sme);
        sme.Name := f.Description;
        sme.Prog := f.FileName;
        sme.Params := '';
      end;

      if AIni.ReadString('File-'+str[i], 'FileType', '')='ftReadme' then
        Options.ReadmeFile := f;
    end;

    str.Clear;

    { Read from [Info] }
    AIni.ReadSection('Info', str);

    for i := 0 to str.Count - 1 do
    begin
      s := AIni.ReadString('Info', str[i], '');
      Delete(s, 1, 1);
      n := Pos('"', s); if n = 0 then n := Length(s)+1;
      Info.Desc[str[i]] := Copy(s, 1, n-1);
      Delete(s,1,n);
      if (s <> '') and (s[1] = ',') then
      begin
        Delete(s,1,2);
        Info.Url[str[i]] := Copy(s,1,Length(s)-1);
      end;
    end;

    str.Clear;

    { Read from [Package] }
    StartMenu.AddUninstallEntry := LowerCase(AIni.ReadString('Package', 'AddUninstallEntry', '')) = '1';
    StartMenu.Path := Info.Desc[PackageInfo_Name];
    StartMenu.DoCreate := LowerCase(AIni.ReadString('Package', 'CreateStartMenu', '')) = 'true';

    if (LowerCase(AIni.ReadString('Package', 'AddReadmeStartMenuEntry', '')) = '1') and
      Assigned(Options.ReadmeFile) then
    begin
      sme := TPackageStartMenuEntry.Create(Self);
      sme.Name := 'Readme';
      sme.Prog := Options.ReadmeFile.FileName;
      StartMenu.Entries.Add(sme);
    end;
    Options.ExecuteProgram := AIni.ReadString('Package', 'ExecuteProgram', '');
  finally
    str.Free;
  end;
end;

{ TKPSOptions }

procedure TKPSOptions.Assign(Source: TPackageOptions);
begin
  inherited;
  FMSIFileName := (Source as TKPSOptions).MSIFileName;
  FMSIOptions := (Source as TKPSOptions).MSIOptions;  // I3126
end;

procedure TKPSOptions.LoadXML(ARoot: IXMLNode);
begin
  inherited;
  FMSIFileName := VarToWideStr(ARoot.ChildNodes['Options'].ChildNodes['MSIFileName'].NodeValue);
  FMSIOptions := VarToWideStr(ARoot.ChildNodes['Options'].ChildNodes['MSIOptions'].NodeValue);  // I3126
end;

procedure TKPSOptions.SaveXML(ARoot: IXMLNode);
begin
  inherited;
  ARoot.ChildNodes['Options'].ChildNodes['MSIFileName'].NodeValue := FMSIFileName;
  ARoot.ChildNodes['Options'].ChildNodes['MSIOptions'].NodeValue := FMSIOptions;  // I3126
end;

end.

