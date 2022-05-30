unit PackageFile;

interface

uses
  SysUtils, Classes, utilfiletypes, utilsystem, IniFiles;

type
  TPackageFile = class
  private
    FFileName: string;
    procedure SetFileName(const Value: string);
  public
    FileType: TKMFileType;
    Description: string;
    FAddStartMenuIcon: Boolean;
    FAddUninstallEntry: Boolean;
    //function CheckFile(const RootPath: string): Boolean;
    property FileName: string read FFileName write SetFileName;
  end;

  TPackageFileList = class(TList)
  protected
    function Get(Index: Integer): TPackageFile;
    procedure Put(Index: Integer; Item: TPackageFile);
  public
    property Items[Index: Integer]: TPackageFile read Get write Put; default;
    function IndexOf(Item: TPackageFile): Integer;
    function Add(Item: TPackageFile): Integer;
  end;

  TPackageInfo = class
  public
    FileType: TKMFileType;
    FileName: string;
    Files: TPackageFileList;
    Info: TStringList;
    FExecuteProgram: string;
    OutPath: string;
    FCreateStartMenu, FAddUninstallEntry: Boolean;
    FStartMenuPath, FReadmeFileName: string;
    StartMenuEntries: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  end;

implementation

{ TPackageInfo }

constructor TPackageInfo.Create;
begin
  inherited Create;
  FileType := ftPackageSource;
  Files := TPackageFileList.Create;
  Info := TStringList.Create;
  StartMenuEntries := TStringList.Create;
end;

destructor TPackageInfo.Destroy;
var
  i: Integer;
begin
  for i := 0 to Files.Count - 1 do Files[i].Free;
  Files.Free;
  Info.Free;
  StartMenuEntries.Free;
  inherited Destroy;
end;

procedure TPackageInfo.Load;
var
  s: TStringList;
  ft: TKMFileType;
  f: TPackageFile;
  i: Integer;
  t: string;
begin
  s := TStringList.Create;
  with TIniFile.Create(FileName) do
  try
    if ReadString('Package', 'FileType', FileTypes[ftPackageSource]) = FileTypes[ftPackageSource]
      then FileType := ftPackageSource
      else FileType := ftPackageInstallerSource;

    FExecuteProgram := ReadString('Package', 'ExecuteProgram', '');

    FAddUninstallEntry := ReadBool('Package', 'AddUninstallEntry', False);
    FCreateStartMenu := ReadBool('Package', 'CreateStartMenu', False);
    FStartMenuPath := ReadString('Package', 'StartMenuPath', '');

    OutPath := ReadString('Package', 'OutPath', '');

    ReadSectionValues('Info', Info);
    for i := 0 to Info.Count - 1 do
      Info.Values[Info.Names[i]] := '"' + Info.Values[Info.Names[i]] + '"';

    ReadSectionValues('StartMenu', StartMenuEntries);

    ReadSection('Files', s);
    for i := 0 to s.Count - 1 do
    begin
      f := TPackageFile.Create;

      f.FileName := ExpandFileNameEx(FileName, ReadString('File-'+s[i], 'FileName', s[i]));

      t := ReadString('File-'+s[i], 'FileType', FileTypes[ftOther]);
      for ft := Low(FileTypes) to High(FileTypes) do if t = FileTypes[ft] then f.FileType := ft;
      f.Description := ReadString('File-'+s[i], 'Description', '');
      if ReadBool('File-'+s[i], 'AddStartMenuIcon', False) then
        StartMenuEntries.Add(f.Description+'='+f.FileName);
      Files.Add(f);

      if f.FileType = ftReadMe then
        FReadMeFileName := f.FileName;
    end;

    if ReadBool('Package', 'AddReadmeStartMenuEntry', False) then
    begin
      for i := 0 to Files.Count - 1 do
        if Files[i].FileType = ftReadMe then
          StartMenuEntries.Add('Readme='+Files[i].FileName);
    end
    else
      FReadMeFileName := ExpandFileNameEx(FileName, ReadString('Package', 'ReadMeFile', ''));
  finally
    Free;
    s.Free;
  end;
end;

procedure TPackageInfo.Save;
var
  i: Integer;
  s, t: string;
begin
  if FileExists(FileName) then DeleteFile(FileName);
  with TIniFile.Create(FileName) do
  try
    WriteString('Package', 'FileType', FileTypes[FileType]);
    WriteString('Package', 'ExecuteProgram', FExecuteProgram);

    WriteBool('Package', 'AddUninstallEntry', FAddUninstallEntry);
    WriteBool('Package', 'CreateStartMenu', FCreateStartMenu);
    WriteString('Package', 'StartMenuPath', FStartMenuPath); 

    WriteString('Package', 'OutPath', OutPath);

    for i := 0 to Info.Count - 1 do
      WriteString('Info', Info.Names[i], Info.Values[Info.Names[i]]);

    for i := 0 to Files.Count - 1 do
      if Files[i].FileName <> '' then
      begin
        s := ExtractRelativePath(FileName, Files[i].FileName);
        t := IntToStr(i);
        WriteString('Files', t, '');
        WriteString('File-'+t, 'FileName', s);
        WriteString('File-'+t, 'FileType', FileTypes[Files[i].FileType]);
        WriteString('File-'+t, 'Description', Files[i].Description);
      end;

    for i := 0 to StartMenuEntries.Count - 1 do
      WriteString('StartMenu', StartMenuEntries.Names[i], StartMenuEntries.Values[StartMenuEntries.Names[i]]);

    if FReadMeFileName <> '' then WriteString('Package', 'ReadMeFile', ExtractRelativePath(FileName, FReadMeFileName));
    UpdateFile;
  finally
    Free;
  end;
end;

{ TPackageFile }

procedure TPackageFile.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

{ TPackageFileList }

function TPackageFileList.Get(Index: Integer): TPackageFile;        begin Result := TPackageFile(inherited Get(Index)); end;
procedure TPackageFileList.Put(Index: Integer; Item: TPackageFile); begin inherited Put(Index, Pointer(Item)); end;
function TPackageFileList.Add(Item: TPackageFile): Integer;         begin Result := inherited Add(Pointer(Item)); end;
function TPackageFileList.IndexOf(Item: TPackageFile): Integer;     begin Result := inherited IndexOf(Pointer(Item)); end;

end.

