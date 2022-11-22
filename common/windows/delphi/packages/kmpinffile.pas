(*
  Name:             kmpinffile
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 May 2007

  Modified Date:    19 Nov 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 May 2007 - mcdurdin - Use widestrings
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
*)
unit kmpinffile;

interface

uses
  SysUtils, Classes, IniFiles, PackageInfo, PackageFileFormats;

type
  EKMPInfFile = class(EPackageInfo);

  TKMPInfFile = class(TPackage)
  protected
    procedure Import(AIni: TIniFile); override;
  public
    procedure CheckFiles(FPath: string);
    procedure RemoveFilePaths;
  end;

implementation

uses
  utilstr;

{ TKMPInfFile }

procedure TKMPInfFile.CheckFiles(FPath: string);
var
  i: Integer;
begin
  for i := 0 to Files.Count - 1 do
    if not FileExists(FPath + Files[i].FileName) then
      raise EKMPInfFile.Create('The file '''+Files[i].FileName+''' does not exist in the archive.');
end;

procedure TKMPInfFile.RemoveFilePaths;
var
  i: Integer;
begin
  for i := 0 to Files.Count - 1 do
    Files[i].FileName := ExtractFileName(Files[i].FileName);
end;

{-------------------------------------------------------------------------------
  kmp.inf 5.0 file format                                                          -
  ===================                                                          -
                                                                               -
  [Install]                                                                    -
  KMXFile=<keyboardfile>                                                       -
  ExecuteProgram=<cmdline>                                                     -
  StartMenuPath=<start menu path>                                              -
                                                                               -
  [InstallFiles]                                                               -
  kmp.inf=Package information                                                  -
  <filename>=<description>                                                     -
                                                                               -
  [Fonts]                                                                      -
  <fontfilename>=<fontdescription>                                             -
                                                                               -
  [PackageInfo]                                                                -
  <name>=""<description>","<url>"" |                                           -
  <name>=""<description>""                                                     -
                                                                               -
-------------------------------------------------------------------------------}

procedure TKMPInfFile.Import(AIni: TIniFile);
var
  str: TStringList;
  n, i: Integer;
  s: WideString;
  f: TPackageContentFile;
  sme: TPackageStartMenuEntry;
begin
  str := TStringList.Create;
  try
    { Read from [InstallFiles] }
    AIni.ReadSection('InstallFiles', str);

    for i := 0 to str.Count - 1 do
    begin
      s := AIni.ReadString('InstallFiles', str[i], '');
      if s = '' then s := str[i];
      f := TPackageContentFile.Create(Self);
      f.FileName := str[i];
      f.Description := s;
      Files.Add(f);
    end;

    str.Clear;

    { Read from [Fonts] }
    AIni.ReadSection('Fonts', str);

    for i := 0 to str.Count - 1 do
    begin
      s := AIni.ReadString('Fonts', str[i], '');
      if s = '' then s := str[i];
      f := TPackageContentFile.Create(Self);
      f.FileName := str[i];
      f.Description := s;
      Files.Add(f);
    end;

    str.Clear;

    { Read from [PackageInfo] }
    AIni.ReadSection('PackageInfo', str);

    for i := 0 to str.Count - 1 do
    begin
      s := AIni.ReadString('PackageInfo', str[i], '');
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

    { Read from [StartMenu] }
    AIni.ReadSection('StartMenu', str);

    for i := 0 to str.Count - 1 do
    begin
      s := AIni.ReadString('StartMenu', str[i], '');
      {lnkfile :=} CommaToken(s);


      sme := TPackageStartMenuEntry.Create(Self);
      sme.Name := CommaToken(s);
      sme.Prog := str[i];
      sme.Params := '';
      StartMenu.Entries.Add(sme);
    end;

    { Read from [Install] }
    StartMenu.AddUninstallEntry := LowerCase(AIni.ReadString('Install', 'StartMenuUninstall', '')) = 'true';
    StartMenu.Path := AIni.ReadString('Install', 'StartMenuPath', '');
    StartMenu.DoCreate := LowerCase(AIni.ReadString('Install', 'CreateStartMenu', '')) = 'true';
    //if LowerCase(AIni.ReadString('Install', 'StartMenuReadme', '')) = 'true' then -- already in [StartMenu]
    Options.ReadmeFile := Files.FromFileName(AIni.ReadString('Install', 'ReadmeFile', ''));
    Options.ExecuteProgram := AIni.ReadString('Install', 'ExecuteProgram', '');
  finally
    str.Free;
  end;
end;


end.

