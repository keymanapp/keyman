unit Keyman.Developer.System.KeymanDeveloperPaths;

interface

uses
  System.SysUtils,

  KeymanPaths;

type
  TKeymanDeveloperPaths = class sealed
  public
    class function LockFilePath: string; static;

    class function NodePath: string; static;

    const S_LexicalModelCompiler = 'kmlmc.cmd';
    class function LexicalModelCompilerPath: string; static;

    const S_Kmc = 'kmc.cmd';
    class function KmcPath: string; static;

    const S_ServerConfigJson = 'config.json';
    class function ServerDataPath: string; static;
    class function ServerPath: string; static;

    const S_OptionsJson = 'options.json';
    class function OptionsPath: string; static;
  end;

implementation

uses
  Winapi.ShlObj,

  RegistryKeys,
  utilsystem;

{ TKeymanDeveloperPaths }

class function TKeymanDeveloperPaths.NodePath: string;
var
  KeymanRoot: string;
begin
  if TKeymanPaths.RunningFromSource(KeymanRoot)
    then Result := KeymanRoot + 'developer\src\inst\node\dist\node.exe'
    else Result := ExtractFilePath(ParamStr(0)) + 'node.js\node.exe';
end;

class function TKeymanDeveloperPaths.OptionsPath: string;
begin
  Result := GetFolderPath(CSIDL_PROFILE) + '.keymandeveloper\';
end;

class function TKeymanDeveloperPaths.ServerDataPath: string;
begin
  Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\server\';
end;

class function TKeymanDeveloperPaths.LockFilePath: string;
begin
  Result := GetFolderPath(CSIDL_APPDATA) + SFolderKeymanDeveloper + '\lock\';
end;

class function TKeymanDeveloperPaths.ServerPath: string;
var
  KeymanRoot: string;
begin
  if TKeymanPaths.RunningFromSource(KeymanRoot)
    then Result := KeymanRoot + 'developer\src\server\'
    else Result := ExtractFilePath(ParamStr(0)) + 'server\';
end;

class function TKeymanDeveloperPaths.KmcPath: string;
var
  KeymanRoot: string;
begin
  if TKeymanPaths.RunningFromSource(KeymanRoot)
    then Result := KeymanRoot + 'developer\src\tike\'
    else Result := ExtractFilePath(ParamStr(0));

  Result := Result + S_Kmc;
end;

class function TKeymanDeveloperPaths.LexicalModelCompilerPath: string;
var
  KeymanRoot: string;
begin
  if TKeymanPaths.RunningFromSource(KeymanRoot)
    then Result := KeymanRoot + 'developer\src\tike\'
    else Result := ExtractFilePath(ParamStr(0));

  Result := Result + S_LexicalModelCompiler;
end;

end.
