unit KeymanDesktopShell;

interface

uses
  keymanapi_tlb,
  utilexecute;

type
  TKeymanDesktopShell = class
    class function RunKeymanConfiguration(params: string = ''): Boolean;
    class function WaitForKeymanConfiguration(const params: string; var ec: Cardinal; Callback: TUtilExecuteCallbackWaitEvent = nil): Boolean;
    class function IsTextEditorVisible: Boolean; static;
    class function OpenHelp(topic: string): Boolean;
    class function OpenHelpJump(const topic: string; ActiveKeyboard: IKeymanKeyboardInstalled): Boolean;
  private
    class function GetRootPath: string;
    class function GetRootDir: string;
    class function GetShellPath: string;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows,

  KeymanPaths;

class function TKeymanDesktopShell.GetRootDir: string;
begin
  Result := ExcludeTrailingPathDelimiter(TKeymanPaths.KeymanDesktopInstallPath);
end;

class function TKeymanDesktopShell.GetRootPath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetRootDir);
end;

class function TKeymanDesktopShell.GetShellPath: string;
begin
  Result := TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell);
end;

class function TKeymanDesktopShell.RunKeymanConfiguration(params: string = ''): Boolean;
begin
  Result := TUtilExecute.ShellCurrentUser(0, GetShellPath, GetRootPath, params);
end;

class function TKeymanDesktopShell.WaitForKeymanConfiguration(
  const params: string; var ec: Cardinal; Callback: TUtilExecuteCallbackWaitEvent): Boolean;
begin
  Result := TUtilExecute.WaitForProcess('"'+GetShellPath+'" '+params, GetRootPath, ec, Callback);
end;

const
  STextEditorClassName = 'TfrmTextEditor';   // I4265

class function TKeymanDesktopShell.IsTextEditorVisible: Boolean;
begin
  Result := FindWindow(STextEditorClassName, nil) <> 0;
end;

class function TKeymanDesktopShell.OpenHelp(topic: string): Boolean;
var
  cleantopic: WideString;
  i: Integer;
begin
  cleantopic := '';
  for i := 1 to Length(topic) do
    if CharInSet(topic[i], ['a'..'z','A'..'Z','0'..'9','_','/','\','-']) then  // I3310
      cleantopic := cleantopic + topic[i];  // I3310
  Result := RunKeymanConfiguration('-h '+cleantopic);
end;

class function TKeymanDesktopShell.OpenHelpJump(const topic: string; ActiveKeyboard: IKeymanKeyboardInstalled): Boolean;
begin
  if ActiveKeyboard = nil
    then Result := RunKeymanConfiguration('-showhelp '+topic)
    else Result := RunKeymanConfiguration('-showhelp "'+topic+'" "'+ActiveKeyboard.ID+'"');
end;

end.
