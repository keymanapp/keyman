program KontextmenuInstaller;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JwaWindows,
  Registry;

procedure Install;
var Reg: TRegistry; ExeDesc: string; answer: string;
begin
  if not FileExists(ExtractFilePath(Paramstr(0))+'Kontextmenuhandler.exe') then
  begin
    Writeln('The file Kontextmenuhandler.exe must be in the current folder');
    Readln;
  end
  else
  begin
    Reg:=TRegistry.Create;
    try
      Reg.RootKey:=HKEY_CLASSES_ROOT;
      if Reg.OpenKey('.exe', false) then
      begin
        try
          ExeDesc:=Reg.ReadString('');
        finally
          Reg.CloseKey;
        end;
        if Reg.KeyExists(ExeDesc+'\Shell\RunAsAdmin') then
        begin
          Writeln('The key '+ExeDesc+'\Shell\RunAsAdmin already exists. Do you want to overwrite it? It cannot be restored! (y/n)');
          Readln(answer);
          if not (Answer[1] in ['Y', 'y']) then
            exit;
        end;
        if Reg.OpenKey(ExeDesc+'\Shell\RunAsAdmin', True) then
        try
          Reg.WriteString('', 'Run &elevated');
        finally
          Reg.CloseKey;
        end;
        if Reg.OpenKey(ExeDesc+'\Shell\RunAsAdmin\Command', True) then
        try
          Reg.WriteString('', '"'+ExtractFilePath(Paramstr(0))+'Kontextmenuhandler.exe" "%1"');
          Writeln('The menu handler was installed successfully.');
          Readln;
        finally
          Reg.CloseKey;
        end;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure Uninstall;
var Reg: TRegistry; ExeDesc: string;
begin
  Reg:=TRegistry.Create;
  try
    Reg.RootKey:=HKEY_CLASSES_ROOT;
    if Reg.OpenKey('.exe', false) then
    begin
      try
        ExeDesc:=Reg.ReadString('');
      finally
        Reg.CloseKey;
      end;
      if Reg.OpenKey(ExeDesc+'\Shell', false) then
      try
        if Reg.DeleteKey('RunAsAdmin') then
        begin
          Writeln('The menu handler was uninstalled successfully.');
          Readln;
        end
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

var Answer: String;
begin
  if Paramcount>0 then
  begin
    if LowerCase(ParamStr(1))='install' then
    begin
      Install;
      Exit;
    end
    else if LowerCase(ParamStr(1))='uninstall' then
    begin
      Uninstall;
      Exit;
    end;
  end;
  repeat
    Writeln('Press i to install, u to uninstall or e to exit');
    Readln(Answer);
  until Answer[1] in ['I', 'i', 'U', 'u', 'E', 'e'];
  if Answer[1] in ['I', 'i'] then
    Install
  else if Answer[1] in ['U', 'u'] then
    Uninstall;
end.
