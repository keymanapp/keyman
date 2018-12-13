unit Keyman.System.OSKBulkRenderer;

interface

uses
  Winapi.Windows,

  VisualKeyboard;

type
  TOSKBulkRenderer = class
  private class var
    FSubDirectories: Boolean;
    FTargetFolder: string;
    FTargetExt: string;
    FSourceFolder: string;
    FOverwrite: Boolean;
    FSystemLayout: DWord;
    FWidth: Integer;
    FIndexFile: TextFile;
    class function ParseParams: Boolean;
    class procedure ShowHelp;
    class function RunFolder(path: string): Boolean; static;
    class function RunFile(const srcfilename: string): Boolean; static;
    class procedure WriteHTMLFooter; static;
    class procedure WriteHTMLHeader; static;
    class procedure WriteIndex(s: string); static;
  public
    class function Run: Boolean;
  end;

implementation

uses
  System.Math,
  System.SysUtils,
  System.Types,

  VersionInfo,
  VisualKeyboardExportPNG;

{ TOSKBulkRenderer }

class procedure TOSKBulkRenderer.ShowHelp;
begin
  writeln('oskbulkrenderer [-f] [-s] <source_folder> [-w width] [-x ext] [-t target_folder] [-u base_hkl]');
  writeln;
  writeln('Renders all visual keyboard files found to .png');
  writeln;
  writeln('  -f                Overwrite existing output files (otherwise skips)');
  writeln('  -w width          Dimensions of keyboard, in pixels, default 800 (height is calculated)');
  writeln('  -s                Search subfolders');
  writeln('  -x ext            File extension to match, .kvk or .kvks, default .kvk');
  writeln('  source_folder     Folder to search for files');
  writeln('  -t target_folder  If specified, .png files will be written here, otherwise in same folder as source file; and also generates an index.html in the same folder');
  writeln('  -u base_hkl       HKL for base layout, if not specified 00000409 (en-us)');
end;

class function TOSKBulkRenderer.ParseParams: Boolean;
var
  i: Integer;
begin
  if ParamCount = 0 then
    Exit(False);

  FOverwrite := False;
  FSubDirectories := False;
  FTargetFolder := '';
  FSourceFolder := '';
  FTargetExt := '.kvk';
  FSystemLayout := $409;
  FWidth := 800;

  i := 1;
  while i <= ParamCount do
  begin
    if SameText(ParamStr(i), '-s') then
      FSubDirectories := True
    else if SameText(ParamStr(i), '-f') then
      FOverwrite := True
    else if SameText(ParamStr(i), '-x') then
    begin
      Inc(i);
      FTargetExt := ParamStr(i);
    end
    else if SameText(ParamStr(i), '-t') then
    begin
      Inc(i);
      FTargetFolder := ParamStr(i);
    end
    else if SameText(ParamStr(i), '-w') then
    begin
      Inc(i);
      FWidth := StrToInt(ParamStr(i));
    end
    else if SameText(ParamStr(i), '-u') then
    begin
      Inc(i);
      FSystemLayout := StrToInt('$'+ParamStr(i));
    end
    else if FSourceFolder = '' then
      FSourceFolder := ParamStr(i)
    else
      Exit(False);
    Inc(i);
  end;

  Result := FSourceFolder <> '';
end;

class function TOSKBulkRenderer.Run: Boolean;
begin
  if not ParseParams then
  begin
    ShowHelp;
    Exit(False);
  end;

  if not DirectoryExists(FSourceFolder) then
  begin
    writeln('Source folder '+FSourceFolder+' does not exist.');
    Exit(False);
  end;

  if FTargetFolder <> '' then
    WriteHtmlHeader;

  Result := RunFolder(FSourceFolder);

  if FTargetFolder <> '' then
    WriteHtmlFooter;
end;

class function TOSKBulkRenderer.RunFolder(path: string): Boolean;
var
  f: TSearchRec;
begin
  Result := True;

  path := IncludeTrailingPathDelimiter(path);

  if FindFirst(path+'*'+FTargetExt, 0, f) = 0 then
  try
    repeat
      if not RunFile(path+f.Name) then
        Exit(False);
    until FindNext(f) <> 0;
  finally
    System.SysUtils.FindClose(f);
  end;

  if FSubDirectories then
    if FindFirst(path+'*', faDirectory, f) = 0 then
    try
      repeat
        if (f.Name <> '..') and (f.Name <> '.') then
          if not RunFolder(path + f.Name) then
            Exit(False);
      until FindNext(f) <> 0;
    finally
      System.SysUtils.FindClose(f);
    end;
end;

class function TOSKBulkRenderer.RunFile(const srcfilename: string): Boolean;
var
  s, destfilename: string;
  vk: TVisualKeyboard;
  vkep: TVisualKeyboardExportPNG;
const
  KeyboardAspect = 0.3412;
begin
  if FTargetFolder <> ''
    then destfilename := IncludeTrailingPathDelimiter(FTargetFolder) + ChangeFileExt(ExtractFileName(srcfilename), '.png')
    else destfilename := ChangeFileExt(srcfilename, '.png');

  writeln(srcfilename + ' => ' + destfilename);

  vk := TVisualKeyboard.Create;
  try
    vk.LoadFromFile(srcfilename);

    if FTargetFolder <> '' then
    begin
      WriteIndex('<h2>'+srcfilename+'</h2>');
      WriteIndex('<dl>');
      WriteIndex('  <dt>Font</dt><dd>'+vk.Header.UnicodeFont.Name+'</dd>');
      s := '';
      if kvkh102 in vk.Header.Flags then s := s + 'key102 ';
      if kvkhDisplayUnderlying in vk.Header.Flags then s := s + 'displayunderlying ';
      if kvkhUseUnderlying in vk.Header.Flags then s := s + 'useunderlying ';
      if kvkhAltGr in vk.Header.Flags then s := s + 'usealtgr ';
      WriteIndex('  <dt>Flags</dt><dd>'+Trim(s)+'</dd>');

      if vk.Header.UnderlyingLayout <> '' then
        WriteIndex('  <dt>Base Layout</dt><dd>'+vk.Header.UnderlyingLayout+'</dd>');
      WriteIndex('  <dt>Associated Keyboard</dt><dd>'+vk.Header.AssociatedKeyboard+'</dd>');
      WriteIndex('</dl>');
      WriteIndex('<div><img src="'+ExtractFileName(destfilename)+'" alt="'+ExtractFileName(destfilename)+'"></div>');
      WriteIndex('');
    end;

    if FileExists(destfilename) and not FOverwrite then
    begin
      writeln('File '+destfilename+' already exists. Skipping');
      Exit(True);
    end;


    vkep := TVisualKeyboardExportPNG.Create(vk, False, False, True, FWidth);
    try
      vkep.ExportToFile(destfilename);
    finally
      vkep.Free;
    end;
  finally
    vk.Free;
  end;

  Result := True;
end;

class procedure TOSKBulkRenderer.WriteHTMLHeader;
begin
  AssignFile(FIndexFile, IncludeTrailingPathDelimiter(FTargetFolder) + 'index.html');
  Rewrite(FIndexFile);
  WriteIndex('<!DOCTYPE HTML>');
  WriteIndex('<html><head>');
  WriteIndex('  <meta charset="utf-8">');
  WriteIndex('  <title>On Screen Keyboard Renders</title>');
  WriteIndex('</head>');
  WriteIndex('<body>');
  WriteIndex('<h1>On Screen Keyboard Renders</h1>');
  WriteIndex('<dl>');
  WriteIndex('  <dt>Date</dt><dd>'+FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)+'</dd>');
  WriteIndex('  <dt>Version</dt><dd>'+GetVersionString+'</dd>');
  WriteIndex('</dl>');
  WriteIndex('');
end;

class procedure TOSKBulkRenderer.WriteHTMLFooter;
begin
  WriteIndex('</body></html>');
  CloseFile(FIndexFile);
end;

class procedure TOSKBulkRenderer.WriteIndex(s: string);
var
  us: utf8string;
begin
  us := UTF8String(s);
  writeln(FIndexFile, us);
end;

end.
