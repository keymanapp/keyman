unit InstallCustomLanguageMain;

interface

function Run: Integer;

implementation

uses
  Xml.XmlIntf,
  Xml.XmlDoc,
  keymanapi_TLB,
  Winapi.ActiveX,
  winapi.shlwapi,
  Winapi.Windows,
  System.Win.ComObj,
  System.Win.Registry,
  BCP47SuppressScriptRegistry,
  TempFileManager,
  Winapi.msctf,
  utilexecute,
  System.Classes,
  System.SysUtils,
  System.Variants;

var
  kmcom: IKeyman = nil;

function ResolveLocaleName(nameToResolve, localeName: PWideChar; cchLocaleName: Integer): Integer; stdcall; external 'kernel32.dll' name 'ResolveLocaleName';

function ExtractBaseLanguageCode: string;
var
  FScript: string;
  FLocaleScript: string;
  FLocaleName: string;
  FScripts: TStringList;
  function ExtractLangCode(t: string): string;
  var
    n: Integer;
  begin
    n := Pos('-', t);
    if n = 0
      then Result := t
      else Result := Copy(t, 1, n-1);
  end;
begin
  FScripts := TStringList.Create;
  with FScripts do
  try
    FScripts.Text := SuppressScriptSubtagRegistry;

    FLocaleName := ExtractLangCode(ParamStr(2));
    FScript := FScripts.Values[FLocaleName];
    if FScript <> '' then
    begin
      FLocaleScript := FLocaleName + '-' + FScript;
      if SameText(Copy(ParamStr(2), 1, Length(FLocaleScript)), FLocaleScript) then
        FLocaleName := Copy(ParamStr(2), 1, Length(FLocaleName)) +
          Copy(ParamStr(2), Length(FLocaleScript)+1, MaxInt)
      else
        FLocaleName := ParamStr(2);
    end
    else
      FLocaleName := ParamStr(2);
  finally
    Free;
  end;

  Result := FLocaleName;
end;

function PowerShell(command: string): string;
begin
  TUtilExecute.Console('powershell.exe -ExecutionPolicy Unrestricted -Command "& {'+command+'}"', ExtractFilePath(ParamStr(0)), Result);
end;

function FindBCP47Language(FLocaleName: string): Cardinal;
type
  TLanguageTagLCID = record
    LanguageTag: string;
    Autonym: string;
    EnglishName: string;
    LocalizedName: string;
    ScriptName: string;
    LCID: Cardinal;
  end;
var
  i, j: Integer;
  FXMLDoc: IXMLDocument;
  List: IXMLNode;
  Language: IXMLNode;
  LanguageTags: array of TLanguageTagLCID;
  Node: IXMLNode;
  Name: string;
begin
  FXMLDoc := LoadXMLData(Powershell('Get-WinUserLanguageList | ConvertTo-Xml -As String'));
  List := FXMLDoc.DocumentElement.ChildNodes[0];
  SetLength(LanguageTags, List.ChildNodes.Count);
  for i := 0 to List.ChildNodes.Count - 1 do
  begin
    Language := List.ChildNodes[i];
    LanguageTags[i].LanguageTag := '';
    LanguageTags[i].LCID := 0;
    for j := 0 to Language.ChildNodes.Count - 1 do
    begin
      Node := Language.ChildNodes[j];
      Name := Node.Attributes['Name'];
      if Name = 'LanguageTag' then
        LanguageTags[i].LanguageTag := VarToStr(Node.NodeValue)
      else if Name = 'Autonym' then
        LanguageTags[i].Autonym := VarToStr(Node.NodeValue)
      else if Name = 'EnglishName' then
        LanguageTags[i].EnglishName := VarToStr(Node.NodeValue)
      else if Name = 'LocalizedName' then
        LanguageTags[i].LocalizedName := VarToStr(Node.NodeValue)
      else if Name = 'ScriptName' then
        LanguageTags[i].ScriptName := VarToStr(Node.NodeValue)
      else if Name = 'InputMethodTips' then
      begin
        if Node.ChildNodes.Count > 0 then
          LanguageTags[i].LCID := StrToInt('$'+Copy(Node.ChildNodes[0].NodeValue, 1, 4));
      end;
    end;
  end;

  for i := Low(LanguageTags) to High(LanguageTags) do
    if SameText(Copy(LanguageTags[i].LanguageTag, 1, Length(FLocaleName)), FLocaleName) then
      Exit(LanguageTags[i].LCID);

  Result := 0;
end;

type
  TSHLoadIndirectString = function(const pszSource: PWideChar; pszOutBuf: PWideChar; cchOutBuf: UINT; ppvReserved: Pointer): HRESULT; stdcall;

var
  hSHLWApi: THandle = 0;
  FSHLoadIndirectString: TSHLoadIndirectString = nil;

function LoadIndirectString(const name: WideString): WideString;
var
  buf: array[0..260] of WideChar;
begin
  Result := '';

  if hSHLWApi = 0 then
  begin
    hSHLWApi := LoadLibrary('shlwapi.dll');
    if hSHLWApi = 0 then
      Exit;
    FSHLoadIndirectString := GetProcAddress(hSHLWApi, 'SHLoadIndirectString');
  end;

  if not Assigned(FSHLoadIndirectString) then Exit;
  if FSHLoadIndirectString(PWideChar(name), buf, Length(buf), nil) = S_OK then
    Result := buf;
end;

type
  TRegistryHelper = class helper for TRegistry
    procedure ReadMultiString(const ValueName: string; Strings: TStrings);
  end;

procedure TRegistryHelper.ReadMultiString(const ValueName: string; Strings: TStrings);
var
  valueType: DWORD;
  valueLen: DWORD;
  p, buffer: PChar;
begin
  Strings.Clear;

  if not CheckResult(RegQueryValueEx(CurrentKey, PChar(ValueName), nil, @valueType, nil, @valueLen)) then
    raise ERegistryException.Create('Unable to find value '+ValueName);

  if valueType <> REG_MULTI_SZ then
    raise ERegistryException.Create('Value '+ValueName+' does not have type REG_MULTI_SZ');

  if valueLen = 0 then
    // Empty
    Exit;

  GetMem(buffer, valueLen + 1);
  try
    RegQueryValueEx(CurrentKey, PChar(ValueName), nil, nil, PBYTE(buffer), @valueLen);
    buffer[valueLen] := #0;

    p := buffer;
    while p^ <> #0 do
    begin
      Strings.Add(p);
      Inc(p, lstrlen(p) + 1);
    end;
  finally
    FreeMem(buffer);
  end;
end;

function FindBCP47Language2(FLocaleName: string): Cardinal;
type
  TLanguageTagLCID = record
    LanguageTag: string;
//    Autonym: string;
//    EnglishName: string;
    LocalizedName: string;
//    ScriptName: string;
    TransientLangId, LCID: Cardinal;
  end;
var
  n, i, j: Integer;
  Language: string;
  LanguageTags: array of TLanguageTagLCID;
  Name: string;
  FValueNames: TStringList;
  FLanguageNames: TStringList;
begin
  with TRegistry.Create do
  try
    if OpenKeyReadOnly('Control Panel\International\User Profile') and ValueExists('Languages') then
    begin
      FLanguageNames := TStringList.Create;
      FValueNames := TStringList.Create;
      try
        ReadMultiString('Languages', FLanguageNames);

        SetLength(LanguageTags, FLanguageNames.Count);
        for i := 0 to FLanguageNames.Count - 1 do
        begin
          Language := FLanguageNames[i];
          LanguageTags[i].LanguageTag := Language;
          if OpenKeyReadOnly('\Control Panel\International\User Profile\'+Language) then
          begin
            if ValueExists('TransientLangId') then
              LanguageTags[i].TransientLangId := ReadInteger('TransientLangId');
            if ValueExists('CachedLanguageName') then
              LanguageTags[i].LocalizedName := LoadIndirectString(ReadString('CachedLanguageName'));
            FValueNames.Clear;
            GetValueNames(FValueNames);
            for j := 0 to FValueNames.Count - 1 do
              if not SameText(FValueNames[j], 'TransientLangId') and (GetDataType(FValueNames[j]) = rdInteger) then
              begin
                if Copy(FValueNames[j],5,1)=':' then
                  LanguageTags[i].LCID := StrToIntDef('$'+Copy(FValueNames[j],1,4),0);
              end;
          end;
        end;
      finally
        FLanguageNames.Free;
        FValueNames.Free;
      end;
    end;
  finally
    Free;
  end;

  for i := Low(LanguageTags) to High(LanguageTags) do
    if SameText(Copy(LanguageTags[i].LanguageTag, 1, Length(FLocaleName)), FLocaleName) then
      Exit(LanguageTags[i].LCID);

  Result := 0;
end;

function InstallBCP47Language(FLocaleName: string): Boolean;
var
  ScriptFilename: string;
begin
  // Use PowerShell
  ScriptFilename := TTempFileManager.Get('.ps1').Name;

  with TStringList.Create do
  try
    Add('$list = Get-WinUserLanguageList');
    Add('$list.Add("'+FLocaleName+'")');
    Add('Set-WinUserLanguageList $list -force');
    SaveToFile(ScriptFilename);
  finally
    Free;
  end;

  TUtilExecute.WaitForProcess('powershell.exe -ExecutionPolicy Unrestricted "& ""'+ScriptFilename+'"""', ExtractFilePath(ScriptFilename));
  Result := True;
end;

function RemoveDefaultKeyboardForLanguage(FLocaleName: string): Boolean;
var
  ScriptFilename: string;
begin
  ScriptFilename := TTempFileManager.Get('.ps1').Name;

  with TStringList.Create do
  try
    Add('$list = Get-WinUserLanguageList');
    Add('$item = $list | Where-Object {$_.LanguageTag -like "'+FLocaleName+'"}');
    Add('$result = $item.InputMethodTips.RemoveAll({ param($m) $m -match "[0-9a-z]{4}:[0-9a-z]{8}" })');
    Add('Set-WinUserLanguageList $list -force');
    SaveToFile(ScriptFilename);
    TUtilExecute.WaitForProcess('powershell.exe -ExecutionPolicy Unrestricted "& ""'+ScriptFilename+'"""', ExtractFilePath(ScriptFilename));
  finally
    Free;
  end;
  Result := True;
end;

function Run: Integer;
var
  Code, n: Integer;
  FLocaleName: string;
begin
  if ParamCount < 2 then
  begin
    writeln('InstallCustomLanguage <KeyboardName> <BCP47Code>');
    Exit(1);
  end;

  FLocaleName := ExtractBaseLanguageCode;

  kmcom := CoKeyman.Create;

  n := kmcom.Keyboards.IndexOf(ParamStr(1));
  if n < 0 then
  begin
    writeln('Keyboard '+ParamStr(1)+' not found.');
    Exit(3);
  end;

  Code := FindBCP47Language2(FLocaleName);

  if Code = 0 then
  begin
    writeln('Installing language and keyboard');
    if not InstallBCP47Language(FLocaleName) then
    begin
      writeln('Language '+ParamStr(2)+' was not successfully installed.');
      Exit(4);
    end;

    Code := FindBCP47Language2(FLocaleName);
    if Code = 0 then
    begin
      writeln('Language '+ParamStr(2)+' was installed but could not be resolved.');
      Exit(5);
    end;

    kmcom.Keyboards[n].Languages.InstallByLangID(Code);

    writeln('Removing default keyboard');
    RemoveDefaultKeyboardForLanguage(FLocaleName);
  end
  else
  begin
    writeln('Adding keyboard to already installed language');
    kmcom.Keyboards[n].Languages.InstallByLangID(Code);
  end;

  Exit(0);
end;

end.
