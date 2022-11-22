unit Keyman.Developer.System.ValidateRepoChanges;

interface

uses
  System.Classes,
  System.JSON,

  kpsfile;

type
  TValidateRepoChanges = class
  private
    class var err_old, err_new: TStringList;
    class var root: string;
    class function Search(path, operation: string): Boolean; static;
    class function CheckKeyboardInfo(name: string): Boolean; static;
    class function CheckPackage(name: string): Boolean; static;
    class function LoadKeyboardInfoFile(filename: string): TJSONObject; static;
    class function GetLanguageCodesFromJson(root: TJSONObject;
      langs: TStringList): Boolean; static;
    class function GetLanguageCodesFromKps(kps: TKPSFile;
      langs: TStringList): Boolean; static;
    class function CompareKeyboardInfoScripts(name: string): Boolean; static;
  public
    class function Execute(path, operation: string): Boolean;
  end;

implementation

uses
  System.Character,
  System.Generics.Collections,
  System.SysUtils,

  BCP47Tag,
  Keyman.System.KeyboardInfoFile,
  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.Standards.LangTagsRegistry,
  TempFileManager,
  Unicode,
  utilexecute;

{ TValidateRepoChanges }

class function TValidateRepoChanges.Execute(path, operation: string): Boolean;
begin
  if path = '' then path := 'c:\projects\keyman\keyboards';

  path := IncludeTrailingPathDelimiter(path);

  root := path;

//  Result := CheckKeyboardInfo(path + 'release\k\kayan\kayan.keyboard_info');

  err_old := TStringList.Create;
  err_new := TStringList.Create;
  try
    Result := Search(path, operation);
    err_old.SaveToFile('repo-check.old.txt');
    err_new.SaveToFile('repo-check.new.txt');
  finally
    err_old.Free;
    err_new.Free;
  end;
end;

class function TValidateRepoChanges.Search(path, operation: string): Boolean;
var
  f: TSearchRec;
begin
  if FindFirst(path + '*.keyboard_info', 0, f) = 0 then
  begin
    repeat
      if operation = 'compare-bcp47-revisions' then CheckKeyboardInfo(path + f.Name)
      else if operation = 'compare-bcp47-scripts' then CompareKeyboardInfoScripts(path + f.Name);
    until FindNext(f) <> 0;
    FindClose(f);
  end;
  if FindFirst(path + '*.kps', 0, f) = 0 then
  begin
    repeat
      if operation = 'compare-bcp47-revisions' then CheckPackage(path + f.Name);
    until FindNext(f) <> 0;
    FindClose(f);
  end;
  if FindFirst(path + '*', faDirectory, f) = 0 then
  begin
    repeat
      if ((f.Attr and faDirectory) = faDirectory) and (f.Name <> '.') and (f.Name <> '..') and not SameText(f.Name, 'build') then
        Search(path + f.Name + '\', operation);
    until FindNext(f) <> 0;
    FindClose(f);
  end;

  Result := True;
end;

class function TValidateRepoChanges.LoadKeyboardInfoFile(filename: string): TJSONObject;
begin
  with TStringStream.Create('', TEncoding.UTF8) do
  try
    LoadFromFile(filename);
    Result := TJSONObject.ParseJsonValue(DataString) as TJSONObject;
  finally
    Free;
  end;
end;

class function TValidateRepoChanges.CheckKeyboardInfo(name: string): Boolean;
var
  i: Integer;
  json_new, json_old: TJSONObject;
//  t: TTempFile;
  relpath: string;
  content: string;
  ec: Integer;
  lang_new: TStringList;
  lang_old: TStringList;
  langc_old: TArray<string>;
  langc_new: TArray<string>;
  found_error: Boolean;
  j: Integer;
  langc_old_prev: string;
begin
  found_error := False;
  relpath := name.Substring(root.Length).Replace('\', '/');

  json_new := LoadKeyboardInfoFile(name);
  if not Assigned(json_new) then
    raise Exception.Create('Unable to load file '+name);
  // Get previous revision from git

//  t := TTempFileManager.Get('.keyboard_info');

  if not TUtilExecute.Console('git show "HEAD:'+relpath+'"', root, content, ec) then
    RaiseLastOSError;

  if ec <> 0 then
    raise Exception.Create('Unable to execute git show for '+name);

  if Copy(content, 1, 3) = string(UTF8Signature) then
    Delete(content, 1, 3);

  json_old := TJSONObject.ParseJsonValue(content) as TJSONObject;
  if not Assigned(json_old) then
    raise Exception.Create('Unable to load old file '+name);

  lang_new := TStringList.Create;
  lang_old := TStringList.Create;
  try
    if not GetLanguageCodesFromJson(json_new, lang_new) then
      raise Exception.Create('Unable to get Language codes');
    if not GetLanguageCodesFromJson(json_old, lang_old) then
      raise Exception.Create('Unable to get Language codes');

    j := 0;
    for i := 0 to lang_new.Count - 1 do
    begin
      repeat
        if j >= lang_old.Count then
        begin
          if not found_error then
          begin
            writeln('Checking '+relpath);
            found_error := True;
          end;
          writeln('  Mismatch in number of language codes');
          break;
        end;

        langc_new := lang_new[i].Split([',']);
        langc_old := lang_old[j].Split([',']);
        Inc(j);
      until langc_old[0] <> langc_old_prev;
      langc_old_prev := langc_old[0];
      if j >= lang_old.Count then Break;

      if langc_new[0] <> langc_old[0] then
      begin
        if not found_error then
        begin
          writeln('Checking '+relpath);
          found_error := True;
        end;
        writeln(Format('  New code %s [%s] does not match old code %s [%s]', [langc_new[1], langc_new[0], langc_old[1], langc_old[0]]));
      end;
    end;

    if found_error then
    begin
      err_old.Add('Checking '+relpath);
      for i := 0 to lang_old.Count - 1 do
      begin
        langc_old := lang_old[i].Split([',']);
        err_old.Add(Format('  %s [%s]', [langc_old[1], langc_old[0]]));
      end;
      err_old.Add('');

      err_new.Add('Checking '+relpath);
      for i := 0 to lang_new.Count - 1 do
      begin
        langc_new := lang_new[i].Split([',']);
        err_new.Add(Format('  %s [%s]', [langc_new[1], langc_new[0]]));
      end;
      err_new.Add('');
    end;
  finally
    lang_new.Free;
    lang_old.Free;
  end;

  Result := True;
end;

class function TValidateRepoChanges.GetLanguageCodesFromJson(root: TJSONObject; langs: TStringList): Boolean;
var
  i: Integer;
  a: TJSONArray;

  procedure AddLang(lang: string);
  begin
    langs.Add(TCanonicalLanguageCodeUtils.FindBestTag(lang, False, False)+','+lang);
  end;

begin
  if root.Values[TKeyboardInfoFile.SLanguages] = nil then
    Exit(False);

  if root.Values[TKeyboardInfoFile.SLanguages] is TJSONArray then
  begin
    a := root.Values[TKeyboardInfoFile.SLanguages] as TJSONArray;
    for i := 0 to a.Count - 1 do
      AddLang(a.Items[i].AsType<string>);
  end
  else
  begin
    root := root.Values[TKeyboardInfoFile.SLanguages] as TJSONObject;
    for i := 0 to root.Count - 1 do
      AddLang(root.Pairs[i].JsonString.Value);
  end;

  langs.Sort;

  Result := True;
end;

class function TValidateRepoChanges.GetLanguageCodesFromKps(kps: TKPSFile; langs: TStringList): Boolean;
var
  i: Integer;
  j: Integer;

  procedure AddLang(lang: string);
  begin
    langs.Add(TCanonicalLanguageCodeUtils.FindBestTag(lang, False, False)+','+lang);
  end;

begin
  for i := 0 to kps.Keyboards.Count - 1 do
    for j := 0 to kps.Keyboards[i].Languages.Count - 1 do
      AddLang(kps.Keyboards[i].Languages[j].ID);

  langs.Sort;

  Result := True;
end;

class function TValidateRepoChanges.CheckPackage(name: string): Boolean;
var
  i: Integer;
  kps_new, kps_old: TKPSFile;
//  t: TTempFile;
  relpath: string;
  content: string;
  ec: Integer;
  lang_new: TStringList;
  lang_old: TStringList;
  langc_old: TArray<string>;
  langc_new: TArray<string>;
  found_error: Boolean;
  j: Integer;
  langc_old_prev: string;
begin
  found_error := False;
  relpath := name.Substring(root.Length).Replace('\', '/');

  kps_new := TKPSFile.Create;
  kps_new.FileName := name;
  kps_new.LoadXML;

  if not TUtilExecute.Console('git show "HEAD:'+relpath+'"', root, content, ec) then
    RaiseLastOSError;

  if ec <> 0 then
    raise Exception.Create('Unable to execute git show for '+name);

  if Copy(content, 1, 3) = string(UTF8Signature) then
    Delete(content, 1, 3);

  kps_old := TKPSFile.Create;
  kps_old.LoadXMLFromText(content);

  lang_new := TStringList.Create;
  lang_old := TStringList.Create;
  try
    if not GetLanguageCodesFromKps(kps_new, lang_new) then
      raise Exception.Create('Unable to get Language codes');
    if not GetLanguageCodesFromKps(kps_old, lang_old) then
      raise Exception.Create('Unable to get Language codes');

    j := 0;
    for i := 0 to lang_new.Count - 1 do
    begin
      repeat
        if j >= lang_old.Count then
        begin
          if not found_error then
          begin
            writeln('Checking '+relpath);
            found_error := True;
          end;
          writeln('  Mismatch in number of language codes');
          break;
        end;

        langc_new := lang_new[i].Split([',']);
        langc_old := lang_old[j].Split([',']);
        Inc(j);
      until langc_old[0] <> langc_old_prev;
      langc_old_prev := langc_old[0];
      if j >= lang_old.Count then Break;

      if langc_new[0] <> langc_old[0] then
      begin
        if not found_error then
        begin
          writeln('Checking '+relpath);
          found_error := True;
        end;
        writeln(Format('  New code %s [%s] does not match old code %s [%s]', [langc_new[1], langc_new[0], langc_old[1], langc_old[0]]));
      end;
    end;

    if found_error then
    begin
      err_old.Add('Checking '+relpath);
      for i := 0 to lang_old.Count - 1 do
      begin
        langc_old := lang_old[i].Split([',']);
        err_old.Add(Format('  %s [%s]', [langc_old[1], langc_old[0]]));
      end;
      err_old.Add('');

      err_new.Add('Checking '+relpath);
      for i := 0 to lang_new.Count - 1 do
      begin
        langc_new := lang_new[i].Split([',']);
        err_new.Add(Format('  %s [%s]', [langc_new[1], langc_new[0]]));
      end;
      err_new.Add('');
    end;
  finally
    lang_new.Free;
    lang_old.Free;
  end;
  kps_old.Free;
  kps_new.Free;

  Result := True;
end;

class function TValidateRepoChanges.CompareKeyboardInfoScripts(name: string): Boolean;
var
  i: Integer;
  json: TJSONObject;
  relpath: string;
  script, base_script: string;
  lang: TStringList;
  base_lang_item, lang_item: TArray<string>;
  found_error: Boolean;

  procedure WriteHeader;
  begin
    if not found_error then
    begin
      writeln;
      writeln('Checking '+relpath);
    end;
    found_error := True;
  end;

  function GetScript(lang: string): string;
  var
    v: string;
    LangTag: TLangTag;
    BCP47: TBCP47Tag;
  begin
    if TLangTagsMap.AllTags.TryGetValue(lang, v) then
      lang := v;

    if not TLangTagsMap.LangTags.TryGetValue(lang, LangTag) then
    begin
      BCP47 := TBCP47Tag.Create(lang);
      try
        Exit(BCP47.Script);
      finally
        BCP47.Free;
      end;
    end;

    Result := LangTag.script;
  end;

begin
  found_error := False;
  relpath := name.Substring(root.Length).Replace('\', '/');

  json := LoadKeyboardInfoFile(name);
  if not Assigned(json) then
    raise Exception.Create('Unable to load file '+name);

  lang := TStringList.Create;
  try
    if not GetLanguageCodesFromJson(json, lang) then
      raise Exception.Create('Unable to get Language codes');

    if lang.Count = 0 then
    begin
      WriteHeader;
      writeln('  Warning: no languages found');
      Exit(True);
    end;

    base_lang_item := lang[0].Split([',']);

    // Lookup the tag first, canonicalize to the base tag for known tags
    base_script := GetScript(base_lang_item[0]);
    if base_script = '' then
    begin
      WriteHeader;
      writeln('  Warning: could not identify tag '+base_lang_item[0]);
      Exit(True);
    end;

    for i := 1 to lang.Count - 1 do
    begin
      lang_item := lang[i].Split([',']);
      script := GetScript(lang_item[0]);
      if script <> base_script then
      begin
        WriteHeader;
        writeln(Format('  Tag %s [%s] has script <%s>, which differs from base tag %s [%s], script <%s>',
          [lang_item[1], lang_item[0], script, base_lang_item[1], base_lang_item[0], base_script]));
      end;
    end;
  finally
    lang.Free;
  end;

  Result := True;
end;

end.
