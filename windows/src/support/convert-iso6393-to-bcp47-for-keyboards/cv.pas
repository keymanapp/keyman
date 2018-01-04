unit cv;

interface

procedure Run;

implementation

uses
  System.Classes,
  System.JSON,
  System.SysUtils,

  JsonUtil,

  Keyman.System.KMXFileLanguages;


function LoadJsonFile(f: string): TJSONObject;
begin
  with TStringStream.Create('', TEncoding.UTF8) do
  try
    LoadFromFile(f);
    Result := TJSONObject.ParseJsonValue(DataString) as TJSONObject;
  finally
    Free;
  end;
end;

procedure SaveJsonFile(o: TJSONObject; f: string);
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    PrettyPrintJSON(o, str);
    with TStringStream.Create(str.Text, TEncoding.UTF8) do
    try
      // Use TStringStream so we don't get a default BOM prolog
      SaveToFile(f);
    finally
      Free;
    end;
  finally
    str.Free;
  end;
end;

procedure ProcessFile(f: string);
var
  o: TJSONObject;
  a, b: TJSONArray;
  i: Integer;
  FNewCode: string;
  FChanged: Boolean;
  olin, o2, ol: TJSONObject;
begin
  writeln(f);
  FChanged := False;
  b := nil;
  ol := nil;
  o := LoadJsonFile(f);
  if o.Values['languages'] is TJSONArray then
  begin
    a := o.Values['languages'] as TJSONArray;
    b := TJSONArray.Create;
    for i := 0 to a.Count - 1 do
    begin
      FNewCode := TKMXFileLanguages.TranslateISO6393ToBCP47(a.Items[i].Value);
      if FNewCode <> a.Items[i].Value then
        FChanged := True;
      b.Add(FNewCode);
    end;
  end
  else if o.Values['languages'] is TJSONObject then
  begin
    olin := o.Values['languages'] as TJSONObject;
    ol := TJSONObject.Create;
    for i := 0 to olin.Count - 1 do
    begin
      FNewCode := TKMXFileLanguages.TranslateISO6393ToBCP47(olin.Pairs[i].JsonString.Value);
      ol.AddPair(FNewCode, olin.Pairs[i].JsonValue.Clone as TJSONValue);
      if FNewCode <> olin.Pairs[i].JsonString.Value then
        FChanged := True;
    end;
  end;

  if not FChanged then
    Exit;

  o2 := TJSONObject.Create;
  for i := 0 to o.Count - 1 do
  begin
    if o.Pairs[i].JsonString.Value = 'languages' then
      if Assigned(b) then
        o2.AddPair(o.Pairs[i].JsonString.Value, b)
      else
        o2.AddPair(o.Pairs[i].JsonString.Value, ol)
    else
      o2.AddPair(o.Pairs[i].JsonString.Value, o.Pairs[i].JsonValue.Clone as TJSONValue);
  end;

  if FChanged then
    SaveJsonFile(o2, f);
end;

procedure ProcessFolder(f: string);
var
  ff: TSearchRec;
begin
  if FindFirst(f+'\*', faDirectory, ff) = 0 then
  begin
    repeat
      if (ff.Name <> '.') and (ff.Name <> '..') and ((ff.Attr and faDirectory) <> 0) then
        ProcessFolder(f+'\'+ff.Name)
      else if SameText(ExtractFileExt(ff.Name), '.keyboard_info') then
        ProcessFile(f+'\'+ff.Name)
    until FindNext(ff) <> 0;
    FindClose(ff);
  end;
end;

procedure Run;
begin
//  ProcessFile('c:\temp\convert-bcp47-for-keyboards\sample.keyboard_info');
  ProcessFolder('c:\projects\keyman\keyboards');
end;

end.
