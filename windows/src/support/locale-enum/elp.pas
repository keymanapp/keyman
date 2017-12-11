unit elp;

interface

procedure run;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Types,
  Winapi.Windows,

  utilstr;

var
  vv: TList<Integer>;

function locproc(p: PChar): Integer; stdcall;
var
  buf: array[0..128] of char;
  v: Integer;
begin
  v := StrToInt('$'+string(p));
  if LCIDToLocaleName(v, buf, 128, LOCALE_ALLOW_NEUTRAL_NAMES) = 0 then
    RaiseLastOSError;
  writeln(IntToHex(v, 4) + ',' + buf);

  v := v and $3ff;
  if vv.IndexOf(v) < 0 then
  begin
    if LCIDToLocaleName(v, buf, 128, LOCALE_ALLOW_NEUTRAL_NAMES) = 0 then
      RaiseLastOSError;
    writeln(IntToHex(v, 4) + ',' + buf);
    vv.Add(v);
  end;

  Result := 1;
end;

procedure run;
var
  i: Integer;
  s, p1, p2: string;
  m: TDictionary<Integer, String>;
  code: string;
  v: Integer;
begin
  vv := TList<Integer>.Create;
  EnumSystemLocales(@locproc, LCID_SUPPORTED);
//  LCIDToLocaleName(lcid, )
Exit;

  m := TDictionary<Integer, String>.Create;

  with TStringList.Create do
  try
    LoadFromFile('c:\temp\locproc\map.txt');
    for i := 0 to Count - 1 do
    begin
      s := Strings[i];
      if Pos('/*', s) > 0 then
        System.Delete(s, Pos('/*', s), MaxInt);
      s := Trim(s);
      if s = '' then
        Continue;

      if s[Length(s)] = ',' then
        System.Delete(s, Length(s), 1);

      if Copy(s, 1, 2) = '[(' then
      begin
        System.Delete(s, 1, 2);
        code := StrToken(s, ',');
        s := Trim(s);
        p1 := StrToken(s, ',');
        s := Trim(s);
        p2 := StrToken(s, ')');
        System.Delete(code, 1, 2); //0x

        v := StrToInt('$'+code);
        if m.ContainsKey(v) then
        begin
          writeln('Duplicate key: '+IntToHex(v,4));
        end
        else
          m.Add(v, p2);

        v := StrToInt('$'+code) and $3FF;
        if m.ContainsKey(v) then
        begin
          writeln('Duplicate key: '+IntToHex(v,4));
        end
        else
          m.Add(v, p1);
      end
      else if Copy(s, 1, 1) = '{' then
      begin
        System.Delete(s, 1, 3);
        code := StrToken(s, ',');
        s := Trim(s);
        p1 := StrToken(s, '}');
        System.Delete(p1,1,1);
        System.Delete(p1,Length(p1),1);

        v := StrToInt('$'+code);
        if m.ContainsKey(v) then
        begin
          writeln('Duplicate key: '+IntToHex(v,4));
        end
        else
          m.Add(v, p1);
//        writeln(s);
      end
      else
        raise Exception.Create('FAIL:'+IntToStr(i)+':'+Strings[i]);
    end;

    for i := 0 to m.Count - 1 do
      writeln(IntToHex(m.Keys.ToArray[i], 4)+','+m.Values.ToArray[i]);

  finally
    Free;
    m.Free;
  end;

end;

end.
