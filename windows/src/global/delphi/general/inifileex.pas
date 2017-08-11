unit inifileex;

interface

uses Classes, IniFiles;

type
  TIniFileEx = class(TIniFile)
    procedure ReadSectionValues2(const Section: string; Strings: TStrings); 
  end;

implementation

procedure TIniFileEx.ReadSectionValues2(const Section: string; Strings: TStrings);
var
  KeyList: TStringList;
  I: Integer;
  s: string;
begin
  KeyList := TStringList.Create;
  try
    ReadSection(Section, KeyList);
    Strings.BeginUpdate;
    try
      for I := 0 to KeyList.Count - 1 do
      begin
        s := ReadString(Section, KeyList[I], ''); if s = '' then s := KeyList[I];
        Strings.Values[KeyList[I]] := s;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    KeyList.Free;
  end;
end;

end.
