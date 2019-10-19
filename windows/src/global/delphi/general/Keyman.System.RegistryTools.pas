unit Keyman.System.RegistryTools;

interface

uses
  System.Win.Registry,
  Winapi.Windows;

type
  TRegistryTools = class
    class procedure CopyKey(Root: HKEY; const SourceKeyName, DestKeyName: string; CopySubKeys: Boolean); overload; static;
    class procedure CopyKey(regRead, regWrite: TRegistry; const SourceKeyName,
      DestKeyName: string; CopySubKeys: Boolean); overload; static;
  end;

implementation

uses
  System.Classes;

{ TRegistryTools }

class procedure TRegistryTools.CopyKey(
  regRead, regWrite: TRegistry;
  const SourceKeyName, DestKeyName: string; CopySubKeys: Boolean);
var
  s: TStringList;
  i: Integer;
  sz: Integer;
  m: Pointer;
begin
  s := TStringList.Create;
  try
    if regRead.OpenKeyReadOnly(SourceKeyName) and regWrite.OpenKey(DestKeyName, True) then
    begin
      regRead.GetValueNames(s);
      for i := 0 to s.Count - 1 do
      begin
        case regRead.GetDataType(s[i]) of
          rdString: regWrite.WriteString(s[i], regRead.ReadString(s[i]));
          rdExpandString: regWrite.WriteExpandString(s[i], regRead.ReadString(s[i]));
          rdInteger: regWrite.WriteInteger(s[i], regRead.ReadInteger(s[i]));
          rdBinary:
            begin
              sz := regRead.GetDataSize(s[i]);
              m := AllocMem(sz);
              try
                regRead.ReadBinaryData(s[i], m^, sz);
                regWrite.WriteBinaryData(s[i], m^, sz);
              finally
                FreeMem(m);
              end;
            end;
        end;
      end;

      if CopySubKeys then
      begin
        regRead.GetKeyNames(s);
        regRead.CloseKey;
        regWrite.CloseKey;
        for i := 0 to s.Count - 1 do
          CopyKey(regRead, regWrite,
            SourceKeyName + '\' + s[i],
            DestKeyName + '\' + s[i],
            True);
      end;
    end;
  finally
    s.Free;
  end;
end;

class procedure TRegistryTools.CopyKey(Root: HKEY; const SourceKeyName,
  DestKeyName: string; CopySubKeys: Boolean);
var
  regRead, regWrite: TRegistry;
begin
  regRead := TRegistry.Create;
  regWrite := TRegistry.Create;
  try
    regRead.RootKey := Root;
    regWrite.RootKey := Root;
    CopyKey(regRead, regWrite, SourceKeyName, DestKeyName, CopySubKeys);
  finally
    regRead.Free;
    regWrite.Free;
  end;
end;

end.
