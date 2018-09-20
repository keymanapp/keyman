unit Keyman.Developer.System.ImportKeyboardMain;

interface

procedure Run;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Win.Registry,
  Winapi.Windows,

  Keyman.Developer.System.ImportKeyboardDLL;

procedure ShowKeyboardList; forward;

const
  CRootKey = 'System\CurrentControlSet\Control\Keyboard Layouts';

procedure Run;
var
  inputHKL, outputFileName: string;
  ikd: TImportKeyboardDLL;
  sl: TStringList;
  ss: TStringStream;
begin
  if ParamCount = 0 then
  begin
    writeln('Usage: importkeyboard /list | hkl [output.kmn] ');
    writeln(' /list shows a list of keyboards available on your system.');
    writeln(' hkl should be an 8 hex digit Keyboard ID.  See /list to enumerate these.');
    writeln(' If output.kmn is not specified, the filename of the source keyboard will be used; e.g. 00000409 will produce kbdus.kmn');
    Exit;
  end;

  if SameText(ParamStr(1), '/list') then
  begin
    ShowKeyboardList;
    Exit;
  end;

  inputHKL := ParamStr(1);
  if ParamCount > 1 then
    outputFileName := ParamStr(2);

  ikd := TImportKeyboardDLL.Create(inputHKL); //, outputFileName, '');
  try
    if outputFileName = '' then
      outputFileName := ikd.RecommendedOutputKMNFileName;
    writeln('Importing Windows system keyboard '+ikd.InputHKL+' to Keyman keyboard '+outputFileName);

    ikd.Execute;

    // Still need a BOM for .kmn ... for now
    sl := TStringList.Create;
    try
      sl.Text := ikd.KMN;
      sl.SaveToFile(outputFileName, TEncoding.UTF8);
    finally
      sl.Free;
    end;

    // No BOM for .kvks
//    ss := TStringStream.Create(ikd.KVKS, TEncoding.UTF8);
//    try
//      ss.SaveToFile(ChangeFileExt(outputFileName, '.kvks'));
//    finally
//      ss.Free;
//    end;
  finally
    ikd.Free;
  end;

end;

procedure ShowKeyboardList;
var
  r: TRegistry;
  keyboardIds: TStringList;
  keyboardId: string;
  layoutFile: string;
  layoutText: string;
begin
  r := TRegistry.Create;
  keyboardIds := TStringList.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    if r.OpenKeyReadOnly(CRootKey) then //TODO Use RegistryKeys
    begin
      r.GetKeyNames(keyboardIds);
      for keyboardId in keyboardIds do
      begin
        if r.OpenKeyReadOnly('\' + CRootKey + '\' + keyboardId) then
        begin
          if not r.ValueExists('keyman install') then
          begin
            layoutFile := r.ReadString('layout file');
            layoutText := r.ReadString('layout text');
            if (layoutFile <> '') and (layoutText <> '') then
            begin
              writeln('  ' + keyboardId + #9 + layoutFile + #9 + layoutText);
            end;
          end;
        end;
      end;
    end;
  finally
    r.Free;
    keyboardIds.Free;
  end;
end;

end.
