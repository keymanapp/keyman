unit Keyman.Configuration.UI.InstallFile;

interface

uses
  System.Classes,

  keymanapi_tlb;

type
  TInstallFile = class sealed
  private
    class procedure AddDefaultLanguageHotkey(
      Keyboard: IKeymanKeyboardInstalled); static;
    class procedure AddDefaultLanguageHotkeys(
      InstalledKeyboards: array of IKeymanKeyboardInstalled); static;
    class procedure InstallKeyboardPackageLanguage(
      FPackage: IKeymanPackageInstalled; const BCP47: string); static;
  public
    class function BrowseAndInstallKeyboardFromFile(Owner: TComponent): Boolean; static;
    class function Execute(KeyboardFileNames: TStrings; const FirstKeyboardFileName: string; FSilent, FNoWelcome: Boolean; const LogFile: string): Boolean; overload; static;
    class function Execute(Owner: TComponent; const FileName: string; ASilent, ANoWelcome: Boolean; const LogFile, BCP47: string): Boolean; overload; static;
    class function Execute(Owner: TComponent; const FileNames: TStrings; ASilent: Boolean): Boolean; overload; static;
  end;

implementation

uses
  System.SysUtils,
  System.Win.ComObj,
  Vcl.Controls,
  Vcl.Dialogs,

  Keyman.Configuration.System.TIPMaintenance,
  Keyman.Configuration.UI.KeymanProtocolHandler,
  Keyman.Configuration.UI.MitigationForWin10_1803,
  kmint,
  UfrmHTML,
  UfrmInstallKeyboard;

class function TInstallFile.Execute(KeyboardFileNames: TStrings; const FirstKeyboardFileName: string; FSilent, FNoWelcome: Boolean;
  const LogFile: string): Boolean;
begin
  if KeyboardFileNames.Count > 1 then
  begin
    Result := TInstallFile.Execute(nil, KeyboardFileNames, FSilent)
  end
  else if TKeymanProtocolHandler.CanHandle(FirstKeyboardFileName) then
  begin
    Result := TKeymanProtocolHandler.Handle(nil, FirstKeyboardFileName, FSilent, FNoWelcome, LogFile);
  end
  // TODO: support bare package ids from command line (if it does not include a file extension, assume it is a .kmp and try and download it)
//  else if IsNotPackageOrKeyboardFile then
  else
  begin
    Result := TInstallFile.Execute(nil, FirstKeyboardFileName, FSilent, FNoWelcome, LogFile, ''); // TODO: support BCP 47 from command line
  end;
end;

class function TInstallFile.Execute(Owner: TComponent; const FileName: string; ASilent, ANoWelcome: Boolean; const LogFile, BCP47: string): Boolean;
var
  n: Integer;
  InstalledKeyboards: array of IKeymanKeyboardInstalled;
  InstalledPackage: IKeymanPackageInstalled;
  i: Integer;
begin
  // TODO: refactor the UI out of this unit into a wrapper unit
  with TfrmInstallKeyboard.Create(Owner) do
  try
    Silent := ASilent;
    InstallFile := FileName;
//TODO:    DefaultBCP47 := BCP47;
    if ModalResult = mrCancel then
      // failed to start install
      Result := False
    else
    begin
      if ASilent then
      begin
        InstallKeyboard(LogFile);
        Result := True;
      end
      else
        Result := ShowModal = mrOk;
    end;
  finally
    Free;
  end;

  if not Result then
    Exit;

  kmcom.Keyboards.Refresh;
  kmcom.Keyboards.Apply;
  kmcom.Packages.Refresh;

  InstalledPackage := nil;
  SetLength(InstalledKeyboards, 0);
  if LowerCase(ExtractFileExt(FileName)) = '.kmp' then
  begin
    n := kmcom.Packages.IndexOf(FileName);
    if (n >= 0) then
    begin
      InstalledPackage := kmcom.Packages[n];
      SetLength(InstalledKeyboards, InstalledPackage.Keyboards.Count);
      for i := 0 to InstalledPackage.Keyboards.Count - 1 do
        InstalledKeyboards[i] := InstalledPackage.Keyboards[i] as IKeymanKeyboardInstalled;
    end;
  end
  else
  begin
    n := kmcom.Keyboards.IndexOf(FileName);
    if n >= 0 then
    begin
      SetLength(InstalledKeyboards, 1);
      InstalledKeyboards[0] := kmcom.Keyboards[n];
    end;
  end;

  kmcom.Languages.Apply;
  AddDefaultLanguageHotkeys(InstalledKeyboards);

  if InstalledPackage <> nil then
  begin
    //if not ASilent then SelectLanguage(False);
    if not ASilent and not ANoWelcome then
      DoShowPackageWelcome(InstalledPackage, False);
  end;

  {$MESSAGE HINT 'How do we correlate this with a Cancel in configuration? Do we change that to Close?' }
  kmcom.Apply;
end;

/// <summary>
/// Takes a list of filename + bcp47 pairs (in filename=bcp47 format) and
/// installs each package / keyboard with the associated bcp47 language. If a
/// bcp47 value is not provided, then the default language for the package is
/// installed. If a package contains multiple keyboards, then bcp47 should not
/// be provided, and will be ignored if it is.
/// This is the handler for the `-i` parameter, e.g.
///   kmshell -i khmer_angkor.kmp c:\temp\sil_euro_latin.kmp=fr
/// </summary>
class function TInstallFile.Execute(Owner: TComponent; const FileNames: TStrings; ASilent: Boolean): Boolean;
var
  i, j: Integer;
  FPackage: IKeymanPackageInstalled;
  FKeyboard: IKeymanKeyboardInstalled;
  Filename: string;
  FilenameBCP47: TArray<string>;
  IsPackage: Boolean;
begin
  Result := False;
  FPackage := nil;
  FKeyboard := nil;

  for i := 0 to FileNames.Count - 1 do
  begin
    try
      kmcom.Keyboards.Refresh;

      FilenameBCP47 := Filenames[i].Split(['=']);
      Filename := FilenameBCP47[0];
      IsPackage := AnsiSameText(ExtractFileExt(FileName), '.kmp');

      if IsPackage then
      begin
        FPackage := (kmcom.Packages as IKeymanPackagesInstalled2).Install2(FileName, True);
        if Length(FilenameBCP47) > 1
          then InstallKeyboardPackageLanguage(FPackage, FilenameBCP47[1])
          else InstallKeyboardPackageLanguage(FPackage, '');
      end
      else
      begin
        FKeyboard := (kmcom.Keyboards as IKeymanKeyboardsInstalled2).Install2(FileName, True);
        // TODO: the split between user and lm here is a real pain; we need to handle this cleanly
        if Length(FilenameBCP47) > 1
          then TTIPMaintenance.DoInstall(FKeyboard.ID, FilenameBCP47[1])
          else TTIPMaintenance.DoInstall(FKeyboard.ID, TTIPMaintenance.GetFirstLanguage(FKeyboard))
      end;
      CheckForMitigationWarningFor_Win10_1803(ASilent, '');
    except
      on E:EOleException do
      begin
        if kmcom.Errors.Count = 0 then Raise;
        if not ASilent then
          for j := 0 to kmcom.Errors.Count - 1 do
            ShowMessage(kmcom.Errors[j].Description);
        Exit;
      end;
    end;

    FPackage := nil;
    FKeyboard := nil;
  end;

  kmcom.Keyboards.Refresh;
  kmcom.Keyboards.Apply;
  Result := True;
end;

class function TInstallFile.BrowseAndInstallKeyboardFromFile(Owner: TComponent): Boolean;
var
  dlgOpen: TOpenDialog;
begin
  dlgOpen := TOpenDialog.Create(nil);
  try
    dlgOpen.Filter :=
      'Keyman files (*.kmx, *.kxx, *.kmp)|*.kmx;*.kxx;*.kmp|Keyman keyboards (*.kmx,*.kxx)' +
      '|*.kmx;*.kxx|Keyman packages (*.kmp)|*.kmp|All files (*.*)|*.*';
    dlgOpen.Title := 'Install Keyman Keyboard';

    if dlgOpen.Execute then
      Result := Execute(Owner, dlgOpen.FileName, False, False, '', '')
    else
      Result := False;
  finally
    dlgOpen.Free;
  end;
end;

class procedure TInstallFile.InstallKeyboardPackageLanguage(FPackage: IKeymanPackageInstalled; const BCP47: string);
var
  DefaultBCP47Language: string;
  i: Integer;
begin
  if (FPackage.Keyboards.Count > 1) or (BCP47 = '') then
  begin
    // We don't attempt to propagate the language association preference
    // when there is more than one keyboard in the package. This means a
    // little bit of nasty hoop jumping in order to get the default
    // language code for each keyboard.
    for i := 0 to FPackage.Keyboards.Count - 1 do
    begin
      DefaultBCP47Language := FPackage.Keyboards[i].DefaultBCP47Languages;
      if DefaultBCP47Language.Contains(' ') then
        DefaultBCP47Language := DefaultBCP47Language.Split([' '])[0];
      TTIPMaintenance.DoInstall(FPackage.Keyboards[i].ID, DefaultBCP47Language);
    end;
  end
  else if FPackage.Keyboards.Count > 0 then
    TTIPMaintenance.DoInstall(FPackage.Keyboards[0].ID, BCP47);
end;

class procedure TInstallFile.AddDefaultLanguageHotkey(Keyboard: IKeymanKeyboardInstalled);
var
  i: Integer;
begin
  if not Keyboard.DefaultHotkey.IsEmpty and (Keyboard.Languages.Count > 0) then
  begin
    for i := 0 to kmcom.Languages.Count - 1 do
    begin
      if kmcom.Languages[i].KeymanKeyboardLanguage = Keyboard.Languages[0] then
      begin
        kmcom.Languages[i].Hotkey.RawValue := Keyboard.DefaultHotkey.RawValue;
        Break;
      end;
    end;
  end;
end;

class procedure TInstallFile.AddDefaultLanguageHotkeys(InstalledKeyboards: array of IKeymanKeyboardInstalled);
var
  i: Integer;
begin
  for i := 0 to High(InstalledKeyboards) do
    AddDefaultLanguageHotkey(InstalledKeyboards[i]);
end;

end.
