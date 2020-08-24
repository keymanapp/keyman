unit Keyman.Configuration.System.TIPMaintenance;

interface

uses
  KeymanAPI_TLB;

type
  TTIPMaintenance = class
  public
//    class function DoUninstall(const KeyboardID, BCP47Tag: string): Boolean;

    class function DoInstall(const KeyboardID, BCP47Tag: string): Boolean;
    class function RegisterTip(LangID: Integer; const KeyboardID, BCP47Tag: string): Boolean;
    class function InstallTip(LangID: Integer; const KeyboardID, BCP47Tag, KeyboardToRemove: string): Boolean;

    class function GetFirstLanguage(Keyboard: IKeymanKeyboardInstalled): string;
  private
    class function GetKeyboardLanguage(const KeyboardID,
      BCP47Tag: string): IKeymanKeyboardLanguageInstalled; static;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows,

  Keyman.System.LanguageCodeUtils,

  BCP47Tag,
  glossary,
  kmint,
  utilkmshell,
  utilexecute;

{ TTIPMaintenance }

class function TTIPMaintenance.InstallTip(LangID: Integer; const KeyboardID, BCP47Tag,
  KeyboardToRemove: string): Boolean;
var
  lang: IKeymanKeyboardLanguageInstalled;
begin
  lang := GetKeyboardLanguage(KeyboardID, BCP47Tag);
  if lang = nil then
    Exit(False);

  // TODO: handle errors
  (lang as IKeymanKeyboardLanguageInstalled2).InstallTip(LangID, KeyboardToRemove);
  Result := True;
end;

class function TTIPMaintenance.RegisterTip(LangID: Integer; const KeyboardID,
  BCP47Tag: string): Boolean;
var
  lang: IKeymanKeyboardLanguageInstalled;
begin
  lang := GetKeyboardLanguage(KeyboardID, BCP47Tag);
  if lang = nil then
    Exit(False);

  // TODO: handle errors
  (lang as IKeymanKeyboardLanguageInstalled2).RegisterTip(LangID);
  Result := True;
end;

class function TTIPMaintenance.DoInstall(const KeyboardID,
  BCP47Tag: string): Boolean;
var
  lang: IKeymanKeyboardLanguageInstalled;
  RegistrationRequired: WordBool;
  TemporaryKeyboardID: WideString;
  LangID: Integer;
begin
  lang := GetKeyboardLanguage(KeyboardID, TBCP47Tag.GetCanonicalTag(BCP47Tag));
  if lang = nil then
    // The keyboard was not found
    Exit(False);

  // After canonicalization, we may find the language is already installed
  if lang.IsInstalled then
    Exit(True);

  TemporaryKeyboardID := '';
  LangID := 0;
  RegistrationRequired := False;

  if (lang as IKeymanKeyboardLanguageInstalled2).FindInstallationLangID(LangID, TemporaryKeyboardID, RegistrationRequired, kifInstallTransitoryLanguage) then
  begin
    if RegistrationRequired then
    begin
      WaitForElevatedConfiguration(0, '-register-tip '+IntToHex(LangID,4)+' "'+KeyboardID+'" "'+lang.BCP47Code+'"');
    end;
    TUtilExecute.WaitForProcess('"'+ParamStr(0)+'" -install-tip '+IntToHex(LangID,4)+' "'+KeyboardID+'" "'+lang.BCP47Code+'" "'+TemporaryKeyboardID+'"', GetCurrentDir);
  end;

  Result := True;
end;

function GetDefaultHKL: HKL;   // I3581   // I3619   // I3619
begin
  if not SystemParametersInfo(SPI_GETDEFAULTINPUTLANG, 0, @Result, 0) then
    Result := 0;
end;

class function TTIPMaintenance.GetFirstLanguage(Keyboard: IKeymanKeyboardInstalled): string;
begin
  if Keyboard.Languages.Count > 0 then
    Result := Keyboard.Languages[0].BCP47Code
  else
  begin
    Result := TLanguageCodeUtils.TranslateWindowsLanguagesToBCP47(HKLToLanguageID(GetDefaultHKL));
  end;
end;

class function TTIPMaintenance.GetKeyboardLanguage(const KeyboardID,
  BCP47Tag: string): IKeymanKeyboardLanguageInstalled;
var
  keyboard: IKeymanKeyboardInstalled;
  i: Integer;
begin
  keyboard := kmcom.Keyboards[KeyboardID];
  if keyboard = nil then
    Exit(nil);

  for i := 0 to keyboard.Languages.Count - 1 do
    if SameText(keyboard.Languages[i].BCP47Code, BCP47Tag) then
      Exit(keyboard.Languages[i]);

  Result := (keyboard.Languages as IKeymanKeyboardLanguagesInstalled2).Add(BCP47Tag);
end;

end.
