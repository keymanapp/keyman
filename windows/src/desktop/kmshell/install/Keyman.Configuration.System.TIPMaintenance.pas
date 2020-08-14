unit Keyman.Configuration.System.TIPMaintenance;

interface

uses
  KeymanAPI_TLB;

type
  TTIPMaintenance = class
  public
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
  lang.InstallTip(LangID, KeyboardToRemove);
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
  lang.RegisterTip(LangID);
  Result := True;
end;

class function TTIPMaintenance.DoInstall(const KeyboardID,
  BCP47Tag: string): Boolean;
var
  lang: IKeymanKEyboardLanguageInstalled;
  RegistrationRequired: WordBool;
  TemporaryKeyboardID: WideString;
  LangID: Integer;
begin
  // TODO: normalize tag

  lang := GetKeyboardLanguage(KeyboardID, BCP47Tag);
  if lang = nil then
    // TODO: add the language
    Exit(False);

  // TODO: define flags
  TemporaryKeyboardID := '';
  LangID := 0;
  RegistrationRequired := False;

  if lang.FindInstallationLangID(LangID, TemporaryKeyboardID, RegistrationRequired, 1) then
  begin
    if RegistrationRequired then
    begin
      WaitForElevatedConfiguration(0, '-register-tip '+IntToHex(LangID,4)+' "'+KeyboardID+'" "'+lang.BCP47Code+'"');
    end;
    TUtilExecute.WaitForProcess('"'+ParamStr(0)+'" -install-tip '+IntToHex(LangID,4)+' "'+KeyboardID+'" "'+lang.BCP47Code+'" "'+TemporaryKeyboardID+'"', GetCurrentDir);
  end;

  Result := True;
end;

class function TTIPMaintenance.GetFirstLanguage(Keyboard: IKeymanKeyboardInstalled): string;
begin
  Result := Keyboard.DefaultBCP47Languages;
  if Result.Contains(' ') then
    Result := Result.Split([' '])[0];
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

  Exit(nil);
end;

end.
