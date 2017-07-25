unit UtilWaitForTSF;

interface

uses
  keymanapi_tlb;

type
  TWaitForTSF = class
    class procedure WaitForLanguageProfilesToBeApplied(InstalledKeyboards: array of IKeymanKeyboardInstalled; Timeout: Cardinal = 5000); overload; static;
    class procedure WaitForLanguageProfilesToBeApplied(InstalledKeyboard: IKeymanKeyboardInstalled; Timeout: Cardinal = 5000); overload; static;

  end;

implementation

uses
  System.SyncObjs,
  Vcl.Controls,
  Vcl.Forms,
  Winapi.Windows,
  kmint;

class procedure TWaitForTSF.WaitForLanguageProfilesToBeApplied(InstalledKeyboard: IKeymanKeyboardInstalled; Timeout: Cardinal = 5000);
var
  FKeyboards: array of IKeymanKeyboardInstalled;
begin
  SetLength(FKeyboards, 1);
  FKeyboards[0] := InstalledKeyboard;
  WaitForLanguageProfilesToBeApplied(FKeyboards, Timeout);
end;

class procedure TWaitForTSF.WaitForLanguageProfilesToBeApplied(InstalledKeyboards: array of IKeymanKeyboardInstalled; Timeout: Cardinal = 5000);
var
  i, j, n: Integer;
  InstalledLanguages: array of IKeymanKeyboardLanguageInstalled;
  StartTime: Cardinal;
  LastCursor: TCursor;
  FEvent: TEvent;
  FHandle: THandle;
begin
  // Because language profiles are refreshed asynchronously, TSF may not
  // report on the language profiles immediately. We'll spin on processmessages
  // until the profile is available, or for 5 seconds.

  // This function will lose all hotkeys set but not applied, so apply first

  FEvent := TEvent.Create;
  LastCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    kmcom.Languages.Refresh;

    n := 0;
    for i := 0 to High(InstalledKeyboards) do
      Inc(n, InstalledKeyboards[i].Languages.Count);

    SetLength(InstalledLanguages, n);
    n := 0;
    for i := 0 to High(InstalledKeyboards) do
    begin
      for j := 0 to InstalledKeyboards[i].Languages.Count - 1 do
      begin
        InstalledLanguages[n] := InstalledKeyboards[i].Languages[j];
        Inc(n);
      end;
    end;

    // Do all keyboards have a corresponding associated language?

    StartTime := GetTickCount;
    repeat
      n := 0;
      for i := 0 to High(InstalledLanguages) do
      begin
        for j := 0 to kmcom.Languages.Count - 1 do
          if kmcom.Languages[j].KeymanKeyboardLanguage = InstalledLanguages[i] then
          begin
            Inc(n);
            Break;
          end;
      end;
      if n >= Length(InstalledLanguages) then
        Exit;

      FHandle := FEvent.Handle;
      MsgWaitForMultipleObjectsEx(1, FHandle, 500, QS_ALLINPUT, 0);
      Application.ProcessMessages;
    until (GetTickCount - StartTime > 5000) or (GetTickCount < StartTime);  // 49.7 days!
  finally
    FEvent.Free;
    Screen.Cursor := LastCursor;
  end;
end;

end.
