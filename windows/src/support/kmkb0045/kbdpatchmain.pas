(*
  Name:             kbdpatchmain
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jul 2008

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jul 2008 - mcdurdin - Initial version
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit kbdpatchmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ErrorControlledRegistry;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    chkDontWrite: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure Log(const msg: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  RegistryKeys, GetOSVersion;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  FInstalledKeyboards: TStringList;
  i: Integer;
  regKeyboards: TRegistryErrorControlled;  // I2890
  regSystem: TRegistryErrorControlled;  // I2890
  regSubstitutes: TRegistryErrorControlled;  // I2890
  FSubstitutes: TStringList;
  J: Integer;
  FCurrentKeyboardID: string;
  FNewKeyboardID: string;
  N: Integer;
  FWrite: Boolean;
begin
  Log('Started Utility');
  if GetOS in [osWin95, osWin98, osWinMe, osWinNT35, osWinNT4, osWin2000, osWinXP, osWin2003Server] then
  begin
    if MessageDlg('You should not run this utility on this version of Windows unless instructed '+
        'to do so by Keyman Support.  Do you want to continue?', mtConfirmation, mbOkCancel, 0) = mrCancel then
    begin
      Log('Cancelled - wrong OS version');
      Exit;
    end;
    Log('User chose to continue on old non-Vista OS version');
  end;
  // 1. Get the list of installed keyborarsd

  FInstalledKeyboards := TStringList.Create;
  FSubstitutes := TStringList.Create;
  regKeyboards := TRegistryErrorControlled.Create;  // I2890
  regSystem := TRegistryErrorControlled.Create;  // I2890
  regSubstitutes := TRegistryErrorControlled.Create;  // I2890
  try
    regSystem.RootKey := HKEY_LOCAL_MACHINE;
    if not regSystem.OpenKey('\'+SRegKey_KeyboardLayouts, False) then
    begin
      Log('Failed - unable to write to registry.  Run this program as Administrator.');
      Exit;
    end;

    regSubstitutes.OpenKey(SRegKey_KeyboardLayoutSubstitutes, False);
    regSubstitutes.GetValueNames(FSubstitutes);
    for I := 0 to FSubstitutes.Count - 1 do
      FSubstitutes[I] := FSubstitutes[I] + '=' + regSubstitutes.ReadString(FSubstitutes[I]);

    regKeyboards.RootKey := HKEY_LOCAL_MACHINE;
    if not regKeyboards.OpenKey(SRegKey_InstalledKeyboards, False) then
    begin
      Log('Finished - no installed Keyman keyboards found (unable to open registry key)');
      Exit;
    end;

    regKeyboards.GetKeyNames(FInstalledKeyboards);
    Log(IntToStr(FInstalledKeyboards.Count) + ' Keyman keyboards found');

    N := $A000; FWrite := not chkDontWrite.Checked;

    for i := 0 to FInstalledKeyboards.Count - 1 do
    begin
      if not regKeyboards.OpenKey('\'+SRegKey_InstalledKeyboards+'\'+FInstalledKeyboards[i], False) then
      begin
        Log('Failed - unable to open registry key '+'\'+SRegKey_InstalledKeyboards+'\'+FInstalledKeyboards[i]);
        Exit;
      end;
      if regKeyboards.ValueExists(SRegValue_KeymanKeyboardID) then
      begin
        FCurrentKeyboardID := regKeyboards.ReadString(SRegValue_KeymanKeyboardID);
        if (StrToIntDef('$'+FCurrentKeyboardID, $5FE) and $FFF) <> $5FE then
        begin
          Log('Keyboard '+FInstalledKeyboards[i]+' already has a valid ID.  Not moving.');
          Continue;
        end;

        Log('Keyboard '+FInstalledKeyboards[i]+' has ID '+FCurrentKeyboardID);

        { Locate the replacement key value }

        if not regSystem.OpenKey('\'+SRegKey_KeyboardLayouts, False) then
        begin
          Log('Failed - unable to write to registry.  Run this program as Administrator.');
          Exit;
        end;

        if (FNewKeyboardID = '') or FWrite then
        begin
          J := MAKELONG($0409, N);
          while regSystem.KeyExists(IntToHex(J,8)) do
          begin
            Inc(N); J := MAKELONG($0409, N);
          end;
        end
        else
        begin
          Inc(N);
          J := MAKELONG($0409, N);
        end;

        FNewKeyboardID := IntToHex(J,8);

        Log('Keyboard '+FInstalledKeyboards[i]+' has been allocated New ID of '+FNewKeyboardID);

        { Replace it - in loaded keyboards }
        if FWrite then
          regSystem.MoveKey(FCurrentKeyboardID, FNewKeyboardID, False);

        for J := 0 to FSubstitutes.Count - 1 do
        begin
          if CompareText(FSubstitutes.ValueFromIndex[J], FCurrentKeyboardID) = 0 then
          begin
            Log('System keyboard '+FSubstitutes[J]+' moving to new ID '+FNewKeyboardID);
            if FWrite then
            begin
              UnloadKeyboardLayout(LoadKeyboardLayout(PChar(FSubstitutes.Names[J]), KLF_NOTELLSHELL or KLF_SUBSTITUTE_OK));
              regSubstitutes.WriteString(FSubstitutes.Names[J], FNewKeyboardID);
              LoadKeyboardLayout(PChar(FSubstitutes.Names[J]), KLF_SUBSTITUTE_OK);
            end;
            FSubstitutes.ValueFromIndex[J] := FNewKeyboardID;
          end;
        end;

        if FWrite then
        begin
          regKeyboards.WriteString(SRegValue_KeymanKeyboardID, FNewKeyboardID);
          regSystem.DeleteKey(FCurrentKeyboardID);
        end;
      end
      else
        Log('Keyboard '+FInstalledKeyboards[i]+' does not have a system shadow keyboard');
    end;
  finally
    regSubstitutes.Free;
    regSystem.Free;
    regKeyboards.Free;
    FInstalledKeyboards.Free;
    FSubstitutes.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Log(const msg: string);
var
  s: string;
begin
  s := FormatDateTime('dd mmm yyyy hh:nn:ss', Now);
  Memo1.Lines.Add(s+'  '+msg);
end;

end.
