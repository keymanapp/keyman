(*
  Name:             UfrmOSKFontHelper
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    25 Sep 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version I1374
                    20 Jul 2008 - mcdurdin - I1533 - Show hint for non-Unicode keyboards
                    29 Mar 2010 - mcdurdin - I2199 - Shift+click
                    24 Jun 2010 - mcdurdin - I2421 - Start work on font helper showing additional detail
                    26 Jul 2010 - mcdurdin - Code tidy - remove old commented-out code
                    17 Dec 2010 - mcdurdin - I2570 - Upgrade E-mbeddedWB (also I2393)
                    18 Feb 2011 - mcdurdin - I2721 - Override Javascript-disabled security for web controls
                    18 Feb 2011 - mcdurdin - I2712 - SMP support for font helper
                    18 Mar 2011 - mcdurdin - I1698, I2120, I2323, I2565 - Font helper crash when searching
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    24 Jan 2012 - mcdurdin - I3216 - Crash when F5 pressed in OSK font helper context
                    05 Jul 2012 - mcdurdin - I3390 - Font helper can crash when WM_FONTCHANGE received [CrashID:keyman.exe_8.0.350.0_2C53A3AE_EAccessViolation]
                    03 Nov 2012 - mcdurdin - I3519 - V9.0 - Merge of I3390 - Font helper can crash when WM_FONTCHANGE received
                    03 Nov 2012 - mcdurdin - I3520 - V9.0 - Merge of I3216 - Crash when F5 pressed in OSK font helper context
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    25 Sep 2014 - mcdurdin - I4412 - V9.0 - Character Map needs to insert characters using SendInput
*)
unit UfrmOSKFontHelper;  // I3306

interface

uses
  System.Contnrs,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmOSKPlugInBase,
  keymanapi_TLB, UfrmKeymanBase, utilcheckfonts,
  UserMessages, Vcl.Grids, Vcl.ExtCtrls;

type
  TKeyboardProps = record
    KeyboardName: WideString;
    UsageFileName: WideString;
    HasOSK: Boolean;
    HasWelcome: Boolean;
  end;

  TfrmOSKFontHelper = class(TfrmOSKPlugInBase)
    panNoKeyboard: TPanel;
    panFonts: TPanel;
    gridFonts: TDrawGrid; // I2721
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCharsData: array of string;
    FSelectedKeyboard: TCheckFontKeyboard;
    FCheckFontsThread: TCheckFontsThread;
    FCheckFontKeyboards: TCheckFontKeyboards;
    FLastSelectedKeyboardID: WideString;
    FLastSelectedKeymanID: Integer;
    FLastSelectedKeyboardName: WideString;

    procedure WMUser_FontChange(var Message: TMessage); message WM_USER_FontChange;  // I3390   // I3519

    procedure CMFontChange(var Message: TMessage); message CM_FONTCHANGE;
    procedure FireCommand(const command: WideString; params: TStringList);
    procedure CheckFontsThreadComplete(Sender: TObject);
    procedure DisplayKeyboardFonts;
    procedure StartCheckingFonts(Keyboard: IKeymanKeyboardInstalled);
    procedure SetDisplay(const msg: string);
  public
    { Public declarations }
    procedure SelectKeyboard(KeymanID: Integer);
  end;

implementation

uses
  findfonts,
  KLog,
  kmint,
  custinterfaces,
  UfrmKeyman7Main,
  UfrmVisualKeyboard,
  Unicode,
  USendInputString;

{$R *.dfm}

{ TfrmOSKFontHelper }

procedure TfrmOSKFontHelper.SelectKeyboard(KeymanID: Integer);
var
  i: Integer;
  kbds: IKeymanKeyboardsInstalled;
begin
  if KeymanID <> -1 then
  begin
    kbds := kmcom.Keyboards;
    for i := 0 to kbds.Count - 1 do
      if kbds[i].KeymanID = KeymanID then
      begin
        // Get fonts for the keyboard
        StartCheckingFonts(kbds[i]);
        kbds := nil;
        Exit;
      end;
  end;

  FLastSelectedKeyboardName := '';
  FLastSelectedKeyboardID := '';
  FLastSelectedKeymanID := -1;
  DisplayKeyboardFonts; // Displays default details
end;

procedure TfrmOSKFontHelper.FireCommand(const command: WideString; params: TStringList);
var
  hwnd: THandle;
  text: string;
  ch: WideString;
  n: Integer;
  s: WideString;
  v: Integer;
begin
//  if command = 'link' then TUtilExecute.URL(params.Values['url'])  // I3349
//  else if command = 'tutorial' then //(ActiveProduct as IKeymanProduct2).OpenTutorial
  if command = 'osk' then frmKeyman7Main.frmVisualKeyboard.ActivePage := apKeyboard
  else if command = 'welcome' then frmKeyman7Main.MnuOpenKeyboardHelp(nil)
  else if command = 'help' then frmKeyman7Main. MnuOpenProductHelp(frmKeyman7Main.frmVisualKeyboard)
  else if command = '' then

  else if command = 'insertchars' then
  begin
    hwnd := kmcom.Control.LastFocusWindow;

    // TODO: something like this will be issued by the grid in the future

    ch := Trim(params.Values['chars']);

    // I1429 - fix U+xxxx in insertchars URL

    n := Pos(' ', ch); if n = 0 then n := Length(ch)+1;

    while ch <> '' do
    begin
      s := Copy(ch, 1, n-1); if s = '' then Break;

      if Copy(s,1,2) = 'U+' then v := StrToIntDef('$'+Copy(s,3,6),0)
      else if s[1] = 'x' then v := StrToIntDef('$'+Copy(s,2,6),0)
      else if s[1] = 'd' then v := StrToIntDef(Copy(s,2,7), 0)
      else v := 0;

      if (v > 32) and (v <= $10FFFF) then
      begin
        if Uni_IsSurrogate(v)
          then text := text + Uni_UTF32ToSurrogate1(v) + Uni_UTF32ToSurrogate2(v)
          else text := text + Char(v);
      end;

      Delete(ch, 1, n); ch := Trim(ch);
      n := Pos(' ', ch);
      if n = 0 then n := Length(ch)+1;
    end;

    if text <> '' then
      SendInputString(hwnd, text);   // I4412
  end;
end;

procedure TfrmOSKFontHelper.FormCreate(Sender: TObject);
begin
  inherited;
  FCheckFontKeyboards := TCheckFontKeyboards.Create;
end;

procedure TfrmOSKFontHelper.FormDestroy(Sender: TObject);
begin
  inherited;
  if Assigned(FCheckFontsThread) then  // I1698, I2120, I2323, I2565
  begin
    FCheckFontsThread.OnTerminate := nil;
    FCheckFontsThread.Terminate;
    FCheckFontsThread := nil;
  end;

  FreeAndNil(FCheckFontKeyboards);
end;

procedure TfrmOSKFontHelper.CheckFontsThreadComplete(Sender: TObject);
var
  i: Integer;
begin
  FCheckFontsThread.Keyboards.OwnsObjects := False;
  for i := 0 to FCheckFontsThread.Keyboards.Count - 1 do
    FCheckFontKeyboards.Add(FCheckFontsThread.Keyboards[i]);

  if FLastSelectedKeyboardID = '' then
    DisplayKeyboardFonts
  else if FCheckFontKeyboards.Keyboards[FLastSelectedKeyboardID] = nil then
    SelectKeyboard(FLastSelectedKeymanID)
  else
    DisplayKeyboardFonts;

  FCheckFontsThread := nil;  // I3390 - moved from above to ensure thread doesn't restart until really ready (appears render can be re-entrant)   // I3519
end;

procedure TfrmOSKFontHelper.CMFontChange(var Message: TMessage);
begin
  PostMessage(Handle, WM_USER_FontChange, 0, 0);  // I3390 - This is via a SendMessage which locks the sending process, so we'll accept it and requery in our own time   // I3519
end;

procedure TfrmOSKFontHelper.WMUser_FontChange(var Message: TMessage);  // I3390   // I3519
begin
  FCheckFontKeyboards.Clear;
  SelectKeyboard(FLastSelectedKeymanID);
end;

procedure TfrmOSKFontHelper.StartCheckingFonts(Keyboard: IKeymanKeyboardInstalled);
begin
  FLastSelectedKeyboardID := Keyboard.ID;
  FLastSelectedKeyboardName := Keyboard.Name;
  FLastSelectedKeymanID := Keyboard.KeymanID;

  if FCheckFontKeyboards.Keyboards[FLastSelectedKeyboardID] <> nil then
  begin
    DisplayKeyboardFonts;
    Exit;
  end;

  if Assigned(FCheckFontsThread) then Exit; // Still looking up previous keyboard fonts, it will be checked shortly

  if Keyboard.Encodings = keANSI then
  begin
    SetDisplay('The selected keyboard is not a Unicode keyboard');
  end
  else
  begin
    SetDisplay('Please wait while searching for related fonts for keyboard '+FLastSelectedKeyboardName);

    FCheckFontsThread := TCheckFontsThread.Create;
    FCheckFontsThread.FreeOnTerminate := True;
    FCheckFontsThread.OnTerminate := CheckFontsThreadComplete;
    FCheckFontsThread.AddKeyboard(Keyboard.ID, Keyboard.Filename, Keyboard.GetCharsUsed);
    FCheckFontsThread.Start;  // I3309
  end;
end;

procedure TfrmOSKFontHelper.DisplayKeyboardFonts;
var
  FKeyboard: TCheckFontKeyboard;
  J: Integer;
var
  i: Integer;
  ch,ch2: WideChar;
begin
  FKeyboard := FCheckFontKeyboards.Keyboards[FLastSelectedKeyboardID];
  if (FLastSelectedKeyboardID <> '') and Assigned(FKeyboard) then
  begin
    SetLength(FCharsData, Length(FKeyboard.Chars));
    J := 0;
    I := 1;
    while I <= Length(FKeyboard.Chars) do  // I2712
    begin
      ch := FKeyboard.Chars[I];
      if Uni_IsSurrogate1(ch) and (I < Length(FKeyboard.Chars)) and Uni_IsSurrogate2(FKeyboard.Chars[I+1]) then
      begin
        ch2 := FKeyboard.Chars[I+1];
        FCharsData[J] := ch + ch2;
        Inc(I);
      end
      else
        FCharsData[J] := ch;
      Inc(I);
      Inc(J);
    end;

    SetLength(FCharsData, J);

    gridFonts.ColCount := Length(FCharsData) + 1;
    gridFonts.RowCount := FKeyboard.Fonts.Count + 1;

    FSelectedKeyboard := FKeyboard;

(*    for I := 0 to FKeyboard.Fonts.Count - 1 do
    begin
      FFontsData := FFontsData + '<Font '+
        'Index="'+IntToStr(I)+'" '+
        'Name="'+XmlEncode(FKeyboard.Fonts[I].FontName)+'" '+
        'Coverage="'+IntToStr(FKeyboard.Fonts[i].Coverage)+'">'; //<IncludedChars>';

      FFontsData := FFontsData + '<ExcludedChars>';
      J := 1;
      while J <= Length(FKeyboard.Fonts[I].ExcludedChars) do  // I2712
      begin
        ch := FKeyboard.Fonts[I].ExcludedChars[J];
        if Uni_IsSurrogate1(ch) and (J < Length(FKeyboard.Fonts[I].ExcludedChars)) then
        begin
          ch2 := FKeyboard.Fonts[I].ExcludedChars[J+1];
          FFontsData := FFontsData + '<Ch CharCode="'+IntToHex(Uni_SurrogateToUTF32(ch,ch2),5)+'" />';
          Inc(J);
        end
        else
          FFontsData := FFontsData + '<Ch CharCode="'+CharCode(ch)+'" />';
        Inc(J);
      end;
      FFontsData := FFontsData + '</ExcludedChars>';
      FFontsData := FFontsData + '</Font>';
    end;

    FFontsData := FFontsData + '</Fonts><Keyboard Name="'+XmlEncode(FLastSelectedKeyboardName)+'" />';
    {finally
      FFonts.Free;
    end;}
*)
    SetDisplay('');
  end
  else
  begin
    if kmcom.Keyboards.Count = 0 then
      SetDisplay('No keyboards are installed')
    else
      SetDisplay('Please select a Keyman keyboard to find related fonts');
  end;
end;

procedure TfrmOSKFontHelper.SetDisplay(const msg: string);
begin
  if msg = '' then
  begin
    panFonts.Visible := True;
    panNoKeyboard.Visible := False;
  end
  else
  begin
    panNoKeyboard.Caption := msg;
    panNoKeyboard.Visible := True;
    panFonts.Visible := False;
  end;
end;

end.
