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
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.Types,
  System.Variants,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,
  Winapi.Messages,
  Winapi.Windows,

  ClearTypeDrawCharacter,
  keymanapi_TLB,
  UfrmOSKPlugInBase,
  UserMessages,
  utilcheckfonts;

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
    grid: TDrawGrid;
    panControls: TPanel;
    tbSize: TTrackBar; // I2721
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure tbSizeChange(Sender: TObject);
    procedure gridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure gridMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure gridMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure gridDblClick(Sender: TObject);
  private
    FChars: array of Cardinal; // UTF-32 codepoints
    FDrawChar: TCleartypeDrawCharacter;
    FDrawCellBitmap: Vcl.Graphics.TBitmap;

    FSelectedKeyboard: TCheckFontKeyboard;
    FCheckFontsThread: TCheckFontsThread;
    FCheckFontKeyboards: TCheckFontKeyboards;
    FLastSelectedKeyboardID: WideString;
    FLastSelectedKeymanID: Integer;

    procedure WMUser_FontChange(var Message: TMessage); message WM_USER_FontChange;  // I3390   // I3519

    procedure CMFontChange(var Message: TMessage); message CM_FONTCHANGE;
    procedure CheckFontsThreadComplete(Sender: TObject);
    procedure DisplayKeyboardFonts;
    procedure StartCheckingFonts(Keyboard: IKeymanKeyboardInstalled);
    procedure SetDisplay(const msg: string);
    procedure FormatGrid;
    function GetFontSize: Integer;
    procedure InsertCharacter;
  public
    { Public declarations }
    procedure SelectKeyboard(KeymanID: Integer);
  end;

implementation

uses
  System.Math,

  findfonts,
  kmint,
  MessageIdentifiers,
  MessageIdentifierConsts,
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
  FSelectedKeyboard := nil;

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

  FLastSelectedKeyboardID := '';
  FLastSelectedKeymanID := -1;
  DisplayKeyboardFonts; // Displays default details
end;

procedure TfrmOSKFontHelper.FormCreate(Sender: TObject);
begin
  inherited;
  FCheckFontKeyboards := TCheckFontKeyboards.Create;
  FDrawCellBitmap := Vcl.Graphics.TBitmap.Create;
  FDrawChar := TCleartypeDrawCharacter.Create;
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
  FreeAndNil(FDrawCellBitmap);
  FreeAndNil(FDrawChar);
end;

procedure TfrmOSKFontHelper.gridDblClick(Sender: TObject);
begin
  InsertCharacter;
end;

procedure TfrmOSKFontHelper.gridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  FFontInfo: TFindFontResult;
  RectBmp: TRect;
  uc: Integer;
  ch: string;
begin
  if not Assigned(FSelectedKeyboard) or (FSelectedKeyboard.Fonts.Count <= ARow) then
    Exit;

  FFontInfo := FSelectedKeyboard.Fonts[ARow];

  if ACol = 0 then
  begin
    // Font name
    grid.Canvas.Font := grid.Font;
    grid.Canvas.TextRect(Rect, Rect.Left, (Rect.Bottom + Rect.Top - grid.Canvas.TextHeight('A')) div 2, FFontInfo.FontName)
  end
  else if ACol = 1 then
  begin
    // % Coverage
    grid.Canvas.Font := grid.Font;
    grid.Canvas.TextRect(Rect, Rect.Left, (Rect.Bottom + Rect.Top - grid.Canvas.TextHeight('A')) div 2, Format('%d%%', [FFontInfo.Coverage]))
  end
  else
  begin
//    grid.Canvas.Font.Name := FFontInfo.FontName;
//    grid.Canvas.TextRect(Rect, Rect.Left, Rect.Top, FCharsData[ACol-2]);
      { Draw a character cell }

    RectBmp := System.Types.Rect(0, 0, Rect.Right-Rect.Left, Rect.Bottom-Rect.Top);
    with FDrawCellBitmap.Canvas do
    begin
      { Draw a character cell }
      if gdSelected in State then
      begin
        if gdFocused in State then
        begin
          Font.Color := clHighlightText;
          Brush.Color := clHighlight;
        end
        else
        begin
          Font.Color := clHighlightText;
          Brush.Color := clGray;
        end;
      end
      else
      begin
        Font.Color := clWindowText;
        Brush.Color := clWindow;
      end;
      FillRect(RectBmp);

      { Retrieve character }

      uc := FChars[ACol-2];

      if uc = 0 then
      begin
        grid.Canvas.Brush.Color := clWindow;
        grid.Canvas.FillRect(Rect);
        Exit;
      end;

      ch := Uni_UTF32CharToUTF16(uc);

      FDrawChar.SetFontDetails(FFontInfo.FontName, GetFontSize);   // I4807
      FDrawChar.Color := Font.Color;
//      case DisplayQuality of
//        NONANTIALIASED_QUALITY: FDrawChar.DisplayQuality := ctPlain;
//        ANTIALIASED_QUALITY: FDrawChar.DisplayQuality := ctAntialias;
//        else
        FDrawChar.DisplayQuality := ctCleartype;
//      end;
      FDrawChar.DrawText(Handle, TA_CENTER or TA_TOP, RectBmp.Right div 2, 0, RectBmp, ch);

      { Draw U+nnnn }

      if grid.DefaultColWidth > 35 then
      begin
        Font.Size := 7;
        Font.Name := 'Arial';
        SetTextAlign(Handle, TA_CENTER or TA_TOP);
        TextOut(RectBmp.Right div 2, RectBmp.Bottom - TextHeight('a'), 'U+'+IntToHex(uc,4));
      end;

      { Draw grid }

      Pen.Color := $c0c0c0;
      MoveTo(RectBmp.Right-1, 0);
      LineTo(RectBmp.Right-1, RectBmp.Bottom);
      MoveTo(0, RectBmp.Bottom-1);
      LineTo(RectBmp.Right, RectBmp.Bottom-1);
    end;

    grid.Canvas.Draw(Rect.Left, Rect.Top, FDrawCellBitmap);
  end;
end;

procedure TfrmOSKFontHelper.gridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 107 { Numpad + } then
  begin
    if tbSize.Position < tbSize.Max then
      tbSize.Position := tbSize.Position + tbSize.PageSize;
  end
  else if Key = 109 { Numpad - } then
  begin
    if tbSize.Position > tbSize.Min then
      tbSize.Position := tbSize.Position - tbSize.PageSize;
  end
  else if Key = VK_RETURN then
    InsertCharacter
  else
    Exit;
  Key := 0;
end;

procedure TfrmOSKFontHelper.InsertCharacter;
var
  ch: string;
  hwnd: THandle;
begin
  if grid.Col < 2 then
    Exit;

  if not Assigned(FSelectedKeyboard) then
    Exit;

  if grid.Col - 2 >= Length(FChars) then
    Exit;

  ch := Uni_UTF32CharToUTF16(FChars[grid.Col-2]);

  hwnd := kmcom.Control.LastFocusWindow;
  SendInputString(hwnd, ch);   // I4412
end;

procedure TfrmOSKFontHelper.gridMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    Handled := True;
    if tbSize.Position < tbSize.Max then
      tbSize.Position := tbSize.Position + tbSize.PageSize;
  end;
end;

procedure TfrmOSKFontHelper.gridMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
  begin
    Handled := True;
    if tbSize.Position > tbSize.Min then
      tbSize.Position := tbSize.Position - tbSize.PageSize;
  end;
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
  FSelectedKeyboard := nil;
  FCheckFontKeyboards.Clear;
  SelectKeyboard(FLastSelectedKeymanID);
end;

procedure TfrmOSKFontHelper.StartCheckingFonts(Keyboard: IKeymanKeyboardInstalled);
begin
  FLastSelectedKeyboardID := Keyboard.ID;
  FLastSelectedKeymanID := Keyboard.KeymanID;

  if FCheckFontKeyboards.Keyboards[FLastSelectedKeyboardID] <> nil then
  begin
    DisplayKeyboardFonts;
    Exit;
  end;

  if Assigned(FCheckFontsThread) then Exit; // Still looking up previous keyboard fonts, it will be checked shortly

  if Keyboard.Encodings = keANSI then
  begin
    SetDisplay(MsgFromIdFormat(S_OSK_FontHelper_NonUnicode, [Keyboard.Name]));
  end
  else
  begin
    SetDisplay(MsgFromIdFormat(S_OSK_FontHelper_PleaseWait, [Keyboard.Name]));

    FCheckFontsThread := TCheckFontsThread.Create;
    FCheckFontsThread.FreeOnTerminate := True;
    FCheckFontsThread.OnTerminate := CheckFontsThreadComplete;
    FCheckFontsThread.AddKeyboard(Keyboard.ID, Keyboard.Filename, Keyboard.GetCharsUsed);
    FCheckFontsThread.Start;  // I3309
  end;
end;

procedure TfrmOSKFontHelper.tbSizeChange(Sender: TObject);
begin
  FormatGrid;
end;

procedure TfrmOSKFontHelper.FormatGrid;
var
  i: Integer;
  m: Integer;
begin
  if FSelectedKeyboard.Fonts.Count = 0 then
  begin
    Exit;   // Exit as grid needs at least one font
  end;

  grid.DefaultColWidth := tbSize.Position;
  grid.DefaultRowHeight := tbSize.Position;

  Canvas.Font := grid.Font;

  m := 64;

  if Assigned(FSelectedKeyboard) then
    for i := 0 to grid.RowCount - 1 do
      m := System.Math.Max(m, Canvas.TextWidth(FSelectedKeyboard.Fonts[i].FontName) + 6);

  grid.ColWidths[0] := m;
  grid.ColWidths[1] := Canvas.TextWidth('100%') + 6;

  for i := 2 to grid.ColCount - 1 do
    grid.ColWidths[i] := tbSize.Position;
  grid.Invalidate;

  FDrawCellBitmap.Width := grid.DefaultColWidth;
  FDrawCellBitmap.Height := grid.DefaultRowHeight;
end;

procedure TfrmOSKFontHelper.DisplayKeyboardFonts;
var
  FKeyboard: TCheckFontKeyboard;
  J: Integer;
var
  i: Integer;
  ch,ch2: WideChar;
begin
  FSelectedKeyboard := nil;
  FKeyboard := FCheckFontKeyboards.Keyboards[FLastSelectedKeyboardID];
  if (FLastSelectedKeyboardID <> '') and Assigned(FKeyboard) then
  begin
    if FKeyboard.Fonts.Count > 0 then
    begin
      SetLength(FChars, Length(FKeyboard.Chars));
      J := 0;
      I := 1;
      while I <= Length(FKeyboard.Chars) do  // I2712
      begin
        ch := FKeyboard.Chars[I];
        if Uni_IsSurrogate1(ch) and (I < Length(FKeyboard.Chars)) and Uni_IsSurrogate2(FKeyboard.Chars[I+1]) then
        begin
          ch2 := FKeyboard.Chars[I+1];
          FChars[J] := Uni_SurrogateToUTF32(ch, ch2);
          Inc(I);
        end
        else
          FChars[J] := Ord(ch);
        Inc(I);
        Inc(J);
      end;

      SetLength(FChars, J);

      grid.ColCount := Length(FChars) + 2;
      grid.RowCount := FKeyboard.Fonts.Count;

      for i := 0 to FKeyboard.Fonts.Count - 1 do
        if FKeyboard.Fonts[i].Coverage < 50 then
        begin
          grid.RowCount := i;
          Break;
        end;

      FSelectedKeyboard := FKeyboard;
      FormatGrid;
      SetDisplay('');
    end
    else
    begin
       FSelectedKeyboard := FKeyboard;
       SetDisplay(MsgFromIdFormat(S_OSK_FontHelper_NoFonts, [FSelectedKeyboard.Name]));
    end;
  end
  else
  begin
    if kmcom.Keyboards.Count = 0 then
      SetDisplay(MsgFromId(S_OSK_FontHelper_NoKeyboards))
    else
      SetDisplay(MsgFromId(S_OSK_FontHelper_ChooseKeyboard))
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

function TfrmOSKFontHelper.GetFontSize: Integer;   // I4807
begin
  if grid.DefaultColWidth < 36
    then Result := grid.DefaultRowHeight
    else Result := grid.DefaultRowHeight - 12;
end;

end.
