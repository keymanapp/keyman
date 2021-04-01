(*
  Name:             UframeOnScreenKeyboardEditor
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Initial version (split from UfrmKeymanWizard)
                    04 Jan 2007 - mcdurdin - Fix OSK AltGr flag bug
                    15 Jan 2007 - mcdurdin - Use correct key cap when no key cap character on selected shift state
                    22 Jan 2007 - mcdurdin - Support export to BMP and PNG
                    19 Mar 2007 - mcdurdin - I712 - Fix character map should follow selected character in OSK and kbd editors
                    30 May 2007 - mcdurdin - I726 - Fixed 102 key not displaying when it should
                    30 May 2007 - mcdurdin - I674 - Fixed crash when attempting to import invalid OSK XML file
                    13 Jul 2007 - mcdurdin - I905 - Stop keys moving around on the keyboard designer
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    14 Jun 2008 - mcdurdin - Fixup VK dirty state
                    10 Sep 2008 - mcdurdin - I1471 - Add key caps to all non-matched keys (option)
                    29 Sep 2008 - mcdurdin - I1658 - Add options for graphic location and usage
                    16 Jan 2009 - mcdurdin - Support HTML graphic and folder export options
                    18 Mar 2011 - mcdurdin - I1471 - Alt+LShift characters incorrectly added
                    18 Mar 2011 - mcdurdin - I2794 - Tike has some handle leaks
                    19 Aug 2011 - mcdurdin - I3022 - When using OSK editor with a Euro layout, key caps move around unexpectedly
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    11 Aug 2013 - mcdurdin - I3885 - V9.0 - Touch Layout Editor
                    21 Feb 2014 - mcdurdin - I4057 - V9.0 - Keyman Developer Keyboard Font dialog helpful to reduce font confusion
                    27 Feb 2014 - mcdurdin - I4085 - V9.0 - Remove old font selection controls from wizard
                    01 May 2014 - mcdurdin - I4181 - V9.0 - Stop using DeleteFileAlways, MOVEFILE_DELAY_UNTIL_REBOOT
                    27 May 2015 - mcdurdin - I4599 - Developer crashes if user tries importing an invalid KVK XML file [CrashID:tike.exe_9.0.481.0_005EB6B7_EDOMParseError
                    30 May 2015 - mcdurdin - I4730 - Loading a kvk file in a keyboard, where content is invalid, crashes developer [CrashID:tike.exe_9.0.504.0_0078D23F_EVisualKeyboardLoader]
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
                    03 Aug 2015 - mcdurdin - I4807 - Add Character Identifier to Keyman Developer
*)
unit UframeOnScreenKeyboardEditor;   // I3885   // I4085   // I4796

interface

uses
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Variants,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ExtDlgs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  OnScreenKeyboard,
  OnScreenKeyboardData,
  KeyBtn,
  VisualKeyboard,
  CharacterDragObject,
  VisualKeyboardParameters,
  UfrmTike,
  UframeTextEditor,
  TextFileFormat,
  TempFileManager;

type
  TImportKMXEvent = procedure(Sender: TObject; var KMXFileName: TTempFile) of object;   // I4181
  TImportKMXFinishedEvent = procedure(Sender: TObject; KMXFileName: TTempFile) of object;   // I4181

  TframeOnScreenKeyboardEditor = class(TTikeForm)
    panVK: TPanel;
    panBottom: TPanel;
    panKeyContent: TPanel;
    panKeyText: TPanel;
    editVKKeyText: TEdit;
    rbKeyText: TRadioButton;
    panKeyBitmap: TPanel;
    rbKeyBitmap: TRadioButton;
    cmdBrowseKeyBitmap: TButton;
    panKeyPreview: TPanel;
    panVKKey: TPanel;
    Label4: TLabel;
    VKkeySample: TKeyBtn;
    VKkeySampleCtrl: TKeyBtn;
    VKkeySampleShift: TKeyBtn;
    VKkeySampleAlt: TKeyBtn;
    panOptions: TPanel;
    chkVKDisplayUnderlyingChars: TCheckBox;
    chkVKSplitCtrlAlt: TCheckBox;
    chkVKOnlyUseWithUnderlyingLayout: TCheckBox;
    chkVKInclude102key: TCheckBox;
    cmdVKSelectLayout: TButton;
    kbdOnScreen: TOnScreenKeyboard;
    dlgVKImportXML: TOpenDialog;
    dlgVKSaveExport: TSaveDialog;
    dlgBrowseForKeyBitmap: TOpenPictureDialog;
    dlgFont: TFontDialog;
    dlgSaveVisualKeyboard: TSaveDialog;
    tmrUpdateCharacterMap: TTimer;
    chkFillUnderlyingLayout: TCheckBox;
    pages: TPageControl;
    pageDesign: TTabSheet;
    pageCode: TTabSheet;
    panTop: TPanel;
    cmdVKImportKMX: TButton;
    cmdExportOnScreenKeyboard: TButton;
    cmdImportOnScreenKeyboard: TButton;
    procedure kbdOnScreenDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure kbdOnScreenDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure kbdOnScreenSelectionChange(Sender: TObject);
    procedure kbdOnScreenShiftChange(Sender: TObject);
    procedure chkVKSplitCtrlAltClick(Sender: TObject);
    procedure chkVKDisplayUnderlyingCharsClick(Sender: TObject);
    procedure chkVKInclude102keyClick(Sender: TObject);
    procedure chkVKOnlyUseWithUnderlyingLayoutClick(Sender: TObject);
    procedure rbKeyTextClick(Sender: TObject);
    procedure rbKeyBitmapClick(Sender: TObject);
    procedure cmdBrowseKeyBitmapClick(Sender: TObject);
    procedure editVKKeyTextChange(Sender: TObject);
    procedure cmdVKSelectLayoutClick(Sender: TObject);
    procedure lblVKFontClick(Sender: TObject);
    procedure cmdImportOnScreenKeyboardClick(Sender: TObject);
    procedure cmdExportOnScreenKeyboardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdVKImportKMXClick(Sender: TObject);
    procedure lblVKCharacterSetClick(Sender: TObject);
    procedure editVKKeyTextClick(Sender: TObject);
    procedure editVKKeyTextKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tmrUpdateCharacterMapTimer(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
    procedure pagesChanging(Sender: TObject; var AllowChange: Boolean);
    procedure pagesChange(Sender: TObject);
  private
    FVKSetup: Boolean;

    FVK: TVisualKeyboard;
    FVKModified: Boolean;
    FVKCurrentKey: TVisualKeyboardKey;

    FVKUnicode: Boolean;
    FOnModified: TNotifyEvent;
    FKMXFileName: WideString;
    FFileName: WideString;
    FOnImportingKMX: TImportKMXEvent;
    FOnImportingKMXFinished: TImportKMXFinishedEvent;
    FVKLoading: Boolean;
    frameSource: TframeTextEditor;

    procedure SourceChanged(Sender: TObject);
    procedure VK_SetAllKeyDetails;
    procedure VK_UpdateKeyFont;
    function VK_GetCurrentKey: TVisualKeyboardKey;
    procedure VK_SelectVKey;
    procedure VK_UpdateSelectedKeyDetails;
    procedure VK_UpdateData;
    procedure ExportToFile(FileName: string);
    function GetBMPExportParams(FFileName: string; var FMulti, FANSI,
      FUnicode: Boolean; var FPixelWidth: Integer): Boolean;
    procedure EnableControls;
    procedure VK_UpdateCharacterSet(FMoveRules: Boolean);
    procedure SetVKModified(const Value: Boolean);
    procedure SetVKUnicode(const Value: Boolean);
    function GetUnderlyingLayout: HKL;
    procedure SetUnderlyingLayout(const Value: HKL);
    procedure VK_FocusKey;
    procedure SetKMXFileName(Value: WideString);
    procedure DoUpdateCharacterMap;
    function GetHTMLExportParams(FFileName: string; var FFolders,
      FGraphical: Boolean): Boolean;
    function GetKeyFont: TFont;   // I4057
    procedure SetKeyFont(const Value: TFont);
    function DoesKeyboardSupportXMLVisualKeyboard: Boolean;
    function TransferDesignToSource: Boolean;
    function TransferSourceToDesign(ASilent: Boolean): Boolean;
    function GetCodeFont: TFont;
    procedure SetCodeFont(const Value: TFont);   // I4057
  protected
    function GetHelpTopic: string; override;
  public

    procedure Load;
    procedure Save;
    procedure UpdateControls;
    procedure SetFocus; override;
    property UnderlyingLayout: HKL read GetUnderlyingLayout write SetUnderlyingLayout;
    property CodeFont: TFont read GetCodeFont write SetCodeFont;
    property KeyFont: TFont read GetKeyFont write SetKeyFont;   // I4057
    property VKModified: Boolean read FVKModified write SetVKModified;
    property VKUnicode: Boolean read FVKUnicode write SetVKUnicode;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnImportingKMX: TImportKMXEvent read FOnImportingKMX write FOnImportingKMX;
    property OnImportingKMXFinished: TImportKMXFinishedEvent read FOnImportingKMXFinished write FOnImportingKMXFinished;
    property FileName: WideString read FFileName write FFileName;
    property KMXFileName: WideString read FKMXFileName write SetKMXFileName;
  end;

implementation

uses
  Xml.Xmldom,

  Keyman.Developer.System.HelpTopics,

  CharacterInfo,
  CharMapInsertMode,
  ExtShiftState,
  KeymanDeveloperOptions,
  kmxfile,
  UfrmVisualKeyboardExportBMPParams,
  UfrmVisualKeyboardExportHTMLParams,
  UfrmVisualKeyboardImportKMX,
  utilstr,
  VisualKeyboardImportXML,
  VisualKeyboardExportBMP,
  VisualKeyboardExportPNG,
  VisualKeyboardExportHTML,
  VisualKeyboardExportXML;

{$R *.dfm}

procedure TframeOnScreenKeyboardEditor.FormCreate(Sender: TObject);
begin
  inherited;

  pages.ActivePage := pageDesign;

  FVKUnicode := True;

  kbdOnScreen.SelectedKey := kbdOnScreen.Keys[0];

  FVK := TVisualKeyboard.Create;
  FVK.Header.Flags := [kvkhDisplayUnderlying];

  FVKCurrentKey := nil;
  kbdOnScreen.DisplayUnderlyingChar := True;

  frameSource := TframeTextEditor.Create(Self);
  frameSource.Parent := pageCode;
  frameSource.Align := alClient;
  frameSource.EditorFormat := efXML;
  frameSource.Visible := True;
  frameSource.OnChanged := SourceChanged;
  frameSource.TextFileFormat := tffUTF8;  // Only UTF-8 supported for KVK XML
end;

procedure TframeOnScreenKeyboardEditor.TntFormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FVK);  // I2794
end;

procedure TframeOnScreenKeyboardEditor.SetCodeFont(const Value: TFont);
begin
  frameSource.CodeFont := Value;
end;

procedure TframeOnScreenKeyboardEditor.SetFocus;
begin
  inherited;
  if pages.ActivePage = pageCode then
    frameSource.SetFocus;
end;

procedure TframeOnScreenKeyboardEditor.SetKeyFont(const Value: TFont);   // I4057
begin
  if not FVKUnicode
    then FVK.Header.ANSIFont := Value
    else FVK.Header.UnicodeFont := Value;

  VK_UpdateKeyFont;
  VKModified := True;

  if pages.ActivePage = pageDesign then
    TransferDesignToSource;
end;

procedure TframeOnScreenKeyboardEditor.SetKMXFileName(Value: WideString);
begin
  FKMXFileName := Value;
  VK_UpdateData;

  if pages.ActivePage = pageDesign then
    TransferDesignToSource;
end;

procedure TframeOnScreenKeyboardEditor.SetUnderlyingLayout(const Value: HKL);
begin
  kbdOnScreen.UnderlyingLayout := Value;
  kbdOnScreen.Repaint;
end;

procedure TframeOnScreenKeyboardEditor.SetVKModified(const Value: Boolean);
begin
  if FVKModified <> Value then
  begin
    FVKModified := Value;
    if FVKModified then
      if Assigned(FOnModified) then FOnModified(Self);
  end;
end;

procedure TframeOnScreenKeyboardEditor.SetVKUnicode(const Value: Boolean);
begin
  if FVKUnicode <> Value then
  begin
    FVKUnicode := Value;
    VK_UpdateKeyFont;
    VK_SetAllKeyDetails;
    VK_UpdateSelectedKeyDetails;
    VK_UpdateCharacterSet(False);
  end;
end;

procedure TframeOnScreenKeyboardEditor.tmrUpdateCharacterMapTimer(Sender: TObject);
begin
  tmrUpdateCharacterMap.Enabled := False;
  UpdateCharacterMap(False, editVKKeyText.Text, 0, 0, True);
end;

{ ---------------------------------------------------------------------------- }
{ - Tab interactions                                                         - }
{ ---------------------------------------------------------------------------- }

procedure TframeOnScreenKeyboardEditor.pagesChange(Sender: TObject);
begin
  if pages.ActivePage = pageCode then
    frameSource.SetFocus;
end;

procedure TframeOnScreenKeyboardEditor.pagesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if pages.ActivePage = pageCode
    then AllowChange := TransferSourceToDesign(False)
    else AllowChange := TransferDesignToSource;
end;

function TframeOnScreenKeyboardEditor.TransferSourceToDesign(ASilent: Boolean): Boolean;
var
  stream: TStringStream;
begin
  stream := TStringStream.Create(frameSource.EditorText, TEncoding.UTF8);
  try
    try
      with TVisualKeyboard.Create do
      try
        LoadFromStream(stream);
      finally
        Free;
      end;
    except
      on E:EVisualKeyboardLoader do
      begin
        ShowMessage(E.Message);
        Exit(False);
      end;
    end;

    stream.Position := 0;
    FVKLoading := True;
    try
      FVK.LoadFromStream(stream);
      VK_UpdateData;
      VK_UpdateKeyFont;
      FVKCurrentKey := nil;
      VK_SelectVKey;
    finally
      FVKLoading := False;
    end;

  finally
    stream.Free;
  end;
  Result := True;
end;

function TframeOnScreenKeyboardEditor.TransferDesignToSource: Boolean;
var
  stream: TStringStream;
begin
  stream := TStringStream.Create('', TEncoding.UTF8);
  try
    FVK.SaveToStream(stream, kvksfXML);
    frameSource.EditorText := stream.DataString;
  finally
    stream.Free;
  end;
  Result := True;
end;

{ ---------------------------------------------------------------------------- }
{ - Design tab                                                               - }
{ ---------------------------------------------------------------------------- }

procedure TframeOnScreenKeyboardEditor.chkVKDisplayUnderlyingCharsClick(
  Sender: TObject);
begin
  if not FVKLoading then VKModified := True;
  kbdOnScreen.DisplayUnderlyingChar := chkVKDisplayUnderlyingChars.Checked;
  if chkVKDisplayUnderlyingChars.Checked
    then FVK.Header.Flags := FVK.Header.Flags + [kvkhDisplayUnderlying]
    else FVK.Header.Flags := FVK.Header.Flags - [kvkhDisplayUnderlying];
end;

procedure TframeOnScreenKeyboardEditor.chkVKInclude102keyClick(Sender: TObject);
begin
  if not FVKLoading then VKModified := True;
  kbdOnScreen.Display102Key := chkVKInclude102key.Checked;
  if chkVKInclude102key.Checked
    then FVK.Header.Flags := FVK.Header.Flags + [kvkh102]
    else FVK.Header.Flags := FVK.Header.Flags - [kvkh102];
end;

procedure TframeOnScreenKeyboardEditor.chkVKOnlyUseWithUnderlyingLayoutClick(
  Sender: TObject);
begin
  if not FVKLoading then VKModified := True;
  if chkVKOnlyUseWithUnderlyingLayout.Checked
    then FVK.Header.Flags := FVK.Header.Flags + [kvkhUseUnderlying]
    else FVK.Header.Flags := FVK.Header.Flags - [kvkhUseUnderlying];
end;

procedure TframeOnScreenKeyboardEditor.chkVKSplitCtrlAltClick(Sender: TObject);
var
  i, j: Integer;
  Found: Boolean;
begin
  if FVKSetup then Exit;

  FVKSetup := True;
  try
    if not chkVKSplitCtrlAlt.Checked then
    begin
      { Check if any lctrl/lalt/rctrl/alt are currently defined,  }
      { warn that they will all be merged and some entries may be }
      { lost if they conflict }

      Found := False;
      for i := 0 to FVK.Keys.Count - 1 do
        if FVK.Keys[i].HasShift(KVKS_LCTRL or KVKS_RCTRL or KVKS_LALT or KVKS_RALT) then
        begin
          Found := True;
          Break;
        end;

      if Found and (MessageDlg('You have already defined some keys to have left or right-specific states.'+
          '  If you continue, these shift states will be merged and you may lose some key states.'#13#10#13#10 +
          'Continue and merge shift states?', mtConfirmation, mbOkCancel, 0) = mrCancel) then
        begin
          chkVKSplitCtrlAlt.Checked := True;
          Exit;
        end;

      FVK.Header.Flags := FVK.Header.Flags - [kvkhAltGr];
      kbdOnScreen.LRShift := False;
      i := 0;
      while i < FVK.Keys.Count do
      begin
        Found := False;
        if FVK.Keys[i].HasShift(KVKS_LCTRL or KVKS_RCTRL) then
        begin
          FVK.Keys[i].Shift := (FVK.Keys[i].Shift and not (KVKS_LCTRL or KVKS_RCTRL)) or KVKS_CTRL;
          Found := True;
        end;
        if FVK.Keys[i].HasShift(KVKS_LALT or KVKS_RALT) then
        begin
          FVK.Keys[i].Shift := (FVK.Keys[i].Shift and not (KVKS_LALT or KVKS_RALT)) or KVKS_ALT;
          Found := True;
        end;
        if Found then
        begin
          Found := False;
          for j := 0 to i-1 do
          begin
            if (FVK.Keys[j].Shift = FVK.Keys[i].Shift) and (FVK.Keys[j].VKey = FVK.Keys[i].VKey) then
            begin
              Found := True;
              FVK.Keys.Delete(i);
              Break;
            end;
          end;
        end;
        if not Found then Inc(i);
      end;
    end
    else
    begin
      { Warn that all ctrl/alt will be set to lctrl/alt }

      Found := False;
      for i := 0 to FVK.Keys.Count - 1 do
        if FVK.Keys[i].HasShift(KVKS_CTRL or KVKS_ALT) then
        begin
          Found := True;
          Break;
        end;

      if Found and (MessageDlg('You have already defined some keys to have alt or ctrl states.'+
          '  If you continue, these shift states will be converted to left-alt and left-ctrl respectively.'#13#10#13#10 +
          'Continue and convert shift states?', mtConfirmation, mbOkCancel, 0) = mrCancel) then
        begin
          chkVKSplitCtrlAlt.Checked := False;
          Exit;
        end;

      FVK.Header.Flags := FVK.Header.Flags + [kvkhAltGr];
      kbdOnScreen.LRShift := True;
      i := 0;
      while i < FVK.Keys.Count do
      begin
        if FVK.Keys[i].HasShift(KVKS_CTRL) then
          FVK.Keys[i].Shift := (FVK.Keys[i].Shift and not KVKS_CTRL) or KVKS_LCTRL;
        if FVK.Keys[i].HasShift(KVKS_ALT) then
          FVK.Keys[i].Shift := (FVK.Keys[i].Shift and not KVKS_ALT) or KVKS_LALT;
        Inc(i);
      end;
    end;

    VK_SetAllKeyDetails;
    if not FVKLoading then VKModified := True;
  finally
    FVKSetup := False;
  end;
end;

procedure TframeOnScreenKeyboardEditor.cmdBrowseKeyBitmapClick(Sender: TObject);
var
  k: TVisualKeyboardKey;
  FBitmap: Vcl.Graphics.TBitmap;
  FPicture: TPicture;
begin
  if dlgBrowseForKeyBitmap.Execute then
  begin
    VKModified := True;
    k := VK_GetCurrentKey;
    if not Assigned(k) then
    begin
      k := TVisualKeyboardKey.Create;
      k.VKey := kbdOnScreen.SelectedKey.USVKey;
      k.Shift := ExtShiftStateToVkShiftState(kbdOnScreen.ShiftState);
      if FVKUnicode then k.Flags := [kvkkUnicode];
      FVK.Keys.Add(k);
    end;

    FBitmap := Vcl.Graphics.TBitmap.Create;
    try
      FPicture := TPicture.Create;
      try
        FPicture.LoadFromFile(dlgBrowseForKeyBitmap.FileName);
        FBitmap.Width := FPicture.Width;
        FBitmap.Height := FPicture.Height;
        FBitmap.Canvas.Draw(0, 0, FPicture.Graphic);
      finally
        FPicture.Free;
      end;
      k.Bitmap := FBitmap;
    finally
      FBitmap.Free;
    end;
    k.Flags := k.Flags + [kvkkBitmap];
    rbKeyBitmap.Checked := True;
  end;

  VK_UpdateSelectedKeyDetails;
end;

procedure TframeOnScreenKeyboardEditor.cmdExportOnScreenKeyboardClick(
  Sender: TObject);
begin
  dlgVKSaveExport.Filter := 'Web page (*.html)|*.htm?|Image (*.bmp)|*.bmp|Image (*.png)|*.png|XML file (*.xml)|*.xml';
  if dlgVKSaveExport.Execute then
    ExportToFile(dlgVKSaveExport.FileName);
end;

procedure TframeOnScreenKeyboardEditor.cmdImportOnScreenKeyboardClick(
  Sender: TObject);
begin
  if dlgVKImportXML.Execute then
    with TVisualKeyboardImportXML.Create(FVK) do
    try
      try
        ImportFromFile(dlgVKImportXML.FileName);
      except
        on E:EDOMParseError do   // I4599
        begin
          ShowMessage(E.Message);
          Exit;
        end;
        on E:EVisualKeyboardImportXML do
        begin
          ShowMessage(E.Message);
          Exit;
        end;
      end;
      VK_UpdateData;
      EnableControls;
      VKModified := True;
    finally
      Free;
    end;
end;

procedure TframeOnScreenKeyboardEditor.cmdVKImportKMXClick(Sender: TObject);
  (*function DoesKeyboardHaveAltGr(const AFileName: WideString): Boolean;
  var
    ki: TKeyboardInfo;
    kfh: PKeyboardFileHeader;
    kg: PKeyboardFileGroup;
    kk: PKeyboardFileKey;
    i: Integer;
    j: Integer;
  begin
    Result := False;
    GetKeyboardInfo(AFilename, True, ki, False);
    try
      kfh := PKeyboardFileHeader(ki.MemoryDump.Memory);

      if kfh.dpGroupArray > ki.MemoryDump.Size - SizeOf(TKeyboardFileGroup)*kfh.cxGroupArray then Exit;

      kg := PKeyboardFileGroup(Dword(kfh)+kfh.dpGroupArray);
      for i := 0 to kfh.cxGroupArray - 1 do
      begin
        if kg.fUsingKeys then
        begin
          if kg.dpKeyArray > ki.MemoryDump.Size - SizeOf(TKeyboardFileKey)*kg.cxKeyArray then Exit;
          kk := PKeyboardFileKey(DWord(kfh)+kg.dpKeyArray);
          for j := 0 to kg.cxKeyArray - 1 do
          begin
            if (kk.Key and (KMX_LCTRLFLAG or KMX_RCTRLFLAG or KMX_LALTFLAG or KMX_RALTFLAG)) <> 0 then
            begin
              Result := True;
              Exit;
            end;
            Inc(kk);
          end;
        end;
        Inc(kg);
      end;
    finally
      ki.MemoryDump.Free;
    end;
  end;*)

  procedure AddUnderlyingLayoutKeys;
  var
    i: Integer;
    k: TVisualKeyboardKey;
    j: Integer;
  begin
    for i := 0 to kbdOnScreen.Keys.Count - 1 do
    begin
      if kbdOnScreen.Keys[i].KeyType = kktNormal then
      begin
        for j := 0 to High(ValidExtShiftStates) do
        begin
          if chkVKSplitCtrlAlt.Checked then
          begin
            if ValidExtShiftStates[j] * [essCtrl, essAlt] <> [] then Continue;
          end
          else
            if ValidExtShiftStates[j] * [essLCtrl, essRCtrl, essLAlt, essRAlt] <> [] then Continue;

          if ValidExtShiftStates[j] = [essAlt] then Continue;
          if ValidExtShiftStates[j] = [essShift, essAlt] then Continue;  // I1471:9011

          if (Trim(kbdOnScreen.Keys[i].KeyCaps[j]) <> '') and
            (kbdOnScreen.Keys[i].KeyCaps[j][1] >= #32) then
          begin
            k := TVisualKeyboardKey.Create;
            k.VKey := kbdOnScreen.Keys[i].VKey;
            k.Shift := ExtShiftStateToVkShiftState(ValidExtShiftStates[j]);
            k.Text := kbdOnScreen.Keys[i].KeyCaps[j];
            if FVKUnicode then k.Flags := [kvkkUnicode] else k.Flags := [];
            FVK.Keys.Add(k);
          end;
        end;
      end;
    end;
  end;

  procedure ImportKMX(AFileName: WideString);
  begin
    with TfrmVisualKeyboardImportKMX.Create(Application.MainForm) do
    try
      Self.FVK.Keys.Clear;

      if chkFillUnderlyingLayout.Checked then
        AddUnderlyingLayoutKeys;

      VK := Self.FVK;

      FileName := AFileName;
      if ShowModal = mrOk then
      begin
        Self.FVK.Header.AssociatedKeyboard := ChangeFileExt(ExtractFileName(AFileName), '');
        if LeftRightCtrlAlt
          then Self.FVK.Header.Flags := Self.FVK.Header.Flags + [kvkhAltGr]
          else Self.FVK.Header.Flags := Self.FVK.Header.Flags - [kvkhAltGr];
        if Show102Key
          then Self.FVK.Header.Flags := Self.FVK.Header.Flags + [kvkh102]
          else Self.FVK.Header.Flags := Self.FVK.Header.Flags - [kvkh102];
        
        Self.VK_UpdateData;
        Self.VKModified := True;
      end;
    finally
      Free;
    end;
  end;

var
  FTempKMXFileName: TTempFile;   // I4181
  FLocalKMXFileName: string;   // I4181
begin
  if Assigned(FOnImportingKMX) then
  begin
    FTempKMXFileName := nil;
    FOnImportingKMX(Self, FTempKMXFileName);
    if not Assigned(FTempKMXFileName) then Exit;
    FLocalKMXFileName := FTempKMXFileName.Name;   // I4181
  end
  else
    FLocalKMXFileName := FKMXFileName;

  if FLocalKMXFileName = '' then Exit;

  if not FileExists(FLocalKMXFileName) then
  begin
    ShowMessage('You must compile the keyboard before you can import it into the on-screen keyboard.');
    Exit;
  end;

  if FVK.Keys.Count > 0 then
    if MessageDlg('Importing '+ExtractFileName(FLocalKMXFileName)+' will overwrite the existing keys defined in the Visual Keyboard.'+
      '  Continue and overwrite?',mtConfirmation, mbOkCancel, 0) = mrCancel then Exit;

  ImportKMX(FLocalKMXFileName);

  if Assigned(FOnImportingKMXFinished) and Assigned(FOnImportingKMX) then
    FOnImportingKMXFinished(Self, FTempKMXFileName);

  VKModified := True;
end;

procedure TframeOnScreenKeyboardEditor.cmdVKSelectLayoutClick(Sender: TObject);
{var
  FLoadedSystemKeyboard: Boolean;
  FSystemKeyboardName: string;}
begin
(*  FLoadedSystemKeyboard := VKkbd.LoadedSystemKeyboard;
  FSystemKeyboardName := VKkbd.SystemKeyboardName;
  if SelectSystemKeyboard(frmKeymanDeveloper, FLoadedSystemKeyboard, FSystemKeyboardName) then
  begin
    VKkbd.SetUnderlyingLayout(LoadKeyboardLayout(PChar(FSystemKeyboardName), 0),
      FLoadedSystemKeyboard, FSystemKeyboardName);
    FVK.Header.UnderlyingLayout := FSystemKeyboardName;
    VK_UpdateUnderlyingLayoutCaption;
  end; *)
end;

procedure TframeOnScreenKeyboardEditor.editVKKeyTextChange(Sender: TObject);
var
  w: WideString;
  k: TVisualKeyboardKey;
begin
  if FVKSetup then Exit;

  if not Assigned(kbdOnScreen.SelectedKey) then Exit;

  rbKeyText.Checked := True;
  
  if not FVKLoading then VKModified := True;

  w := editVKKeyText.Text;
  k := VK_GetCurrentKey;
  if w <> '' then
  begin
    if not Assigned(k) then
    begin
      k := TVisualKeyboardKey.Create;
      k.VKey := kbdOnScreen.SelectedKey.USVKey;
      k.Shift := ExtShiftStateToVkShiftState(kbdOnScreen.ShiftState);
      if FVKUnicode then k.Flags := [kvkkUnicode];
      FVK.Keys.Add(k);
    end;

    k.Flags := k.Flags - [kvkkBitmap];
    k.Bitmap := nil;
    k.Text := w;
  end
  else if Assigned(k) then
  begin
    k.Text := '';
    VK_UpdateSelectedKeyDetails;
    FVK.Keys.Remove(k);
    FVKCurrentKey := nil;
  end;

  VK_UpdateSelectedKeyDetails;
end;

procedure TframeOnScreenKeyboardEditor.editVKKeyTextClick(Sender: TObject);
begin
  DoUpdateCharacterMap;
end;

procedure TframeOnScreenKeyboardEditor.editVKKeyTextKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  DoUpdateCharacterMap;
end;


procedure TframeOnScreenKeyboardEditor.DoUpdateCharacterMap;
begin
  tmrUpdateCharacterMap.Enabled := False;
  tmrUpdateCharacterMap.Enabled := True;
end;

procedure TframeOnScreenKeyboardEditor.kbdOnScreenDragDrop(Sender,
  Source: TObject; X, Y: Integer);
var
  i: Integer;
  k: TOnScreenKeyboardKey;
  FKey: TVisualKeyboardKey;
  s: WideString;
  ss: Word;
begin
  inherited;
  k := kbdOnScreen.GetKeyAtPoint(X, Y);
  if not Assigned(k) or (k.KeyType <> kktNormal) then Exit;

  FKey := nil;

  ss := ExtShiftStateToVkShiftState(kbdOnScreen.ShiftState);

  for i := 0 to FVK.Keys.Count - 1 do
    if (FVK.Keys[i].VKey = k.USVKey) and (FVK.Keys[i].Shift = ss) and
      (((kvkkUnicode in FVK.Keys[i].Flags) and (FVKUnicode)) or
       (not (kvkkUnicode in FVK.Keys[i].Flags) and (not FVKUnicode))) then
    begin
      FKey := FVK.Keys[i];
      Break;
    end;

  if not Assigned(FKey) then
  begin
    FKey := TVisualKeyboardKey.Create;
    FKey.VKey := k.USVKey;
    FKey.Shift := ExtShiftStateToVkShiftState(kbdOnScreen.ShiftState);
    if FVKUnicode then FKey.Flags := [kvkkUnicode];
    FVK.Keys.Add(FKey);
  end;

  FKey.Bitmap := nil;

  with Source as TCharacterDragObject do
    s := Text[cmimCharacter];

  if GetAsyncKeyState(VK_CONTROL) < 0
    then FKey.Text := FKey.Text + s
    else FKey.Text := s;

  k.KeyGlyph := FKey.Bitmap;
  k.KeyValue := FKey.Text;

  if k = kbdOnScreen.SelectedKey then VK_SelectVKey;

  VKModified := True;
end;

procedure TframeOnScreenKeyboardEditor.kbdOnScreenDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  k: TOnScreenKeyboardKey;
begin
  Accept := Source is TCharacterDragObject;

  if Accept then
  begin
    k := kbdOnScreen.GetKeyAtPoint(X, Y);
    if not Assigned(k) or (k.KeyType <> kktNormal) then Accept := False;
  end;
end;

procedure TframeOnScreenKeyboardEditor.kbdOnScreenSelectionChange(Sender: TObject);
begin
  VK_SelectVKey;
end;

procedure TframeOnScreenKeyboardEditor.kbdOnScreenShiftChange(Sender: TObject);
begin
  FVKCurrentKey := nil;
  VK_SetAllKeyDetails;
  VK_SelectVKey;
  VK_FocusKey;
end;

procedure TframeOnScreenKeyboardEditor.lblVKCharacterSetClick(Sender: TObject);
begin
  VKUnicode := not VKUnicode;
end;

procedure TframeOnScreenKeyboardEditor.lblVKFontClick(Sender: TObject);
begin
  if not FVKUnicode
    then dlgFont.Font := FVK.Header.ANSIFont
    else dlgFont.Font := FVK.Header.UnicodeFont;

  if dlgFont.Execute then
  begin
    if not FVKUnicode
      then FVK.Header.ANSIFont := dlgFont.Font
      else FVK.Header.UnicodeFont := dlgFont.Font;
    VK_UpdateKeyFont;
    VKModified := True;
  end;
end;

procedure TframeOnScreenKeyboardEditor.rbKeyBitmapClick(Sender: TObject);
begin
  rbKeyText.Checked := False;
  EnableControls;
  VK_FocusKey;
  if not FVKSetup and not FVKLoading then VKModified := True;
end;

procedure TframeOnScreenKeyboardEditor.rbKeyTextClick(Sender: TObject);
var
  k: TVisualKeyboardKey;
begin
  rbKeyBitmap.Checked := False;
  k := VK_GetCurrentKey;
  if Assigned(k) and (kvkkBitmap in k.Flags) then
  begin
    FVK.Keys.Remove(k);
    FVKCurrentKey := nil;
  end;
  EnableControls;
  VK_FocusKey;
  if not FVKSetup and not FVKLoading then VKModified := True;
end;

procedure TframeOnScreenKeyboardEditor.VK_SetAllKeyDetails;
var
  k: TOnScreenKeyboardKey;
  i: Integer;
  ss: Word;
begin
  kbdOnScreen.Keys.ClearValues;

  ss := ExtShiftStateToVkShiftState(kbdOnScreen.ShiftState);
  for i := 0 to FVK.Keys.Count - 1 do
  begin
    if (FVK.Keys[i].Shift = ss) and
      (((kvkkUnicode in FVK.Keys[i].Flags) and (FVKUnicode)) or
      (not (kvkkUnicode in FVK.Keys[i].Flags) and (not FVKUnicode))) then
    begin
      k := kbdOnScreen.Keys.ItemsByUSVK[FVK.Keys[i].VKey];  // I3022
      if Assigned(k) then
      begin
        k.KeyValue := FVK.Keys[i].Text;
        k.KeyGlyph := FVK.Keys[i].Bitmap;
      end;
    end;
  end;
//  UpdateSelectedKeyDetails;
end;

procedure TframeOnScreenKeyboardEditor.VK_UpdateKeyFont;
var
  FFont: TFont;
begin
  if not FVKUnicode
    then FFont := FVK.Header.ANSIFont
    else FFont := FVK.Header.UnicodeFont;

  editVKKeyText.Font := FFont;

  kbdOnScreen.DataFont := FFont;

  VKkeySample.DataFont := FFont;
  VKkeySample.DataFont.Color := clBlack;
  VKkeySample.Repaint;

  frameSource.CharFont := FFont;
end;

function TframeOnScreenKeyboardEditor.VK_GetCurrentKey: TVisualKeyboardKey;
var
  i: Integer;
  ss: Word;
begin
  if Assigned(FVKCurrentKey) then
  begin
    Result := FVKCurrentKey;
    Exit;
  end;

  if kbdOnScreen.SelectedKey = nil then
  begin
    Result := nil;
    Exit;
  end;

  ss := ExtShiftStateToVkShiftState(kbdOnScreen.ShiftState);
  for i := 0 to FVK.Keys.Count - 1 do
  begin
    if (FVK.Keys[i].VKey = kbdOnScreen.SelectedKey.USVKey) and (FVK.Keys[i].Shift = ss) and
      (((kvkkUnicode in FVK.Keys[i].Flags) and (FVKUnicode)) or
       (not (kvkkUnicode in FVK.Keys[i].Flags) and (not FVKUnicode))) then
    begin
      FVKCurrentKey := FVK.Keys[i];
      Result := FVK.Keys[i];
      Exit;
    end;
  end;

  FVKCurrentKey := nil;
  Result := nil;
end;

procedure TframeOnScreenKeyboardEditor.VK_UpdateSelectedKeyDetails;
  procedure SetKeyText(key: TOnScreenKeyboardKey);
  begin
    if key <> nil then
    begin
      if (key.ActiveKeyCap = '') or (key.ActiveKeyCap[1] < #32)
        then VKkeySample.KeyText := key.KeyCaps[0]
        else VKkeySample.KeyText := key.ActiveKeyCap;
    end
    else
      VKkeySample.KeyText := '';
  end;
var
  k: TVisualKeyboardKey;
  osk: TOnScreenKeyboardKey;
  x: Integer;
begin
  k := VK_GetCurrentKey;
  if not Assigned(k) then
  begin
    VKkeySample.KeyData := '';
    VKkeySample.KeyGlyph := nil;
    SetKeyText(kbdOnScreen.SelectedKey);
  end
  else
  begin
    osk := kbdOnScreen.Keys.ItemsByUSVK[k.VKey];  // I3022
    if Assigned(osk) then
    begin
      SetKeyText(osk);
      VKkeySample.KeyData  := k.Text;
      VKkeySample.KeyGlyph := k.Bitmap;
      osk.KeyGlyph := k.Bitmap;
      osk.KeyValue := k.Text;
    end;
  end;

  x := VKkeySample.Left;
  if kbdOnScreen.ShiftState * [essAlt, essLAlt, essRAlt] <> [] then
  begin
    if essLAlt in kbdOnScreen.ShiftState then VKkeySampleAlt.KeyText := 'L Alt'
    else if essRAlt in kbdOnScreen.ShiftState then VKkeySampleAlt.KeyText := 'R Alt'
    else VKkeySampleAlt.KeyText := 'Alt';
    VKkeySampleAlt.Left := x - 3 - VKkeySampleAlt.Width;
    x := VKkeySampleAlt.Left;
    VKkeySampleAlt.Visible := True;
  end
  else
    VKkeySampleAlt.Visible := False;

  if kbdOnScreen.ShiftState * [essCtrl, essLCtrl, essRCtrl] <> [] then
  begin
    if essLCtrl in kbdOnScreen.ShiftState then VKkeySampleCtrl.KeyText := 'L Ctrl'
    else if essRCtrl in kbdOnScreen.ShiftState then VKkeySampleCtrl.KeyText := 'R Ctrl'
    else VKkeySampleCtrl.KeyText := 'Ctrl';
    VKkeySampleCtrl.Left := x - 3 - VKkeySampleCtrl.Width;
    x := VKkeySampleCtrl.Left;
    VKkeySampleCtrl.Visible := True;
  end
  else
    VKkeySampleCtrl.Visible := False;

  if essShift in kbdOnScreen.ShiftState then
  begin
    VKkeySampleShift.Left := x - 3 - VKkeySampleShift.Width;
    VKkeySampleShift.Visible := True;
    VKkeySampleShift.Invalidate;
  end
  else
    VKkeySampleShift.Visible := False;
end;

procedure TframeOnScreenKeyboardEditor.VK_SelectVKey;
var
  k: TVisualKeyboardKey;
begin
  FVKSetup := True;
  try
    FVKCurrentKey := nil;
    k := VK_GetCurrentKey;
    if Assigned(k) then
    begin
      if kvkkBitmap in k.Flags then
      begin
        editVKKeyText.Text := '';
        rbKeyBitmap.Checked := True;
      end
      else
      begin
        rbKeyText.Checked := True;
        editVKKeyText.Text := k.Text;
        editVKKeyText.SelectAll;
      end;
    end
    else
    begin
      rbKeyText.Checked := True;
      editVKKeyText.Text := '';
    end;

    VK_UpdateSelectedKeyDetails;
    EnableControls;
    VK_FocusKey;

    DoUpdateCharacterMap;
  finally
    FVKSetup := False;
  end;
end;

procedure TframeOnScreenKeyboardEditor.VK_UpdateData;
begin
  VK_SetAllKeyDetails;

  if FVK.Header.AssociatedKeyboard <> ChangeFileExt(ExtractFileName(FKMXFileName), '') then
  begin
    FVK.Header.AssociatedKeyboard := ChangeFileExt(ExtractFileName(FKMXFileName), '');
    if not FVKLoading then VKModified := True;
  end;

  chkVKSplitCtrlAlt.Checked := kvkhAltGr in FVK.Header.Flags;
  chkVKInclude102Key.Checked := kvkh102 in FVK.Header.Flags;
  chkVKDisplayUnderlyingChars.Checked := kvkhDisplayUnderlying in FVK.Header.Flags;
  chkVKOnlyUseWithUnderlyingLayout.Checked := kvkhUseUnderlying in FVK.Header.Flags;

  kbdOnScreen.Display102Key := chkVKInclude102key.Checked;
  kbdOnScreen.DisplayUnderlyingChar := chkVKDisplayUnderlyingChars.Checked;
  kbdOnScreen.LRShift := chkVKSplitCtrlAlt.Checked;
end;

procedure TframeOnScreenKeyboardEditor.Load;
var
  stream: TStream;
begin
  FVKLoading := True;
  try
    FVK.Clear;

    if (FileName <> '') and FileExists(FileName) then
    begin
      stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
      try
        try
          FVK.LoadFromStream(stream);
        except
          on E:EVisualKeyboardLoader do   // I4730
          begin
            stream.Position := 0;
            if not TVisualKeyboard.IsBinaryFile(stream) then
            begin
              // Load the keyboard in the source tab
              pages.ActivePage := pageCode;
              frameSource.LoadFromStream(stream);
              Exit; // Avoids transferring empty keyboard to source
            end
            else
              ShowMessage(E.Message);
            FVK.Clear;
          end;
        end;
      finally
        stream.Free;
      end;
    end;

    VK_UpdateData;
    VK_UpdateKeyFont;
  finally
    FVKModified := False;
    FVKLoading := False;
  end;

  if pages.ActivePage = pageCode then
    TransferDesignToSource;
end;

procedure TframeOnScreenKeyboardEditor.UpdateControls;
begin
  VK_UpdateCharacterSet(False);
  VK_SetAllKeyDetails;
end;

function TframeOnScreenKeyboardEditor.DoesKeyboardSupportXMLVisualKeyboard: Boolean;
begin
  Result := SameText(ExtractFileExt(FFileName), '.kvks');
end;

procedure TframeOnScreenKeyboardEditor.Save;
begin
  if FFileName = '' then
  begin
    if KMXFileName = '' then
    begin
      if dlgSaveVisualKeyboard.Execute
        then FFileName := dlgSaveVisualKeyboard.FileName
        else Exit;
    end
    else
      FFileName := ChangeFileExt(KMXFileName, '.kvks');
  end;

  if DoesKeyboardSupportXMLVisualKeyboard then
  begin
    if pages.ActivePage = pageDesign then
      if not TransferDesignToSource then
      begin
        ShowMessage('Could not transfer design page to source');
        Exit;
      end;
    frameSource.SaveToFile(FFileName);
  end
  else
  begin
    if pages.ActivePage = pageCode then
      if not TransferSourceToDesign(False) then
        Exit;
    FVK.SaveToFile(FFileName, kvksfBinary);
  end;

//  if IsKMXVersion10OrLater then
//else
  FVKModified := False;
end;

function TframeOnScreenKeyboardEditor.GetBMPExportParams(FFileName: string;
  var FMulti, FANSI, FUnicode: Boolean; var FPixelWidth: Integer): Boolean;
begin
  Result := False;
  with TfrmVisualKeyboardExportBMPParams.Create(Application.MainForm) do
  try
    FileName := FFileName;
    if ShowModal = mrOk then
    begin
      Result := True;
      FMulti := MultiFile;
      FANSI := OutputANSI;
      FUnicode := OutputUnicode;
      FPixelWidth := PixelWidth;
    end;
  finally
    Free;
  end;
end;

function TframeOnScreenKeyboardEditor.GetCodeFont: TFont;
begin
  Result := frameSource.CodeFont;
end;

function TframeOnScreenKeyboardEditor.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_OnScreenKeyboardEditor;
end;

function TframeOnScreenKeyboardEditor.GetHTMLExportParams(FFileName: string;
  var FFolders, FGraphical: Boolean): Boolean;
begin
  Result := False;
  with TfrmVisualKeyboardExportHTMLParams.Create(Application.MainForm) do
  try
    FileName := FFileName;
    if ShowModal = mrOk then
    begin
      Result := True;
      FFolders := Folders;
      FGraphical := Graphical;
    end;
  finally
    Free;
  end;
end;

function TframeOnScreenKeyboardEditor.GetKeyFont: TFont;   // I4057
begin
  if not FVKUnicode
    then Result := FVK.Header.ANSIFont
    else Result := FVK.Header.UnicodeFont;
end;

function TframeOnScreenKeyboardEditor.GetUnderlyingLayout: HKL;
begin
  Result := kbdOnScreen.UnderlyingLayout;
end;

procedure TframeOnScreenKeyboardEditor.ExportToFile(FileName: string);
var
  FVKE: TVisualKeyboardExport;
  ext: string;
  FMulti, FANSI, FUnicode: Boolean;
  FPixelWidth: Integer;
  FFolders, FGraphical: Boolean;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  FMulti := False;
  FANSI := False;
  FUnicode := False;

  if (ext = '.bmp') or (ext = '.png') then
  begin
    if not GetBMPExportParams(FileName, FMulti, FANSI, FUnicode, FPixelWidth) then Exit;
  end
  else if (ext = '.html') or (ext = '.htm') then
    if not GetHTMLExportParams(FileName, FFolders, FGraphical) then Exit;

  Update;

  Screen.Cursor := crHourglass;
  try
    if (ext = '.html') or (ext = '.htm') then FVKE := TVisualKeyboardExportHTML.Create(FVK, FFolders, FGraphical)
    else if ext = '.bmp' then FVKE := TVisualKeyboardExportBMP.Create(FVK, FMulti, FANSI, FUnicode, FPixelWidth)
    else if ext = '.png' then FVKE := TVisualKeyboardExportPNG.Create(FVK, FMulti, FANSI, FUnicode, FPixelWidth)
    else if ext = '.xml' then
    begin
      FVKE := TVisualKeyboardExportXML.Create(FVK)
    end
    else
    begin
      ShowMessage('The export file type was not recognised.  Please use HTML, BMP, PNG or XML.');
      Exit;
    end;

    with FVKE do
    try
      ExportToFile(FileName);
    finally
      Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TframeOnScreenKeyboardEditor.EnableControls;
var
  e: Boolean;
begin
  e := True;
  cmdVKImportKMX.Enabled := e;
  cmdImportOnScreenKeyboard.Enabled := e;
  cmdExportOnScreenKeyboard.Enabled := e;
  panVK.Visible := e;
end;

procedure TframeOnScreenKeyboardEditor.VK_UpdateCharacterSet(FMoveRules: Boolean);
var
  i, n: Integer;
  FKey: TVisualKeyboardKey;
begin
  {if FVKUnicode then
  begin
    lblVKCharacterSet.Font.Color := clBlue;
    lblVKCharacterSet.Font.Style := [fsUnderline];
    lblVKCharacterSet.Cursor := crHandPoint;
  end
  else
  begin
  }
//  end;

  if FMoveRules then
  begin
    if FVKUnicode then

    // Delete existing keys in the destination layout
    for i := FVK.Keys.Count - 1 downto 0 do
      if FVKUnicode then
      begin
        if kvkkUnicode in FVK.Keys[i].Flags then FVK.Keys.Delete(i);
      end
      else
        if not (kvkkUnicode in FVK.Keys[i].Flags) then FVK.Keys.Delete(i);

    // Copy keys
    n := FVK.Keys.Count;
    for i := 0 to n - 1 do
    begin
      FKey := TVisualKeyboardKey.Create;
      FKey.VKey := FVK.Keys[i].VKey;
      FKey.Flags := FVK.Keys[i].Flags;
      FKey.Shift := FVK.Keys[i].Shift;
      FKey.Text := FVK.Keys[i].Text;
      if Assigned(FVK.Keys[i].Bitmap) then
      begin
        FKey.Bitmap := Vcl.Graphics.TBitmap.Create;
        FKey.Bitmap.Assign(FVK.Keys[i].Bitmap);
      end;
      if FVKUnicode
        then FKey.Flags := FKey.Flags + [kvkkUnicode]
        else FKey.Flags := FKey.Flags - [kvkkUnicode];
      FVK.Keys.Add(FKey);
    end;
  end;
end;

procedure TframeOnScreenKeyboardEditor.VK_FocusKey;
begin
  if editVKKeyText.CanFocus then
    if rbKeyText.Checked
      then editVKKeyText.SetFocus
      else cmdBrowseKeyBitmap.SetFocus;
end;

{ ---------------------------------------------------------------------------- }
{ - Source tab                                                               - }
{ ---------------------------------------------------------------------------- }

procedure TframeOnScreenKeyboardEditor.SourceChanged(Sender: TObject);
begin
  VKModified := True;
end;

end.
