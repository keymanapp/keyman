(*
  Name:             UframeTouchLayoutBuilder
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      21 Aug 2013

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          21 Aug 2013 - mcdurdin - I3894 - V9.0 - Support modified state for touch layout builder
                    21 Aug 2013 - mcdurdin - I3895 - V9.0 - App hotkeys don't work within layout builder
                    07 Nov 2013 - mcdurdin - I3945 - V9.0 - Touch Layout Editor should allow import from existing On Screen Keyboard
                    07 Nov 2013 - mcdurdin - I3642 - V9.0 - kmx compiler should validate &layoutfile
                    07 Nov 2013 - mcdurdin - I3948 - V9.0 - Touch Layout Editor does not set modified flag
                    29 Nov 2013 - mcdurdin - I3983 - V9.0 - Prevent crash on exit in touch layout editor
                    10 Jan 2014 - mcdurdin - I4023 - V9.0 - Touch Layout Editor crashes with file in use sometimes
                    07 Feb 2014 - mcdurdin - I4034 - V9.0 - Restructure keyboard wizard for source views and features
                    13 Feb 2014 - mcdurdin - I4046 - V9.0 - Layout editor needs to refresh character map
                    13 Feb 2014 - mcdurdin - I4047 - V9.0 - TIKE shows an error if you try to save before viewing the touch layout tab
                    21 Feb 2014 - mcdurdin - I4057 - V9.0 - Keyman Developer Keyboard Font dialog helpful to reduce font confusion
                    27 Feb 2014 - mcdurdin - I4083 - V9.0 - When errors encountered in JSON layout file, locate the error in the source view
                    19 Mar 2014 - mcdurdin - I4146 - V9.0 - When importing OSK into touch layout, unused layers are populated with base layer
                    19 Mar 2014 - mcdurdin - I4147 - V9.0 - If a touch layout is added to a keyboard, it doesn't get saved until the tab is visited
                    24 Apr 2014 - mcdurdin - I4195 - V9.0 - Use TTempFileManager for all temporary files
                    02 May 2014 - mcdurdin - I4226 - V9.0 - Keyboard wizard crashes with missing template-default.js when adding Touch Layout
                    03 Aug 2015 - mcdurdin - I4807 - Add Character Identifier to Keyman Developer
*)
unit UframeTouchLayoutBuilder;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmTike, UserMessages, KMDActionInterfaces,
  KeyboardFonts, TempFileManager, Keyman.Developer.UI.UframeCEFHost;

type
  TframeTouchLayoutBuilder = class(TTikeForm, IKMDEditActions)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);   // I4057
  private
    class var FInitialFilenameIndex: Integer;

  private
    cef: TframeCEFHost;

    FLoading: Boolean;   // I4057
    FTemplateFileName: string;
    FOnModified: TNotifyEvent;
    FSavedLayoutJS: string;
    FOnImportFromOSKCommand: TNotifyEvent;
    FOnSelectTemplateCommand: TNotifyEvent;
    FCanUndo: Boolean;
    FCanRedo: Boolean;
    FDisplayScriptErrors: Boolean;
    FLastError: string;   // I4083
    FLastErrorOffset: Integer;   // I4083
    FFilename: string;
    function GetLayoutJS: string;
    procedure WMUser_FireCommand(var Message: TMessage); message WM_USER_FireCommand;
    procedure DoModified;
    procedure DoLoad;
    procedure FireCommand(const commands: TStringList); virtual;
    procedure DoSelectTemplate;
    procedure DoImportFromOSK;
    function BuilderCommand(const cmd: string): Boolean;
    procedure UpdateCharacterMap(code: string);  // I4046
    function GetFontInfo(Index: TKeyboardFont): TKeyboardFontInfo;   // I4057
    procedure SetFontInfo(Index: TKeyboardFont; const Value: TKeyboardFontInfo);   // I4057
    procedure SetupModifierKeysForImportedLayout;

    procedure cefBeforeBrowse(Sender: TObject; const Url: string;
      out Result: Boolean);
    procedure cefLoadEnd(Sender: TObject);
  protected
    function GetHelpTopic: string; override;

    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure Redo;
    procedure SelectAll;
    procedure ClearSelection;
    function CanCut: Boolean;
    function CanCopy: Boolean;
    function CanPaste: Boolean;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function CanSelectAll: Boolean;
    function CanClearSelection: Boolean;

  public
    { Public declarations }
    function Load(const AFilename: string; ALoadFromTemplate, ALoadFromString: Boolean): Boolean;
    procedure Save(const AFilename: string);
    function SaveToString: string;
    procedure ImportFromKVK(const KVKFileName: string);   // I3945
    function IsModified: Boolean;
    property TemplateFileName: string read FTemplateFileName write FTemplateFileName;
    property FontInfo[Index: TKeyboardFont]: TKeyboardFontInfo read GetFontInfo write SetFontInfo;   // I4057
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnImportFromOSKCommand: TNotifyEvent read FOnImportFromOSKCommand write FOnImportFromOSKCommand;
    property OnSelectTemplateCommand: TNotifyEvent read FOnSelectTemplateCommand write FOnSelectTemplateCommand;
    property LastError: string read FLastError;   // I4083
    property LastErrorOffset: Integer read FLastErrorOffset;
  end;

implementation

uses
  System.JSON,

  xmlintf,
  xmldoc,

  Keyman.Developer.System.HelpTopics,

  KeymanDeveloperOptions,
  VKeys,
  OnScreenKeyboardData,
  TouchLayout,
  TouchLayoutUtils,   // I4057
  RedistFiles,
  ScanCodeMap,
  StrUtils,
  UfrmCharacterIdentifier,
  UfrmCharacterMapDock,
  UfrmCharacterMapNew,
  UfrmMain,
  UmodWebHTTPServer,
  Unicode,
  utildir,
  utilfiletypes,
  utilhttp,
  VisualKeyboard;

{$R *.dfm}

{ TframeTouchLayoutBuilder }

procedure TframeTouchLayoutBuilder.FireCommand(const commands: TStringList);
var
  i: Integer;
  command: string;
begin
  i := 0;
  while i < commands.Count do
  begin
    command := commands[i];
    if command = 'modified' then DoModified   // I3948
    else if command = 'import' then DoImportFromOSK
    else if command = 'template' then DoSelectTemplate
    else if command = 'undo-disable' then FCanUndo := False
    else if command = 'undo-enable' then FCanUndo := True
    else if command = 'redo-disable' then FCanRedo := False
    else if command = 'redo-enable' then FCanRedo := True
    else if command.StartsWith('selected-char,') then
    begin
      Inc(i);
      UpdateCharacterMap(command.Substring('selected-char,'.Length));   // I4046
    end
    else ShowMessage('keyman:'+commands.Text);
    Inc(i);
  end;
end;

procedure TframeTouchLayoutBuilder.UpdateCharacterMap(code: string);   // I4046
var
  v: Integer;
begin
  if not FKeymanDeveloperOptions.CharMapAutoLookup then   // I4807
    Exit;

  if not TryStrToInt(code, v) then
    Exit;

  if frmCharacterMapDock.Visible then   // I4807
    frmCharacterMapNew.FindCharacter(v);
  if frmCharacterIdentifier.Visible then   // I4807
    frmCharacterIdentifier.Chars := Uni_UTF32CharToUTF16(v);
end;

procedure TframeTouchLayoutBuilder.FormCreate(Sender: TObject);
begin
  inherited;
  FDisplayScriptErrors := True;   // I4047
  FTemplateFileName := GetLayoutBuilderPath + 'template-basic' + Ext_KeymanTouchLayout;   // I4226

  cef := TframeCEFHost.Create(Self);
  cef.Parent := Self;
  cef.Visible := True;
  cef.OnBeforeBrowse := cefBeforeBrowse;
  cef.OnLoadEnd := cefLoadEnd;
end;

procedure TframeTouchLayoutBuilder.FormDestroy(Sender: TObject);
begin
  inherited;
  if FFilename <> '' then
    modWebHttpServer.AppSource.UnregisterSource(FFilename);
end;

procedure TframeTouchLayoutBuilder.ImportFromKVK(const KVKFileName: string);   // I3945

type
  TKeyData = record
    Data: TOnScreenKeyboardKeyData;
    VKey: Integer;
    Cap: string;
  end;

var
  I: Integer;
  f: AnsiString;
  n: Integer;
  FVK: TVisualKeyboard;
  nrow: Integer;
  LocalKeyData: array[0..58] of TKeyData;
  baseTemplate: AnsiString;
  baseTemplateLayer: AnsiString;
  FTouchLayout: TTouchLayout;
  FOldLayout: TTouchLayout;

    function GetKeyIndex(VK: Word): Integer;
    var
      I: Integer;
    begin
      for I := 0 to High(LocalKeyData) do
        if LocalKeyData[I].VKey = VK then
        begin
          Result := I;
          Exit;
        end;
      Result := -1;
    end;

    function FillOSK(Shift: Word): Boolean;
    var
      i, n: Integer;
      Found: Boolean;
    begin
      Found := False;
      for i := 0 to High(LocalKeyData) do
      begin
        LocalKeyData[i].Data := KeyData[i];
        LocalKeyData[i].VKey := MapScanCodeToUSVK(KeyData[i].ScanCode);
        LocalKeyData[i].Cap := '';   // I4146
      end;

      for i := 0 to FVK.Keys.Count - 1 do
      begin
        if (FVK.Keys[i].Shift = Shift) and (kvkkUnicode in FVK.Keys[i].Flags) then
        begin
          n := GetKeyIndex(FVK.Keys[i].VKey);
          if n < 0 then Continue;
          LocalKeyData[n].Cap := FVK.Keys[i].Text;
          Found := True;
        end;
      end;

      Result := Found;   // I4146
    end;

// string escaping and un-escaping

    function EscapeString(const s:WideString): ansistring;
    var
      i:Integer;
    begin
      Result := '"';
      for i:=1 to Length(s) do
        case s[i] Of
          '/', '\', '"': Result := Result + '\' + AnsiChar(s[i]);
          #8: Result := Result + '\b';
          #9: Result := Result + '\t';
          #10: Result := Result + '\n';
          #12: Result := Result + '\f';
          #13: Result := Result + '\r';
          else
            if CharInSet(s[i], [WideChar(' ') .. WideChar('~')])
              then Result := Result + AnsiChar(s[i])
              else Result := Result + '\u'+AnsiString(IntToHex(Ord(s[i]),4));
        end;
      Result := Result + '"';
    end;

type
  TLayoutShiftState = record Shift: Word; id: ansistring end;
const
  ShiftStates: array[0..21] of TLayoutShiftState = (
    (Shift: KVKS_NORMAL; id: 'default'),
    (Shift: KVKS_SHIFT; id: 'shift'),
    (Shift: KVKS_CTRL; id: 'ctrl'),
    (Shift: KVKS_SHIFT or KVKS_CTRL; id: 'shift-ctrl'),
    (Shift: KVKS_ALT; id: 'alt'),
    (Shift: KVKS_SHIFT or KVKS_ALT; id: 'shift-alt'),
    (Shift: KVKS_CTRL or KVKS_ALT; id: 'ctrl-alt'),
    (Shift: KVKS_SHIFT or KVKS_CTRL or KVKS_ALT; id: 'shift-ctrl-alt'),

    (Shift: KVKS_NORMAL; id: 'default'),
    (Shift: KVKS_LCTRL; id: 'leftctrl'),
    (Shift: KVKS_RCTRL; id: 'rightctrl'),
    (Shift: KVKS_LALT; id: 'leftalt'),
    (Shift: KVKS_LCTRL or KVKS_LALT; id: 'leftctrl-leftalt'),
    (Shift: KVKS_RALT; id: 'rightalt'),
    (Shift: KVKS_RCTRL or KVKS_RALT; id: 'rightctrl-rightalt'),
    (Shift: KVKS_SHIFT; id: 'shift'),
    (Shift: KVKS_SHIFT or KVKS_LCTRL; id: 'shift-lctrl'),
    (Shift: KVKS_SHIFT or KVKS_RCTRL; id: 'shift-rctrl'),
    (Shift: KVKS_SHIFT or KVKS_LALT; id: 'shift-lalt'),
    (Shift: KVKS_SHIFT or KVKS_RALT; id: 'shift-ralt'),
    (Shift: KVKS_SHIFT or KVKS_LCTRL or KVKS_LALT; id: 'shift-lctrl-lalt'),
    (Shift: KVKS_SHIFT or KVKS_RCTRL or KVKS_RALT; id: 'shift-rctrl-ralt')
  );
const
  NFirstChiralShiftState = 8;
var
  n1, n2: Integer;
begin
  if not FileExists(KVKFileName) then
  begin
    ShowMessage('This keyboard does not include an On Screen Keyboard file named '+KVKFileName);
    Exit;
  end;

  FVK := TVisualKeyboard.Create;
  with FVK do
  try
    try
      LoadFromFile(KVKFileName);
    except
      on E:Exception do
      begin
        ShowMessage(E.Message);
        Exit;
      end;
    end;

    baseTemplate := '{ "tablet": { "layer": [';
    f := '{ "desktop": { "font": '+EscapeString(Header.UnicodeFont.Name)+', "layer": [';

    if kvkhAltGr in FVK.Header.Flags then
    begin
      n1 := NFirstChiralShiftState;
      n2 := High(ShiftStates);
    end
    else
    begin
      n1 := 0;
      n2 := NFirstChiralShiftState - 1;
    end;

    with TStringList.Create do
    try
      LoadFromFile(GetLayoutBuilderPath + 'physical-keyboard-template.js');
      baseTemplateLayer := String_UtoA(Text);
    finally
      Free;
    end;

    for n := n1 to n2 do
    begin
      if not FillOSK(ShiftStates[n].Shift) then
      begin
        Continue;
      end;

      if n > n1 then
      begin
        f := f + ',';
        baseTemplate := baseTemplate + ',';
      end;

      f := f + '{' +
        '"id": "'+ShiftStates[n].id+'",'+
        '"row": [';

      baseTemplate := baseTemplate + '{' +
        '"id": "'+ShiftStates[n].id+'",' +
        baseTemplateLayer+
        '}';

      nrow := 1;
      for I := 0 to High(LocalKeyData) do
      begin
        if LocalKeyData[i].Data.Row then
        begin
          if I > 0 then
            f := f + ']},';
          f := f + '{"id": '+AnsiString(IntToStr(nrow))+',"key": [ ';
          Inc(nrow);
        end
        else
          f := f + ',';

        f := f + '{"id": "'+AnsiString(VKeyNames[LocalKeyData[i].VKey])+'",'+
                  '"text": '+EscapeString(LocalKeyData[i].Cap);
        if LocalKeyData[i].Data.Width <> 33 then
          f := f + ',"width": "'+AnsiString(IntToStr((LocalKeyData[i].Data.Width-1)*2))+'"';
        if LocalKeyData[i].Data.KeyType <> kktNormal then
          f := f + ',"sp": "1", "fontsize": "0.5em"';
        f := f + '}';
      end;

      f := f + ']}'+  // END key, last row
               ']}';  // END row array, layer
    end;
    f := f + ']}}';

    baseTemplate := baseTemplate + ']}}';
  finally
    Free;
  end;
  FSavedLayoutJS := String_AtoU(f);

  // Merge the two files -- the base template and the constructed KVK keys
  //Load(String_AtoU(baseTemplate),True,True);

  FTouchLayout := TTouchLayout.Create;   // I3642
  try
    if FTouchLayout.Load(String_AtoU(baseTemplate)) then
    begin
      FOldLayout := TTouchLayout.Create;
      try
        FOldLayout.Load(FSavedLayoutJS);
        if FTouchLayout.Merge(FOldLayout)
          then FSavedLayoutJS := FTouchLayout.Save(False);
      finally
        FOldLayout.Free;
      end;
    end;
  finally
    FTouchLayout.Free;
  end;

  // Setup modifier keys in the final keyboard
  SetupModifierKeysForImportedLayout;

  DoModified;
end;

{**
 * Sets up the K_LCONTROL modifier key to access all other layers for
 * each layer on each given platform. Will rewrite the K_LCONTROL key
 * or remove it if there are no extended layers.
 *}
procedure TframeTouchLayoutBuilder.SetupModifierKeysForImportedLayout;
  procedure SetupModifierKeysForPlatform(p: TTouchLayoutPlatform);
  var
    l, l2: TTouchLayoutLayer;
    k: TTouchLayoutKey;
    nExtendedLayers: Integer;
    FirstExtendedLayerID: string;
    sk: TTouchLayoutSubKey;
  begin
    nExtendedLayers := 0;

    for l in p.Layers do
    begin
      if (l.id <> 'default') and (l.Id <> 'shift') then
      begin
        Inc(nExtendedLayers);
        if FirstExtendedLayerID = '' then
          FirstExtendedLayerID := l.Id;
      end;
    end;

    for l in p.Layers do
    begin
      // Find the Ctrl key for the layer
      k := l.FindKeyById('K_LCONTROL');
      if not Assigned(k) then
        Continue;

      // If there are no extended layers, delete it
      if nExtendedLayers = 0 then
      begin
        (k.Parent as TTouchLayoutRow).Keys.Remove(k);
        // Stretch Spacebar?
        Continue;
      end;

      // If there are extended layers, set it up to
      // go to the first extended layer from Default, Shift, or
      // return to default from any other layer.
      // Long-press will go to other extended layers.

      if (l.id = 'default') or (l.Id = 'shift') then
      begin
        k.NextLayer := FirstExtendedLayerID;
        k.Text := FirstExtendedLayerID;
      end
      else
      begin
        k.NextLayer := 'default';
        k.Text := 'default';
      end;

      if nExtendedLayers > 1 then
      begin
        for l2 in p.Layers do
        begin
          if (l2.id = 'default') or (l2.Id = 'shift') then Continue;
          if l2.id = l.id then Continue;
          sk := TTouchLayoutSubKey.Create(k);
          sk.Id := 'K_LCONTROL';
          sk.Text := l2.id;
          sk.SpT := tktSpecial;
          sk.NextLayer := l2.Id;
          k.Sk.Add(sk);
        end;
      end;
    end;
  end;
var
  p: TTouchLayoutPlatform;
begin
  with TTouchLayout.Create do
  try
    Load(FSavedLayoutJS);
    for p in Data.Platforms do
    begin
      SetupModifierKeysForPlatform(p);
    end;
    FSavedLayoutJS := Write(True);
  finally
    Free;
  end;

  try
    DoLoad;
  except
    on E:Exception do
    begin
      ShowMessage(E.Message);
      Exit;
    end;
  end;
end;

function TframeTouchLayoutBuilder.IsModified: Boolean;
var
  s: string;
begin
  s := GetLayoutJS;   // I4047
  if s = ''
    then Result := False
    else Result := s <> FSavedLayoutJS;
end;

function TframeTouchLayoutBuilder.Load(const AFilename: string; ALoadFromTemplate, ALoadFromString: Boolean): Boolean;
var
  FBaseFileName: string;
  FNewLayoutJS: string;
  FTouchLayout: TTouchLayout;
  FOldLayout: TTouchLayout;
  function GetNextFilename: string;
  begin
    Inc(FInitialFilenameIndex);
    Result := '*'+IntToStr(FInitialFilenameIndex);
  end;
begin
  FLastErrorOffset := -1;
  FLastError := '';

//  FreeAndNil(FHTMLTempFilename);   // I4195
//  FHTMLTempFilename := TTempFileManager.Get('.html');   // I4195

  if ALoadFromString then
  begin
    FNewLayoutJS := AFilename;
    FFilename := GetNextFilename;
  end
  else
  begin
    if ALoadFromTemplate or (AFileName = '') or not FileExists(AFileName) then
    begin
      FBaseFileName := FTemplateFileName;
      FFilename := GetNextFilename;
    end
    else
    begin
      FBaseFileName := AFileName;
      FFilename := AFileName;
    end;

    with TStringList.Create do
    try
      LoadFromFile(FBaseFileName, TEncoding.UTF8);
      FNewLayoutJS := Text;
    finally
      Free;
    end;
  end;

  FTouchLayout := TTouchLayout.Create;   // I3642
  try
    if not FTouchLayout.Load(FNewLayoutJS) then
    begin
      FLastError := FTouchLayout.LoadError;   // I4083
      FLastErrorOffset := FTouchLayout.LoadErrorOffset;   // I4083
      Exit(False);
    end
    else
    begin
      if (FSavedLayoutJS <> '') and ALoadFromTemplate then
      begin
        FOldLayout := TTouchLayout.Create;
        try
          FOldLayout.Load(FSavedLayoutJS);
          if FTouchLayout.Merge(FOldLayout)
            then FSavedLayoutJS := FTouchLayout.Save(False)
            else FSavedLayoutJS := FNewLayoutJS;
        finally
          FOldLayout.Free;
        end;
      end
      else
        FSavedLayoutJS := FNewLayoutJS;
    end;
  finally
    FTouchLayout.Free;
  end;

  modWebHttpServer.AppSource.RegisterSource(FFilename, FSavedLayoutJS);

  try
    DoLoad;
  except
    on E:Exception do
    begin
      ShowMessage(E.Message);
      Exit(False);
    end;
  end;
  Result := True;
end;

procedure TframeTouchLayoutBuilder.DoLoad;   // I3642
begin
  FLoading := True;   // I4057
  cef.Navigate(modWebHttpServer.GetLocalhostURL + '/app/source/toucheditor?Filename='+URLEncode(FFilename));   // I4195
end;

function TframeTouchLayoutBuilder.GetFontInfo(Index: TKeyboardFont): TKeyboardFontInfo;   // I4057
var
  s: string;
  FIndexName: string;
  jsonfont: TJSONPair;
  jsonfontsize: TJSONPair;
begin
  Result.Enabled := False;

  s := GetLayoutJS;
  if s = '' then
    Exit;

  with TTouchLayout.Create do
  try
    if not Load(s) then
      Exit;

    case Index of
      kfontTouchLayoutPhone: FIndexName := 'phone';
      kfontTouchLayoutTablet: FIndexName := 'tablet';
      kfontTouchLayoutDesktop: FIndexName := 'desktop';
      else Exit;
    end;

    if not HasPlatform(FIndexName) then
      Exit;

    Result.Name := '';
    Result.Size := '';

    jsonfont := (LayoutPlatform[FIndexName].JsonValue as TJSONObject).Get('font');
    if Assigned(jsonfont) then Result.Name := jsonfont.JsonValue.Value;

    jsonfontsize := (LayoutPlatform[FIndexName].JsonValue as TJSONObject).Get('fontsize');
    if Assigned(jsonfontsize) then Result.Size := jsonfontsize.JsonValue.Value;

    Result.Enabled := True;
  finally
    Free;
  end;
end;

function TframeTouchLayoutBuilder.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_TouchLayoutBuilder;
end;

function TframeTouchLayoutBuilder.GetLayoutJS: string;
begin
  // TODO: layout builder needs to call Generate whenever it makes a change
  Result := modWebHttpServer.AppSource.GetSource(FFilename);
end;

procedure TframeTouchLayoutBuilder.Save(const AFilename: string);
var
  s: string;
begin
  if not IsModified and FileExists(AFilename) then   // I4047   // I4147
    Exit;
  s := GetLayoutJS;
  if s = '' then
    Exit;
  FSavedLayoutJS := s;
  with TStringStream.Create(FSavedLayoutJS, TEncoding.UTF8) do
  try
    SaveToFile(AFileName);
  finally
    Free;
  end;
end;

function TframeTouchLayoutBuilder.SaveToString: string;
begin
  FSavedLayoutJS := GetLayoutJS;
  Result := FSavedLayoutJS;
end;

procedure TframeTouchLayoutBuilder.DoModified;   // I3948
begin
  if Assigned(FOnModified) then FOnModified(Self);
end;

procedure TframeTouchLayoutBuilder.DoSelectTemplate;
begin
  if Assigned(FOnSelectTemplateCommand) then FOnSelectTemplateCommand(Self);
end;

procedure TframeTouchLayoutBuilder.DoImportFromOSK;
begin
  if Assigned(FOnImportFromOSKCommand) then FOnImportFromOSKCommand(Self);
end;

procedure TframeTouchLayoutBuilder.cefBeforeBrowse(Sender: TObject;
  const Url: string; out Result: Boolean);
var
  params: TStringList;
begin
  if csDestroying in ComponentState then   // I3983
  begin
    Result := True;
    Exit;
  end;

  if GetParamsFromURL(URL, params) then
  begin
    PostMessage(Handle, WM_USER_FireCommand, 0, Integer(params));
    Result := True;
  end;
end;

procedure TframeTouchLayoutBuilder.cefLoadEnd(Sender: TObject);
begin
  FLoading := False;
end;

procedure TframeTouchLayoutBuilder.WMUser_FireCommand(var Message: TMessage);
var
  params: TStringList;
begin
  params := TStringList(Message.LParam);
  if (params.Count > 0) and (params[0] = 'command') then
  begin
    params.Delete(0);
    FireCommand(params);
  end;
  params.Free;
end;

function TframeTouchLayoutBuilder.BuilderCommand(const cmd: string): Boolean;
begin
  Result := False;
  if FLoading then Exit;   // I4057
  FDisplayScriptErrors := False;   // I4047
  try
    try
      cef.cef.ExecuteJavaScript('builder.'+cmd+'();', '');
    except
      // Ignore errors
    end;
  finally
    FDisplayScriptErrors := True;   // I4047
  end;
end;

{ IKMDEditActions }

function TframeTouchLayoutBuilder.CanClearSelection: Boolean;
begin
  Result := True; //web.IsCommandEnabled('Delete');
end;

function TframeTouchLayoutBuilder.CanCopy: Boolean;
begin
  Result := True; //web.IsCommandEnabled('Copy');
end;

function TframeTouchLayoutBuilder.CanCut: Boolean;
begin
  Result := True; //web.IsCommandEnabled('Cut');
end;

function TframeTouchLayoutBuilder.CanPaste: Boolean;
begin
  Result := True; //web.IsCommandEnabled('Paste');
end;

function TframeTouchLayoutBuilder.CanRedo: Boolean;
begin
  Result := FCanRedo;
end;

function TframeTouchLayoutBuilder.CanSelectAll: Boolean;
begin
  Result := True; //web.IsCommandEnabled('SelectAll');
end;

function TframeTouchLayoutBuilder.CanUndo: Boolean;
begin
  Result := FCanUndo;
end;

procedure TframeTouchLayoutBuilder.ClearSelection;
begin
  cef.cef.ClipboardDel;
end;

procedure TframeTouchLayoutBuilder.CopyToClipboard;
begin
  cef.cef.ClipboardCopy;
end;

procedure TframeTouchLayoutBuilder.CutToClipboard;
begin
  cef.cef.ClipboardCut;
end;

procedure TframeTouchLayoutBuilder.SelectAll;
begin
  cef.cef.SelectAll;
end;

procedure TframeTouchLayoutBuilder.SetFontInfo(Index: TKeyboardFont; const Value: TKeyboardFontInfo);   // I4057
var
  s: string;
begin
  s := GetLayoutJS;
  if s = '' then
    Exit;

  if not UpdateTouchLayoutFont(s, Index, Value.Name, Value.Size) then
    Exit;

  modWebHTTPServer.AppSource.SetSource(FFilename, s);

  try
    DoLoad;
  except
    on E:Exception do
    begin
      ShowMessage(E.Message);
      Exit;
    end;
  end;

  DoModified;
end;

procedure TframeTouchLayoutBuilder.Undo;
begin
  BuilderCommand('undo');
end;

procedure TframeTouchLayoutBuilder.PasteFromClipboard;
begin
  cef.cef.ClipboardPaste;
end;

procedure TframeTouchLayoutBuilder.Redo;
begin
  BuilderCommand('redo');
end;

end.
