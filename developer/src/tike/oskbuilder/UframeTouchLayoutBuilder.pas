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
  System.Classes,
  System.JSON,
  System.SysUtils,
  System.Variants,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,

  Keyman.UI.UframeCEFHost,

  KeyboardFonts,
  KMDActionInterfaces,
  TempFileManager,
  UfrmTike,
  UserMessages;

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
    procedure DoModified;
    procedure DoLoad;
    procedure FireCommand(const commands: TStringList); virtual;
    procedure DoSelectTemplate;
    procedure DoImportFromOSK;
    function BuilderCommand(const cmd: string; const parameters: TJSONValue = nil): Boolean;
    procedure UpdateCharacterMap(code: string);  // I4046
    function GetFontInfo(Index: TKeyboardFont): TKeyboardFontInfo;   // I4057
    procedure SetFontInfo(Index: TKeyboardFont; const Value: TKeyboardFontInfo);   // I4057

    procedure cefCommand(Sender: TObject; const command: string; params: TStringList);
    procedure cefLoadEnd(Sender: TObject);
    procedure RegisterSources(const AState: string);
    procedure CharMapDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CharMapDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure UnregisterSources;
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
    procedure SetFocus; override;
    procedure SetupCharMapDrop;
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
  System.TypInfo,

  xmlintf,
  xmldoc,

  Keyman.Developer.System.HelpTopics,
  Keyman.Developer.System.TouchLayoutToVisualKeyboardConverter,

  CharacterDragObject,
  CharMapDropTool,
  CharMapInsertMode,
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
  cef.OnCommand := cefCommand;
  cef.OnLoadEnd := cefLoadEnd;
  SetupCharMapDrop;
end;

procedure TframeTouchLayoutBuilder.FormDestroy(Sender: TObject);
begin
  inherited;
  UnregisterSources;
end;

procedure TframeTouchLayoutBuilder.UnregisterSources;
begin
  if (FFileName <> '') and modWebHttpServer.AppSource.IsSourceRegistered(FFilename) then
    modWebHttpServer.AppSource.UnregisterSource(FFilename);
  if (FFileName <> '') and modWebHttpServer.AppSource.IsSourceRegistered(FFilename+'#state') then
    modWebHttpServer.AppSource.UnregisterSource(FFilename+'#state');
end;

procedure TframeTouchLayoutBuilder.RegisterSources(const AState: string);
begin
  if FFilename <> '' then
    modWebHttpServer.AppSource.RegisterSource(FFilename, FSavedLayoutJS);
  if (FFileName <> '') and (AState <> '') then
    modWebHttpServer.AppSource.RegisterSource(FFilename + '#state', AState, True);
end;

procedure TframeTouchLayoutBuilder.ImportFromKVK(const KVKFileName: string);   // I3945
var
  converter: TTouchLayoutToVisualKeyboardConverter;
  FVK: TVisualKeyboard;
  FJS: string;
begin
  if not FileExists(KVKFileName) then
  begin
    ShowMessage('This keyboard does not include an On Screen Keyboard file named '+KVKFileName);
    Exit;
  end;

  FVK := TVisualKeyboard.Create;
  try
    try
      FVK.LoadFromFile(KVKFileName);
    except
      on E:Exception do
      begin
        ShowMessage(E.Message);
        Exit;
      end;
    end;

    converter := TTouchLayoutToVisualKeyboardConverter.Create(FVK);
    try
      if not converter.Execute(FJS) then
      begin
        ShowMessage('The converter failed to run.');
        Exit;
      end;
    finally
      converter.Free;
    end;
  finally
    FVK.Free;
  end;

  try
    Load(FJS, False, True);
  except
    on E:Exception do
    begin
      ShowMessage(E.Message);
      Exit;
    end;
  end;

  FSavedLayoutJS := '';

  DoModified;
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
  FLastFilename: string;
  FBaseFileName: string;
  FNewLayoutJS: string;
  FTouchLayout: TTouchLayout;
  FOldLayout: TTouchLayout;
  FState: string;
  function GetNextFilename: string;
  begin
    Inc(FInitialFilenameIndex);
    Result := '*'+IntToStr(FInitialFilenameIndex);
  end;
begin
  FLastErrorOffset := -1;
  FLastError := '';
  FLastFilename := FFilename;

  if FFilename <> '' then
  begin
    if not modWebHttpServer.AppSource.TryGetSource(FFilename + '#state', FState) then
      FState := '';
  end;

  UnregisterSources;

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
      FFilename := FLastFilename;
      RegisterSources(FState);
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

  if (FFileName <> '') and modWebHttpServer.AppSource.IsSourceRegistered(FFileName) then
  begin
    // If two .kmn files are loaded which both reference the same
    // .keyman-touch-layout file, it's safest to just block it. This is a rare
    // scenario, as most keyboard projects have a single .kmn, and it usually
    // indicates a project may be in a bit of chaos anyway.
    ShowMessage(
      'The touch layout is already opened for editing in another keyboard '+
      'editor. Please close the other keyboard editor before opening this one '+
      'again.');

    // We want to prevent this window unregistering the sources it doesn't own
    // when it is destroyed immediately after this, which we can do by blanking
    // the filename.
    FFileName := '';
    Exit(False);
  end;

  RegisterSources(FState);

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
  if (FFilename = '') and (AFilename <> '') then
  begin
    Load(AFilename, not FileExists(AFilename), False);
  end;

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

procedure TframeTouchLayoutBuilder.cefCommand(Sender: TObject; const command: string; params: TStringList);
begin
  if csDestroying in ComponentState then   // I3983
    Exit;

  if command = 'command' then
  begin
    FireCommand(params);
  end;
end;

procedure TframeTouchLayoutBuilder.cefLoadEnd(Sender: TObject);
begin
  FLoading := False;
end;

function TframeTouchLayoutBuilder.BuilderCommand(const cmd: string; const parameters: TJSONValue): Boolean;
begin
  Result := False;
  if FLoading then Exit;   // I4057
  FDisplayScriptErrors := False;   // I4047
  try
    try
      if Assigned(parameters) then
      begin
        cef.cef.ExecuteJavaScript('builder.'+cmd+'('+parameters.ToJSON+')', '');
      end
      else
      begin
        cef.cef.ExecuteJavaScript('builder.'+cmd+'();', '');
      end;
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

procedure TframeTouchLayoutBuilder.SetFocus;
begin
  cef.SetFocus;
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


procedure TframeTouchLayoutBuilder.CharMapDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  j: TJSONObject;
begin
  cef.cefwp.SetFocus;
  j := TJSONObject.Create;
  try
    j.AddPair('x', TJSONNumber.Create(X));
    j.AddPair('y', TJSONNumber.Create(Y));
    j.AddPair('state', GetEnumName(TypeInfo(TDragState), Ord(State)));
    BuilderCommand('charmapDragOver', j);
  finally
    j.Free;
  end;

  // We cannot test acceptance via event because it is asynchronous
  // So we will just assume we can accept and throw it away if it is outside bounds
  // during drop.
  Accept := True;
end;

procedure TframeTouchLayoutBuilder.CharMapDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  j: TJSONObject;
  cdo: TCharacterDragObject;
begin
  cef.cefwp.SetFocus;
  cdo := Source as TCharacterDragObject;
  j := TJSONObject.Create;
  try
    j.AddPair('x', TJSONNumber.Create(X));
    j.AddPair('y', TJSONNumber.Create(Y));
    j.AddPair('text', cdo.Text[cmimCharacter]); // It never makes sense to drop anything other than char
    if GetKeyState(VK_SHIFT) < 0 then
      j.AddPair('shift', TJSONBool.Create(True));
    if GetKeyState(VK_CONTROL) < 0 then
      j.AddPair('ctrl', TJSONBool.Create(True));
    if GetKeyState(VK_MENU) < 0 then
      j.AddPair('alt', TJSONBool.Create(True));
    BuilderCommand('charmapDragDrop', j);
  finally
    j.Free;
  end;
end;

procedure TframeTouchLayoutBuilder.SetupCharMapDrop;
begin
  GetCharMapDropTool.Handle(cef, cmimDefault, CharMapDragOver, CharMapDragDrop);
end;

end.
