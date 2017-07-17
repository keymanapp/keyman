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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmTike, Vcl.OleCtrls, SHDocVw_EWB,
  EwbCore, EmbeddedWB, KeymanEmbeddedWB, UserMessages, KMDActionInterfaces,
  KeyboardFonts, TempFileManager;

type
  TframeTouchLayoutBuilder = class(TTikeForm, IKMDEditActions)   // I4034
    web: TKeymanEmbeddedWB;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure webBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
    procedure webTranslateAccelerator(Sender: TCustomEmbeddedWB;
      const lpMsg: PMsg; const pguidCmdGroup: PGUID; const nCmdID: Cardinal;
      var Done: Boolean);
    procedure webScriptError(Sender: TObject; ErrorLine, ErrorCharacter,
      ErrorCode, ErrorMessage, ErrorUrl: string;
      var ScriptErrorAction: TScriptErrorAction);
    procedure webDocumentComplete(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);   // I4057
  private
    FLoading: Boolean;   // I4057
    FTemplateFileName: string;
    FOnModified: TNotifyEvent;
    FSavedLayoutJS: string;
    FHTMLTempFilename: TTempFile;   // I4195
    FOnImportFromOSKCommand: TNotifyEvent;
    FOnSelectTemplateCommand: TNotifyEvent;
    FCanUndo: Boolean;
    FCanRedo: Boolean;
    FDisplayScriptErrors: Boolean;
    FLastError: string;   // I4083
    FLastErrorOffset: Integer;   // I4083
    function GetLayoutJS: string;
    procedure WMUser_FireCommand(var Message: TMessage); message WM_USER_FireCommand;
    procedure DoModified;
    procedure DoLoad;
    procedure FireCommand(const command: WideString; params: TStringList); virtual;
    procedure DoSelectTemplate;
    procedure DoImportFromOSK;
    function BuilderCommand(const cmd: string): Boolean;
    procedure UpdateCharacterMap(code: string);  // I4046
    function GetFontInfo(Index: TKeyboardFont): TKeyboardFontInfo;   // I4057
    procedure SetFontInfo(Index: TKeyboardFont; const Value: TKeyboardFontInfo);   // I4057
    { Private declarations }
  protected

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
    property LastErrorOffset: Integer read FLastErrorOffset;   // I4083
  end;

implementation

uses
  System.JSON,

  mshtml_ewb,
  mshtmcid,
  xmlintf,
  xmldoc,

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
  Unicode,
  utildir,
  utilhttp,
  VisualKeyboard;

{$R *.dfm}

{ TframeTouchLayoutBuilder }

procedure TframeTouchLayoutBuilder.FireCommand(const command: WideString;
  params: TStringList);
begin
  if command = 'modified' then DoModified   // I3948
  else if command = 'import' then DoImportFromOSK
  else if command = 'template' then DoSelectTemplate
  else if command = 'undo-disable' then FCanUndo := False
  else if command = 'undo-enable' then FCanUndo := True
  else if command = 'redo-disable' then FCanRedo := False
  else if command = 'redo-enable' then FCanRedo := True
  else if command = 'selected-char' then UpdateCharacterMap(params.Values['code'])   // I4046
  else ShowMessage(command + '?' + params.Text);
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
  FTemplateFileName := GetLayoutBuilderPath + 'template-basic.js';   // I4226
end;

procedure TframeTouchLayoutBuilder.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FHTMLTempFilename);   // I4023   // I4195
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
    begin
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
        end;
      end;

      Result := True;   // I4146
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
  ShiftStates: array[0..1] of TLayoutShiftState = (
    (Shift: KVKS_NORMAL; id: 'default'),
    (Shift: KVKS_SHIFT; id: 'shift')
  );

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

    f := '{ "desktop": { "font": '+EscapeString(Header.UnicodeFont.Name)+', "layer": [';

    for n := 0 to High(ShiftStates) do
    begin
      if not FillOSK(ShiftStates[n].Shift) then
      begin
        Continue;
      end;

      if n > 0 then
        f := f + ',';
      f := f + '{' +
        '"id": "'+ShiftStates[n].id+'",'+
        '"row": [';

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
  finally
    Free;
  end;
  FSavedLayoutJS := String_AtoU(f);

  FTemplateFileName := GetLayoutBuilderPath + 'template-traditional.js';
  Load('',True,False);
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
  FBaseFileName: string;
  FNewLayoutJS: string;
  FTouchLayout: TTouchLayout;
  FOldLayout: TTouchLayout;
begin
  FLastErrorOffset := -1;
  FLastError := '';

  FreeAndNil(FHTMLTempFilename);   // I4195
  FHTMLTempFilename := TTempFileManager.Get('.html');   // I4195

  if ALoadFromString then
    FNewLayoutJS := AFilename
  else
  begin
    if ALoadFromTemplate or (AFileName = '') or not FileExists(AFileName)
      then FBaseFileName := FTemplateFileName
      else FBaseFileName := AFileName;

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
var
  FStyleSheet: IXMLDocument;
  root: IXMLNode;
  output: WideString;
begin
  FLoading := True;   // I4057

  with NewXMLDocument do
  begin
    root := AddChild('TouchLayoutBuilder');
    //root.AddChild('FileName').NodeValue := FBaseFileName;
    root.AddChild('LibPath').NodeValue := ConvertPathToFileURL(GetLayoutBuilderPath);
    root.AddChild('LayoutJS').NodeValue := FSavedLayoutJS;
    FStyleSheet := LoadXMLDocument(GetLayoutBuilderPath + 'builder.xsl');
    root.TransformNode(FStyleSheet.DocumentElement, output);
  end;

  with TStringStream.Create(output, TEncoding.UTF8) do
  try
    SaveToFile(FHTMLTempFilename.Name);   // I4195
  finally
    Free;
  end;
  web.Navigate(FHTMLTempFilename.Name);   // I4195
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

function TframeTouchLayoutBuilder.GetLayoutJS: string;
var
  doc: IHTMLDocument2;
  data: IHTMLElement;
begin
  Result := FSavedLayoutJS;   // I4057
  if not BuilderCommand('generate') then
    Exit;
  doc := web.GetDocument;
  if not Assigned(doc) then Exit;
  data := (doc as IHTMLDocument3).getElementById('data');
  if not Assigned(data) then Exit;
  Result := (data as IHTMLTextAreaElement).value;
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

procedure TframeTouchLayoutBuilder.webBeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  params: TStringList;
begin
  if csDestroying in ComponentState then   // I3983
  begin
    Cancel := True;
    Exit;
  end;

  if GetParamsFromURL(URL, params) then
  begin
    PostMessage(Handle, WM_USER_FireCommand, 0, Integer(params));
    Cancel := True;
  end;
end;

procedure TframeTouchLayoutBuilder.webDocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);   // I4057
begin
  FLoading := False;
end;

procedure TframeTouchLayoutBuilder.webScriptError(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string;
  var ScriptErrorAction: TScriptErrorAction);
begin
  if FDisplayScriptErrors then   // I4047
    ShowMessage(ErrorMessage + ' ['+ErrorUrl+':'+ErrorLine+','+ErrorCharacter+']');
  ScriptErrorAction := TScriptErrorAction.eaContinue;
end;

procedure TframeTouchLayoutBuilder.webTranslateAccelerator(
  Sender: TCustomEmbeddedWB; const lpMsg: PMsg; const pguidCmdGroup: PGUID;
  const nCmdID: Cardinal; var Done: Boolean);   // I3895
begin
  Done := False;
  if (lpMsg <> nil) then
  begin
    if (lpMsg.message = WM_SYSKEYDOWN) then
    begin
      Done := False;
    end
    else if (lpMsg.message = WM_SYSCHAR) then
    begin
      SendMessage(Handle, WM_SYSCOMMAND, SC_KEYMENU, lpMsg.wParam);
      Done := True;
    end
    else if (lpMsg.message = WM_KEYDOWN) then
    begin
      if SendMessage(Application.Handle, CM_APPKEYDOWN, lpMsg.wParam, lpMsg.lParam) = 1 then
      begin
        Done := True;
      end;
    end;
  end;
end;

procedure TframeTouchLayoutBuilder.WMUser_FireCommand(var Message: TMessage);
var
  command: WideString;
  params: TStringList;
begin
  params := TStringList(Message.LParam);
  command := params[0];
  params.Delete(0);
  FireCommand(command, params);
  params.Free;
end;

function TframeTouchLayoutBuilder.BuilderCommand(const cmd: string): Boolean;
var
  Doc: IHTMLDocument2; // current HTML document
  HTMLWin: IHTMLWindow2; // parent window of current HTML document
begin
  Result := False;
  if FLoading then Exit;   // I4057
  FDisplayScriptErrors := False;   // I4047
  try
    try
      if web.DocumentLoaded(Doc) then
      begin
        HTMLWin := Doc.parentWindow;
        if Assigned(HTMLWin) then
        begin
          HTMLWin.execScript('builder.'+cmd+'();', 'JScript');   // I4047
          Result := True;
        end;
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
  Result := web.IsCommandEnabled('Delete');
end;

function TframeTouchLayoutBuilder.CanCopy: Boolean;
begin
  Result := web.IsCommandEnabled('Copy');
end;

function TframeTouchLayoutBuilder.CanCut: Boolean;
begin
  Result := web.IsCommandEnabled('Cut');
end;

function TframeTouchLayoutBuilder.CanPaste: Boolean;
begin
  Result := web.IsCommandEnabled('Paste');
end;

function TframeTouchLayoutBuilder.CanRedo: Boolean;
begin
  Result := FCanRedo;
end;

function TframeTouchLayoutBuilder.CanSelectAll: Boolean;
begin
  Result := web.IsCommandEnabled('SelectAll');
end;

function TframeTouchLayoutBuilder.CanUndo: Boolean;
begin
  Result := FCanUndo;
end;

procedure TframeTouchLayoutBuilder.ClearSelection;
begin
  web.Delete;
end;

procedure TframeTouchLayoutBuilder.CopyToClipboard;
begin
  web.Copy;
end;

procedure TframeTouchLayoutBuilder.CutToClipboard;
begin
  web.Cut;
end;

procedure TframeTouchLayoutBuilder.SelectAll;
begin
  web.SelectAll;
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

  FSavedLayoutJS := s;

  FreeAndNil(FHTMLTempFilename);   // I4023   // I4195
  FHTMLTempFilename := TTempFileManager.Get('.html');   // I4195

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
  web.Paste;
end;

procedure TframeTouchLayoutBuilder.Redo;
begin
  BuilderCommand('redo');
end;

end.
