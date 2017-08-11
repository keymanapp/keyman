unit UfrmTouchKeyboard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw_EWB, EwbCore,
  EmbeddedWB, KeymanEmbeddedWB, UfrmWebContainer, UserMessages,
  Vcl.ExtCtrls;

type
  TfrmTouchKeyboard = class(TfrmWebContainer)
    tmrWatchTouchLayout: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure webDocumentComplete(ASender: TObject; const pDisp: IDispatch;
      var URL: OleVariant);
    procedure webScriptError2(Sender: TObject; ErrorLine, ErrorCharacter,
      ErrorCode, ErrorMessage, ErrorUrl: string;
      var ScriptErrorAction: TScriptErrorAction);
    procedure webBeforeNavigate2(ASender: TObject; const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData, Headers: OleVariant;
      var Cancel: WordBool);
    procedure tmrWatchTouchLayoutTimer(Sender: TObject);
  private
    FContext: string;
    IsReady: Boolean;
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
    procedure UpdateContext;
  protected
    procedure Content_Render(FRefreshKeyman: Boolean = False;
      const AdditionalData: WideString = ''); override;
    { Private declarations }
  public
    { Public declarations }
    procedure SetContext(const AContext: string);
    procedure RefreshSelectedKeyboard;
  end;

var
  frmTouchKeyboard: TfrmTouchKeyboard;

implementation

uses
  DebugPaths,
  ErrorControlledRegistry,
  keymanapi_TLB,
  RegistryKeys,
  System.StrUtils,
  UfrmKeyman7Main,
  XMLRenderer;

{$R *.dfm}

function GetTouchKeyboardPath: string;
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanEngine) and ValueExists(SRegValue_RootPath)
      then Result := IncludeTrailingPathDelimiter(ReadString(SRegValue_RootPath)) + 'xml\touch\'
      else Result := ExtractFilePath(ParamStr(0)) + 'xml\touch\';
  finally
    Free;
  end;
  Result := GetDebugPath('Debug_TouchKeyboardPath', Result, True);
end;

procedure TfrmTouchKeyboard.Content_Render(FRefreshKeyman: Boolean;
  const AdditionalData: WideString);
var
  FTemplatePath: WideString;
const
  STemplateName = 'touchkeyboard.html';
begin
  FTemplatePath := GetTouchKeyboardPath+STemplateName;
  web.Navigate(FTemplatePath);
end;

procedure TfrmTouchKeyboard.FormCreate(Sender: TObject);
var
  n: Integer;
  h: THandle;
  r: TRect;
begin
  h := FindWindow('IPTIP_Main_Window', nil);
  if h <> 0 then
  begin
    GetWindowRect(h, r);
//    n := Screen.Height * 4 div 10;
    SetBounds(r.Left, r.Top, r.Width, r.Height);
  end
  else
  begin
      // DEBUG ONLY
    n := Screen.WorkAreaHeight * 4 div 10;
    SetBounds(Screen.WorkAreaLeft, Screen.WorkAreaHeight - n, Screen.WorkAreaWidth, n);
  end;
end;

procedure TfrmTouchKeyboard.FormShow(Sender: TObject);
begin
  Content_Render;
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

procedure TfrmTouchKeyboard.RefreshSelectedKeyboard;
var
  kbd: IKeymanKeyboardInstalled;
  FPath: string;
  FKeyboardName: string;

  function PathToFileURL(s: string): string;
  var
    i: Integer;
  begin
    Result := 'file:///';
    for i := 1 to Length(s) do
      case s[i] of
        '\': Result := Result + '/';
        ':','.','a'..'z','A'..'Z','0'..'9','_','-': Result := Result + s[i];
        else Result := Result + '%'+IntToHex(Ord(s[i]), 2);
      end;

    //Result := 'file:///'+ReplaceStr(s, '\', '/');
  end;
begin
  if not IsReady then
    Exit;

  kbd := frmKeyman7Main.ActiveKeyboard;
  if not Assigned(kbd) then
  begin
    web.ExecScript('setKeymanLanguage("English","us","./languages/us.js")', 'JScript'); // TODO escaping, figure out non-Keyman keyboard interaction
  end
  else
  begin
    FPath := ChangeFileExt(kbd.Filename, '.js');
    FKeyboardName := LowerCase(ChangeFileExt(ExtractFileName(FPath), ''));
    if FileExists(FPath) then
    begin
      web.ExecScript('setKeymanLanguage("'+kbd.Name+'", "'+FKeyboardName+'", "'+PathToFileURL(FPath)+'")', 'JScript'); //TODO escape
    end;
  end;
end;

procedure TfrmTouchKeyboard.SetContext(const AContext: string);
begin
  FContext := AContext;
  UpdateContext;
end;

procedure TfrmTouchKeyboard.tmrWatchTouchLayoutTimer(Sender: TObject);
var
  h: THandle;
  r: TRect;
begin
  h := FindWindow('IPTIP_Main_Window', nil);
  if h <> 0 then
  begin
    GetWindowRect(h, r);
//    n := Screen.Height * 4 div 10;
    SetBounds(r.Left, r.Top, r.Width, r.Height);
    if IsWindowVisible(h) and IsWindowEnabled(h) then
    begin
      Visible := True;
      SetWindowPos(Handle, HWND_TOPMOST, r.Left, r.Top, r.Width, r.Height, SWP_NOACTIVATE);
    end
    else
      Visible := False;
  end
  else
    Visible := False;
end;

procedure TfrmTouchKeyboard.UpdateContext;
begin
  if web.DocumentLoaded then
    web.ExecScript('setContext("'+FContext+'")', 'JavaScript');   // TODO escaping
end;

procedure TfrmTouchKeyboard.webBeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  inherited;
  if not Cancel then
    IsReady := False;
end;

procedure TfrmTouchKeyboard.webDocumentComplete(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  UpdateContext;
  IsReady := True;
  // Do nothing
end;

procedure TfrmTouchKeyboard.webScriptError2(Sender: TObject; ErrorLine,
  ErrorCharacter, ErrorCode, ErrorMessage, ErrorUrl: string;
  var ScriptErrorAction: TScriptErrorAction);
begin
  ShowMessage(ErrorMessage + #13#10'('+ErrorCharacter+','+ErrorLine+': '+ErrorUrl+')');
  ScriptErrorAction := eaContinue;
end;

procedure TfrmTouchKeyboard.WMUserFormShown(var Message: TMessage);
begin
  FormStyle := fsStayOnTop;   // I4209
end;

end.
