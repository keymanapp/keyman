unit Keyman.Developer.UI.UfrmLdmlKeyboardEditor;

interface

uses
  System.Classes,
  System.ImageList,
  System.SysUtils,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.ImgList,
  Winapi.Messages,
  Winapi.Windows,

  MenuImgList,
  UfrmEditor,
  Keyman.Developer.UI.Debug.UfrmLdmlKeyboardDebug;

type
  TfrmLdmlKeyboardEditor = class(TfrmEditor)
    panDebugHost: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
  private
    FDebugForm: TfrmLdmlKeyboardDebug;
    function GetIsDebugVisible: Boolean;
    procedure SetupDebugForm;
  protected
    procedure LoadSettings; override;
    procedure SaveSettings(SaveProject: Boolean); override;
    function GetHelpTopic: string; override;
  public
    procedure StartDebugging;
    procedure StopDebugging;
    function PrepareForBuild(var DebugReset: Boolean): Boolean;
    property DebugForm: TfrmLdmlKeyboardDebug read FDebugForm;
    property IsDebugVisible: Boolean read GetIsDebugVisible;
  end;

implementation

uses
  keymanstrings,
  KeymanDeveloperOptions,
  Keyman.Developer.System.HelpTopics,
  Keyman.System.Debug.DebugUIStatus,
  Keyman.Developer.System.Project.xmlLdmlProjectFile,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.xmlLdmlProjectFileUI,
  Keyman.Developer.UI.UfrmMessageDlgWithSave,
  KeymanDeveloperUtils,
  TextFileFormat,
  UfrmMessages;

{$R *.dfm}

{ TfrmLdmlKeyboardEditor }

procedure TfrmLdmlKeyboardEditor.FormCreate(Sender: TObject);
begin
  inherited;
  EditorFormat := efXML;
  SetupDebugForm;
end;

function TfrmLdmlKeyboardEditor.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_LdmlEditor;
end;

function TfrmLdmlKeyboardEditor.GetIsDebugVisible: Boolean;
begin
  Result := panDebugHost.Visible;
end;

procedure TfrmLdmlKeyboardEditor.SetupDebugForm;
begin
  panDebugHost.Visible := False;

  FDebugForm := TfrmLdmlKeyboardDebug.Create(Self);
  FDebugForm.BorderStyle := bsNone;
  FDebugForm.Parent := panDebugHost;
  FDebugForm.Align := alClient;
  FDebugForm.Visible := True;
  FDebugForm.EditorMemo := EditorFrame;
end;

procedure TfrmLdmlKeyboardEditor.StartDebugging;
begin
  if not IsDebugVisible then
  begin
    (ProjectFileUI as TxmlLdmlProjectFileUI).Debug := True;   // I4687

    frmMessages.Clear;   // I4686

    if not (ProjectFileUI as TxmlLdmlProjectFileUI).DoAction(pfaCompile, False) then
    begin
      ShowMessage(SKErrorsInCompile);
      Exit;
    end;

    if not FileExists((ProjectFile as TxmlLdmlProjectFile).TargetFilename) then
    begin
      ShowMessage(SKKeyboardKMXDoesNotExist);
      Exit;
    end;

    if FDebugForm.DefaultFont then
      FDebugForm.UpdateFont(nil);
    FDebugForm.DebugFileName := FileName;
    FDebugForm.CompiledFileName := (ProjectFile as TxmlLdmlProjectFile).TargetFilename;   // I4695
    FDebugForm.ShowDebugForm;
  end;
end;

procedure TfrmLdmlKeyboardEditor.StopDebugging;
begin
  FDebugForm.HideDebugForm;
  panDebugHost.Visible := False;
  EditorFrame.SetFocus;
end;

procedure TfrmLdmlKeyboardEditor.LoadSettings;
var
  FFont: TFont;
begin
  inherited;

  if ProjectFile.IDEState['DebugDefaultFont'] <> '0' then   // I4702
    FDebugForm.UpdateFont(nil)
  else
  begin
    FFont := TFont.Create;
    try
      SetFontFromString(FFont, ProjectFile.IDEState['DebugFont']);   // I4702
      FDebugForm.UpdateFont(FFont);
    finally
      FFont.Free;
    end;
  end;
end;

procedure TfrmLdmlKeyboardEditor.SaveSettings(SaveProject: Boolean);
begin
  if FDebugForm.DefaultFont
    then ProjectFile.IDEState['DebugDefaultFont'] := '1'   // I4702
    else ProjectFile.IDEState['DebugDefaultFont'] := '0';   // I4702

  ProjectFile.IDEState['DebugFont'] := FontAsString(FDebugForm.memo.Font);   // I4702

  inherited;
end;

function TfrmLdmlKeyboardEditor.PrepareForBuild(var DebugReset: Boolean): Boolean;
var
  FSave: Boolean;
begin
  if IsDebugVisible then
  begin
    if not FKeymanDeveloperOptions.DebuggerAutoResetBeforeCompiling then
    begin
      if TfrmMessageDlgWithSave.Execute(
          'You must reset the debugger before recompiling your keyboard.  Reset the debugger and recompile?',
          'Always reset the debugger automatically before compiling',
          '', True, FSave) in [mrNo, mrCancel] then
        Exit(False);
      if FSave then
      begin
        FKeymanDeveloperOptions.DebuggerAutoResetBeforeCompiling := True;
        FKeymanDeveloperOptions.Write;
      end;
    end;

    DebugReset := True;
    StopDebugging;
  end;

  Result := True;
end;


end.
