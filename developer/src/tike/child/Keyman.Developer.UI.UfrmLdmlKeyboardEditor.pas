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
    { Private declarations }
  public
    { Public declarations }
    procedure StartDebugging;
    procedure StopDebugging;
    property DebugForm: TfrmLdmlKeyboardDebug read FDebugForm;
    property IsDebugVisible: Boolean read GetIsDebugVisible;
  end;

implementation

uses
  keymanstrings,
  Keyman.System.Debug.DebugUIStatus,
  Keyman.Developer.System.Project.xmlLdmlProjectFile,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.xmlLdmlProjectFileUI,
  UfrmMessages;

{$R *.dfm}

{ TfrmLdmlKeyboardEditor }

procedure TfrmLdmlKeyboardEditor.FormCreate(Sender: TObject);
begin
  inherited;
  SetupDebugForm;
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
    FDebugForm.UIStatus := duiReadyForInput;

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

end.
