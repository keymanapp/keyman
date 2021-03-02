unit Keyman.Developer.UI.UfrmWordlistEditor;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,

  Keyman.Developer.UI.UframeWordlistEditor,
  UfrmMDIEditor;

type
  TfrmWordlistEditor = class(TfrmTikeEditor)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    frame: TframeWordlistEditor;
    procedure FrameModified(Sender: TObject);
  protected
    function GetHelpTopic: string; override;
    function DoOpenFile: Boolean; override;
    function DoSaveFile: Boolean; override;
    function GetFileNameFilter: string; override;
    function GetDefaultExt: string; override;

  public
    procedure FindError(const Filename: string; s: string; line: Integer); override;   // I4081
  end;

implementation

{$R *.dfm}

uses
  Keyman.Developer.System.HelpTopics,
  TextFileFormat,
  UfrmMain,
  UfrmMessages;

{ TfrmWordlistEditor }

procedure TfrmWordlistEditor.FormCreate(Sender: TObject);
begin
  inherited;
  frame := TframeWordlistEditor.Create(Self);
  frame.Align := alClient;
  frame.Parent := Self;
  frame.OnChanged := FrameModified;
end;

procedure TfrmWordlistEditor.FormDestroy(Sender: TObject);
begin
  inherited;
  frame.Free;
end;

procedure TfrmWordlistEditor.FrameModified(Sender: TObject);
begin
  Modified := frame.Modified;
end;

function TfrmWordlistEditor.DoOpenFile: Boolean;
begin
  Result := frame.LoadFromFile(Filename);
end;

function TfrmWordlistEditor.DoSaveFile: Boolean;
begin
  Result := frame.SaveToFile(Filename);
end;

procedure TfrmWordlistEditor.FindError(const Filename: string; s: string; line: Integer);
begin
  frame.FindError(Filename, s, line);
end;

function TfrmWordlistEditor.GetDefaultExt: string;
begin
  Result := 'tsv';
end;

function TfrmWordlistEditor.GetFileNameFilter: string;
begin
  Result := 'Wordlist files (*.tsv)|*.tsv|All files (*.*)|*.*';
end;

function TfrmWordlistEditor.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_WordlistEditor;
end;

end.
