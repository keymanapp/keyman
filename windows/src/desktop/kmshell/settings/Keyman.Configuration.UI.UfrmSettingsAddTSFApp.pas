unit Keyman.Configuration.UI.UfrmSettingsAddTSFApp;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Grids,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows;

type
  TfrmSettingsAddTSFApp = class(TForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    Label1: TLabel;
    lblFilename: TLabel;
    editFilename: TEdit;
    cbValue: TComboBox;
    Label2: TLabel;
    procedure cmdOKClick(Sender: TObject);
  private
    function GetFilename: string;
    function GetValueInt: Integer;
    { Private declarations }
  public
    { Public declarations }
    property Filename: string read GetFilename;
    property ValueInt: Integer read GetValueInt;
  end;

implementation

{$R *.dfm}

{ TfrmSettingsAddTSFApp }

procedure TfrmSettingsAddTSFApp.cmdOKClick(Sender: TObject);
var
  filename: string;
begin
  filename := editFilename.Text;
  filename := filename.Trim.ToLower;
  if not filename.EndsWith('.exe') or filename.Contains('\') then
  begin
    ShowMessage('The application '+filename+' is not valid. It must be an executable file ending in .exe and must not include a path');
    Exit;
  end;

  if cbValue.ItemIndex < 0 then
  begin
    ShowMessage('You must select an entry in the Value list');
    Exit;
  end;

  ModalResult := mrOk;
end;

function TfrmSettingsAddTSFApp.GetFilename: string;
begin
  Result := editFilename.Text;
end;

function TfrmSettingsAddTSFApp.GetValueInt: Integer;
begin
  Result := cbValue.ItemIndex;
end;

end.
