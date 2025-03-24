(*
 * Keyman is copyright (C) SIL Global. MIT License.
 *)
unit UfrmOpenCRMRecord;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls;

type
  TfrmOpenCRMRecord = class(TForm)
    TntLabel1: TLabel;
    editSearchFor: TEdit;
    cmdOK: TButton;
    cmdCancel: TButton;
    lblRepo: TLabel;
    cbRepository: TComboBox;
    procedure cbRepositoryClick(Sender: TObject);
    procedure editSearchForChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Changing: Boolean;
    function GetSearchText: WideString;
    procedure SetSearchText(const Value: WideString);
    { Private declarations }
  public
    { Public declarations }
    property SearchText: WideString read GetSearchText write SetSearchText;
  end;

function RepoFullNameToShortName(name: string): string;
function RepoShortNameToFullName(name: string): string;

implementation

{$R *.dfm}

const repos: TArray<TArray<string>> = [
  ['api.keyman.com', 'api'],
  ['downloads.keyman.com', 'downloads'],
  ['help.keyman.com', 'help'],
  ['keyboards', 'keyboards'],
  ['keyman', ''],
  ['keyman.com', 'keyman.com'],
  ['keymankeyboards.com', 'keymankeyboards'],
  ['keymanweb.com', 'keymanweb'],
  ['lexical-models', 'lexical-models'],
  ['r.keymanweb.com', 'r'],
  ['s.keyman.com', 's'],
  ['shared-sites', 'shared'],
  ['status.keyman.com', 'status'],
  ['website-local-proxy', 'proxy']
];

function RepoShortNameToFullName(name: string): string;
var
  i: Integer;
begin
  for i := 0 to High(repos) do
    if repos[i][1] = name then
      Exit(repos[i][0]);
  Result := name;
end;

function RepoFullNameToShortName(name: string): string;
var
  i: Integer;
begin
  for i := 0 to High(repos) do
    if repos[i][0] = name then
      Exit(repos[i][1]);
  Result := name;
end;

procedure TfrmOpenCRMRecord.editSearchForChange(Sender: TObject);
var
  s: string;
  parts: TArray<String>;
  repo: string;
begin
  if Changing then Exit;

  Changing := True;

  s := editSearchFor.Text;
  parts := s.Split(['#']);
  if Length(parts) = 1 then
  begin
    cbRepository.ItemIndex := cbRepository.Items.IndexOf('keyman');
  end
  else
  begin
    repo := RepoShortNameToFullName(parts[0]);
    cbRepository.ItemIndex := cbRepository.Items.IndexOf(repo);
  end;

  Changing := False;
end;

procedure TfrmOpenCRMRecord.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(repos) do
    cbRepository.Items.Add(repos[i][0]);
  cbRepository.ItemIndex := cbRepository.Items.IndexOf('keyman');
end;

function TfrmOpenCRMRecord.GetSearchText: WideString;
begin
  Result := editSearchFor.Text;
end;

procedure TfrmOpenCRMRecord.cbRepositoryClick(Sender: TObject);
var
  repo, s: string;
  parts: TArray<string>;
begin
  if Changing then Exit;
  Changing := True;

  repo := RepoFullNameToShortName(cbRepository.Text);

  s := editSearchFor.Text;
  parts := s.Split(['#']);
  if Length(parts) = 1 then
  begin
    editSearchFor.Text := repo + '#' + parts[0];
  end
  else if Length(parts) >= 2 then
  begin
    editSearchFor.Text := repo + '#' + parts[1];
  end;

  editSearchFor.SetFocus;
  editSearchFor.SelStart := Length(editSearchFor.Text);
  editSearchFor.SelLength := 0;
  Changing := False;
end;

procedure TfrmOpenCRMRecord.SetSearchText(const Value: WideString);
begin
  editSearchFor.Text := Value;
  editSearchFor.SelectAll;
end;

end.
