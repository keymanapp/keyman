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
    cmdCopyHTML: TButton;
    procedure cbRepositoryClick(Sender: TObject);
    procedure editSearchForChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdCopyHTMLClick(Sender: TObject);
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


type
  TIssueQuery = record
    searchString: string;
    repo: string;
    issueNumber: Integer;
  end;

function SearchTextToQuery(s: string): TIssueQuery;

implementation

uses
  Vcl.Clipbrd;

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

function SearchTextToQuery(s: string): TIssueQuery;
var
  parts: TArray<String>;
begin
  Result.repo := 'keyman';
  Result.issueNumber := 0;
  Result.searchString := '';

  s := s.Trim;
  parts := s.Split(['#']);
  if Length(parts) = 0 then
  begin
    Exit;
  end
  else if Length(parts) = 1 then
  begin
    Result.issueNumber := StrToIntDef(s, 0);
    if IntToStr(Result.issueNumber) <> s then
    begin
      Result.issueNumber := 0;
      Result.searchString := s;
    end;
  end
  else
  begin
    Result.repo := RepoShortNameToFullName(parts[0]);
    Result.issueNumber := StrToIntDef(parts[1], 0);
    if IntToStr(Result.issueNumber) <> parts[1] then
    begin
      Result.issueNumber := 0;
      Result.searchString := parts[1];
    end;
  end;
end;

type
  TMyClipboard = class(TClipboard);

procedure TfrmOpenCRMRecord.cmdCopyHTMLClick(Sender: TObject);
var
  c: TMyClipboard;
  m: TMemoryStream;
  ss: TStream;
  displayRepo, html, s: string;
  iq: TIssueQuery;
  CF_HTML: UINT;
  header: string;
const
  // https://learn.microsoft.com/en-us/windows/win32/dataxchg/html-clipboard-format
  // yeesh what a format
  header_template =
    'Version:0.9'#$D#$A+
    'StartHTML:%0.09d'#$D#$A+
    'EndHTML:%0.09d'#$D#$A+
    'StartFragment:%0.09d'#$D#$A+
    'EndFragment:%0.09d'#$D#$A;
  start_fragment =  '<!--StartFragment -->';
  end_fragment = '<!--EndFragment -->';
  context_start = '<html>'#$D#$A'<body>'#$D#$A;
  context_end = #$D#$A'</body>'#$D#$A'</html>';
begin
  iq := SearchTextToQuery(SearchText);
  if iq.repo = 'keyman' then
    displayRepo := ''
  else
    displayRepo := iq.repo;
  html := Format('<a href="https://github.com/keymanapp/%0:s/issues/%1:d">%2:s#%1:d</a>', [
    iq.repo, iq.issueNumber, displayRepo
  ]);

  // Warning, this will go sadly badly with non-ascii letters
  // because I am lazily not using UTF8Strings at this point

  header := Format(header_template, [0,0,0,0]);
  s := Format(header_template, [
    header.Length,
    header.Length + context_start.Length + start_fragment.Length + html.Length + end_fragment.Length + context_end.Length,
    header.Length + context_start.Length + start_fragment.Length,
    header.Length + context_start.Length + start_fragment.Length + html.Length
  ]) + context_start + start_fragment + html + end_fragment + context_end + #0;

  ss := TStringStream.Create(s, TEncoding.UTF8);
  try
    CF_HTML := RegisterClipboardFormat('HTML Format');
    m := TMemoryStream.Create;
    try
      m.CopyFrom(ss, 0);
      c := TMyClipboard(Clipboard); // access protected members yay delphi
      c.SetBuffer(CF_HTML, m.Memory^, m.Size);
    finally
      m.Free;
    end;
  finally
    ss.Free;
  end;
end;

procedure TfrmOpenCRMRecord.editSearchForChange(Sender: TObject);
var
  iq: TIssueQuery;
begin
  if Changing then Exit;

  Changing := True;

  iq := SearchTextToQuery(SearchText);

  if iq.searchString <> '' then
  begin
    cmdCopyHTML.Enabled := False;
    cmdOK.Caption := '&Search';
  end
  else if iq.issueNumber > 0 then
  begin
    cmdCopyHTML.Enabled := True;
    cmdOK.Caption := '&Open issue';
  end
  else
  begin
    cmdCopyHTML.Enabled := False;
    cmdOK.Caption := '&All issues';
  end;

  cbRepository.ItemIndex := cbRepository.Items.IndexOf(iq.repo);

  Changing := False;
end;

procedure TfrmOpenCRMRecord.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(repos) do
    cbRepository.Items.Add(repos[i][0]);
  cbRepository.ItemIndex := cbRepository.Items.IndexOf('keyman');
  editSearchForChange(nil);
end;

function TfrmOpenCRMRecord.GetSearchText: WideString;
begin
  Result := Trim(editSearchFor.Text);
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
