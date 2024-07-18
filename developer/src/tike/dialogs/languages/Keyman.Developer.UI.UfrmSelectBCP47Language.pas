(*
  Name:             UfrmSelectBCP47Language
  Copyright:        Copyright (C) SIL International.
  Date:             7 Dec 2017
  Authors:          mcdurdin
*)
unit Keyman.Developer.UI.UfrmSelectBCP47Language;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,

  UfrmTike,
  BCP47Tag, Vcl.ExtCtrls;

type
  TfrmSelectBCP47Language = class(TTikeForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    lblLanguageTag: TLabel;
    lblScriptTag: TLabel;
    lblRegionTag: TLabel;
    cbLanguageTag: TComboBox;
    cbScriptTag: TComboBox;
    cbRegionTag: TComboBox;
    lblBCP47Code: TLabel;
    editBCP47Code: TEdit;
    lblValidateCode: TLabel;
    lblLanguageName: TLabel;
    lblScriptName: TLabel;
    lblRegionName: TLabel;
    lblLinkToW3C: TLinkLabel;
    Label1: TLabel;
    editLanguageName: TEdit;
    cmdResetLanguageName: TButton;
    procedure cbLanguageTagChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbScriptTagChange(Sender: TObject);
    procedure cbRegionTagChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblLinkToW3CLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure cmdResetLanguageNameClick(Sender: TObject);
    procedure editLanguageNameChange(Sender: TObject);
  private
    tag: TBCP47Tag;
    FCustomLanguageName: Boolean;
    procedure EnableControls;
    function GetLanguageID: string;
    function GetLanguageName: string;
    procedure RefreshLanguageName;
    procedure SetLanguageID(const Value: string);
    procedure SetLanguageName(const Value: string);
    function LookupLanguageName: string;
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    property LanguageID: string read GetLanguageID write SetLanguageID;
    property LanguageName: string read GetLanguageName write SetLanguageName;
  end;

implementation

uses
  System.Generics.Collections,

  Keyman.Developer.System.HelpTopics,
  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.KMXFileLanguages,
  Keyman.System.LanguageCodeUtils,
  utilexecute;

{$R *.dfm}

{ TfrmSelectBCP47Language }


procedure TfrmSelectBCP47Language.FormCreate(Sender: TObject);
  procedure PopulateComboBoxFromDict(cb: TComboBox; dict: TDictionary<string, string>);
  var
    i: Integer;
    a: TArray<TPair<string,string>>;
  begin
    cb.Items.BeginUpdate;
    try
      a := dict.ToArray;
      for i := 0 to Length(a) - 1 do
        cb.Items.Add(a[i].Key);
    finally
      cb.Items.EndUpdate;
    end;
  end;
begin
  inherited;
  tag := TBCP47Tag.Create('');
  PopulateComboBoxFromDict(cbLanguageTag, TLanguageCodeUtils.BCP47Languages);
  PopulateComboBoxFromDict(cbScriptTag, TLanguageCodeUtils.BCP47Scripts);
  PopulateComboBoxFromDict(cbRegionTag, TLanguageCodeUtils.BCP47Regions);
  RefreshLanguageName;
  EnableControls;
end;

procedure TfrmSelectBCP47Language.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(tag);
end;

function TfrmSelectBCP47Language.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_SelectBCP47Language;
end;

function TfrmSelectBCP47Language.GetLanguageID: string;
begin
  Result := tag.Tag;
end;

function TfrmSelectBCP47Language.GetLanguageName: string;
begin
  Result := editLanguageName.Text;
end;

procedure TfrmSelectBCP47Language.lblLinkToW3CLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  TUtilExecute.URL(Link);
end;

function TfrmSelectBCP47Language.LookupLanguageName: string;
begin
  Result := TLanguageCodeUtils.LanguageName(lblLanguageName.Caption, lblScriptName.Caption, lblRegionName.Caption);
end;

procedure TfrmSelectBCP47Language.cmdResetLanguageNameClick(Sender: TObject);
begin
  editLanguageName.Text := LookupLanguageName;
  FCustomLanguageName := False;
  EnableControls;
end;

procedure TfrmSelectBCP47Language.editLanguageNameChange(Sender: TObject);
begin
  FCustomLanguageName := True;
  EnableControls;
end;

procedure TfrmSelectBCP47Language.EnableControls;
begin
  cmdResetLanguageName.Enabled := FCustomLanguageName;
end;

procedure TfrmSelectBCP47Language.cbLanguageTagChange(Sender: TObject);
var
  t: string;
begin
  inherited;
  tag.Tag := TKMXFileLanguages.TranslateISO6393ToBCP47(cbLanguageTag.Text);
  t := TCanonicalLanguageCodeUtils.FindBestTag(Tag.Tag, False, False);
  if t <> '' then
  begin
    with TBCP47Tag.Create(t) do
    try
      cbScriptTag.Text := Script;
      Self.tag.Script := Script;
    finally
      Free;
    end;
  end
  else
  begin
    cbScriptTag.Text := '';
    tag.Script := '';
  end;
  FCustomLanguageName := False; // Always reset when entering a language tag.
  RefreshLanguageName;
  EnableControls;
end;

procedure TfrmSelectBCP47Language.cbRegionTagChange(Sender: TObject);
begin
  inherited;
  tag.Region := cbRegionTag.Text;
  RefreshLanguageName;
end;

procedure TfrmSelectBCP47Language.cbScriptTagChange(Sender: TObject);
begin
  inherited;
  tag.Script := cbScriptTag.Text;
  RefreshLanguageName;
end;

procedure TfrmSelectBCP47Language.RefreshLanguageName;
var
  msg: string;

  function GetDictEntry(subtag: string; dict: TDictionary<string,string>): string;
  begin
    if subtag = '' then
      Result := ''
    else if not dict.TryGetValue(subtag, Result) then
      Result := subtag;
  end;


begin
  editBCP47Code.Text := tag.Tag;
  lblLanguageName.Caption := GetDictEntry(tag.Language, TLanguageCodeUtils.BCP47Languages);
  lblScriptName.Caption := GetDictEntry(tag.Script, TLanguageCodeUtils.BCP47Scripts);
  lblRegionName.Caption := GetDictEntry(tag.Region, TLanguageCodeUtils.BCP47Regions);
  if not FCustomLanguageName then
  begin
    editLanguageName.Text := LookupLanguageName;
    FCustomLanguageName := False;
    EnableControls;
  end;
  cmdOK.Enabled := tag.IsValid(True, msg);
  if not cmdOK.Enabled
    then lblValidateCode.Caption := msg
    else lblValidateCode.Caption := 'This is a valid BCP 47 tag';
end;

procedure TfrmSelectBCP47Language.SetLanguageID(const Value: string);
begin
  tag.Tag := Value;
  cbLanguageTag.Text := tag.Language;
  cbRegionTag.Text := tag.Region;
  cbScriptTag.Text := tag.Script;
  RefreshLanguageName;
  FCustomLanguageName := editLanguageName.Text <> LookupLanguageName;
  EnableControls;
end;

procedure TfrmSelectBCP47Language.SetLanguageName(const Value: string);
begin
  editLanguageName.Text := Value;
  FCustomLanguageName := editLanguageName.Text <> LookupLanguageName;
  EnableControls;
end;

end.
