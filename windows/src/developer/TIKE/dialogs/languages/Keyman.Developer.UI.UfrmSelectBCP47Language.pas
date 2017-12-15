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
    editLanguageTag: TEdit;
    editScriptTag: TEdit;
    editRegionTag: TEdit;
    lblBCP47Code: TLabel;
    editBCP47Code: TEdit;
    lblValidateCode: TLabel;
    lblLanguageName: TLabel;
    lblScriptName: TLabel;
    lblRegionName: TLabel;
    lblLinkToW3C: TLinkLabel;
    procedure editLanguageTagChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure editScriptTagChange(Sender: TObject);
    procedure editRegionTagChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblLinkToW3CLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    tag: TBCP47Tag;
    function GetLanguageID: string;
    function GetLanguageName: string;
    procedure RefreshPreview;
    { Private declarations }
  public
    { Public declarations }
    property LanguageID: string read GetLanguageID;
    property LanguageName: string read GetLanguageName;
  end;

implementation

uses
  Keyman.System.KMXFileLanguages,
  utilexecute;

{$R *.dfm}

{ TfrmSelectBCP47Language }

procedure TfrmSelectBCP47Language.FormCreate(Sender: TObject);
begin
  inherited;
  tag := TBCP47Tag.Create('');
end;

procedure TfrmSelectBCP47Language.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(tag);
end;

function TfrmSelectBCP47Language.GetLanguageID: string;
begin
  Result := tag.Tag;
end;

function TfrmSelectBCP47Language.GetLanguageName: string;
begin
  // TODO: BCP47: per lookup with BCP-47 feature
  Result := tag.Tag;
end;

procedure TfrmSelectBCP47Language.lblLinkToW3CLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  TUtilExecute.URL(Link);
end;

procedure TfrmSelectBCP47Language.editLanguageTagChange(Sender: TObject);
begin
  inherited;
  // TODO: BCP47: search on language names instead of just taking a tag
  tag.Language := TKMXFileLanguages.TranslateISO6393ToBCP47(editLanguageTag.Text);
  RefreshPreview;
end;

procedure TfrmSelectBCP47Language.editRegionTagChange(Sender: TObject);
begin
  inherited;
  tag.Region := editRegionTag.Text;
  RefreshPreview;
end;

procedure TfrmSelectBCP47Language.editScriptTagChange(Sender: TObject);
begin
  inherited;
  tag.Script := editScriptTag.Text;
  RefreshPreview;
end;

procedure TfrmSelectBCP47Language.RefreshPreview;
var
  msg: string;
begin
  editBCP47Code.Text := tag.Tag;
  lblLanguageName.Caption := tag.Language; // TODO: BCP47: Lookup language name
  lblScriptName.Caption := tag.Script; // TODO: BCP47: Lookup script name
  lblRegionName.Caption := tag.Region; // TODO: BCP47: Lookup region name
  cmdOK.Enabled := tag.IsValid(msg);
  if not cmdOK.Enabled
    then lblValidateCode.Caption := msg
    else lblValidateCode.Caption := 'This is a valid BCP 47 tag';
end;

end.
