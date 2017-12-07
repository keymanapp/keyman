{$A1,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q+,R+,S-,T-,U-,V+,W+,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM ON}
{$WARN SYMBOL_EXPERIMENTAL ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM ON}
{$WARN UNIT_DEPRECATED ON}
{$WARN UNIT_EXPERIMENTAL ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN OPTION_TRUNCATED ON}
{$WARN WIDECHAR_REDUCED ON}
{$WARN DUPLICATES_IGNORED ON}
{$WARN UNIT_INIT_SEQ ON}
{$WARN LOCAL_PINVOKE ON}
{$WARN MESSAGE_DIRECTIVE ON}
{$WARN TYPEINFO_IMPLICITLY_ADDED ON}
{$WARN RLINK_WARNING ON}
{$WARN IMPLICIT_STRING_CAST ON}
{$WARN IMPLICIT_STRING_CAST_LOSS ON}
{$WARN EXPLICIT_STRING_CAST OFF}
{$WARN EXPLICIT_STRING_CAST_LOSS OFF}
{$WARN CVT_WCHAR_TO_ACHAR ON}
{$WARN CVT_NARROWING_STRING_LOST ON}
{$WARN CVT_ACHAR_TO_WCHAR ON}
{$WARN CVT_WIDENING_STRING_LOST ON}
{$WARN NON_PORTABLE_TYPECAST ON}
{$WARN XML_WHITESPACE_NOT_ALLOWED ON}
{$WARN XML_UNKNOWN_ENTITY ON}
{$WARN XML_INVALID_NAME_START ON}
{$WARN XML_INVALID_NAME ON}
{$WARN XML_EXPECTED_CHARACTER ON}
{$WARN XML_CREF_NO_RESOLVE ON}
{$WARN XML_NO_PARM ON}
{$WARN XML_NO_MATCHING_PARM ON}
{$WARN IMMUTABLE_STRINGS OFF}
(*
  Name:             UfrmSelectISOLanguages
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      16 Jun 2008

  Modified Date:    16 Jun 2008
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          16 Jun 2008 - mcdurdin - I1400 - Initial version
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
