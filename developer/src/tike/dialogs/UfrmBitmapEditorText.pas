(*
  Name:             UfrmBitmapEditorText
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    24 Jul 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    24 Jul 2015 - mcdurdin - I4796 - Refresh Keyman Developer look and feel for release
*)
unit UfrmBitmapEditorText;  // I3306   // I4796

interface

uses
  System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ClearTypeDrawCharacter, UfrmTike, mbColorPreview;

type
  TBitmapEditorTextDrawPreviewEvent = procedure(ADisplayQuality: TClearTypeDisplayQuality; AInsertFont: TFont; AInsertText: WideString) of object;

  TfrmBitmapEditorText = class(TTIKEForm)
    editText: TEdit;
    lblText: TLabel;
    cmdOK: TButton;
    cmdCancel: TButton;
    dlgFont: TFontDialog;
    cbDisplayQuality: TComboBox;
    lblFont: TLabel;
    editFont: TEdit;
    lblQuality: TLabel;
    cmdFont: TButton;
    cpTextColor: TmbColorPreview;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbDisplayQualityClick(Sender: TObject);
    procedure editTextChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmdFontClick(Sender: TObject);
    procedure dlgFontApply(Sender: TObject; Wnd: HWND);
  private
    FInsertFont: TFont;
    FOnDrawPreview: TBitmapEditorTextDrawPreviewEvent;
    FDisplayQuality: TClearTypeDisplayQuality;
    FExcludeDisplayRect: TRect;
    procedure EnableControls;
    function GetInsertText: WideString;
    procedure SetInsertText(Value: WideString);
    procedure SetInsertFont(const Value: TFont);
    procedure DrawPreview;
    procedure SetDisplayQuality(const Value: TClearTypeDisplayQuality);
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    property DisplayQuality: TClearTypeDisplayQuality read FDisplayQuality write SetDisplayQuality;
    property InsertText: WideString read GetInsertText write SetInsertText;
    property InsertFont: TFont read FInsertFont write SetInsertFont;
    property ExcludeDisplayRect: TRect read FExcludeDisplayRect write FExcludeDisplayRect;
    property OnDrawPreview: TBitmapEditorTextDrawPreviewEvent read FOnDrawPreview write FOnDrawPreview;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  UFixFontDialogBold,
  UframeBitmapEditor;

{$R *.dfm}

{ TfrmBitmapEditorText }

procedure TfrmBitmapEditorText.FormCreate(Sender: TObject);
begin
  inherited;
  FInsertFont := TFont.Create;
end;

procedure TfrmBitmapEditorText.FormDestroy(Sender: TObject);
begin
  FInsertFont.Free;
end;

procedure TfrmBitmapEditorText.FormShow(Sender: TObject);
var
  X, Y: Integer;
  r: TRect;
begin
  X := (Screen.Width - Width) div 2;
  Y := (Screen.Height - Height) div 2;

  r := Rect(X, Y, X+Width, Y+Height);

  if IntersectRect(r, FExcludeDisplayRect, r) then
  begin
    r := FExcludeDisplayRect;
    OffsetRect(r, -4, -4);
    Inc(r.Right, 8);
    Inc(r.Bottom, 8);
    if r.Bottom + Height > Screen.Height then
      if r.Top - Height < 0 then
        if r.Right + Width > Screen.Width then
          if r.Left - Width < 0 then
            // Don't move the dialog from center, because it will always overlap the editor
          else
            X := r.Left - Width
        else
          X := r.Right
      else
        Y := r.Top - Height
    else
      Y := r.Bottom;
  end;

  SetBounds(X, Y, Width, Height);

  DrawPreview;
end;

function TfrmBitmapEditorText.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_BitmapEditorText;
end;

function TfrmBitmapEditorText.GetInsertText: WideString;
begin
  Result := editText.Text;
end;

procedure TfrmBitmapEditorText.cbDisplayQualityClick(Sender: TObject);
begin
  FDisplayQuality := TClearTypeDisplayQuality(cbDisplayQuality.ItemIndex);
  DrawPreview;
end;

procedure TfrmBitmapEditorText.DrawPreview;
begin
  if Assigned(FOnDrawPreview) then
    FOnDrawPreview(DisplayQuality, InsertFont, InsertText);
end;

procedure TfrmBitmapEditorText.editTextChange(Sender: TObject);
begin
  DrawPreview;
  EnableControls;
end;

procedure TfrmBitmapEditorText.EnableControls;
begin
  cmdOK.Enabled := InsertText <> '';
end;

procedure TfrmBitmapEditorText.SetDisplayQuality(const Value: TClearTypeDisplayQuality);
begin
  FDisplayQuality := Value;
  cbDisplayQuality.ItemIndex := Ord(FDisplayQuality);
  DrawPreview;
end;

procedure TfrmBitmapEditorText.SetInsertFont(const Value: TFont);
    function FontDetailsToString(Font: TFont): WideString;
    begin
      Result := Font.Name + ', ' + IntToStr(Abs(Font.Size)) + 'pt';
      if fsBold in Font.Style then
        Result := 'Bold '+Result;
      if fsItalic in Font.Style then
        Result := 'Italic '+Result;
      if fsUnderline in Font.Style then
        Result := 'Underline '+Result;
      if fsStrikeOut in Font.Style then
        Result := 'Strikethrough '+Result;
    end;
begin
  FInsertFont.Assign(Value);
  editText.Font := FInsertFont;
  editFont.Text := FontDetailsToString(Value);
  if FInsertFont.Color = TframeBitmapEditor.TransparentReplacementColour
    then cpTextColor.Color := clNone
    else cpTextColor.Color := FInsertFont.Color;
  DrawPreview;
end;

procedure TfrmBitmapEditorText.SetInsertText(Value: WideString);
begin
  editText.Text := Value;
  DrawPreview;
end;

procedure TfrmBitmapEditorText.cmdFontClick(Sender: TObject);
begin
  dlgFont.Font := FInsertFont;
  if dlgFont.Execute then
    InsertFont := FixFontDialogBold(dlgFont.Font);
end;

procedure TfrmBitmapEditorText.dlgFontApply(Sender: TObject; Wnd: HWND);
begin
  InsertFont := FixFontDialogBold(dlgFont.Font);
end;

end.
