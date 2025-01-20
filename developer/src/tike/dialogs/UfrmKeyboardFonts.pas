(*
  Name:             UfrmKeyboardFonts
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      3 Aug 2015

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          03 Aug 2015 - mcdurdin - I4820 - Keyboard fonts dialog box has incorrect control types
                    24 Aug 2015 - mcdurdin - I4872 - OSK font and Touch Layout font should be the same in Developer
*)
unit UfrmKeyboardFonts;   // I4820   // I4872

interface

uses
  System.UITypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmTike, Vcl.StdCtrls,
  KeyboardFonts, scFontCombobox;

type
  TKeyboardFontControl = record
    CaptionLabel: TLabel;
    NameCombo: TSCFontComboBox;
    SizeEdit: TEdit;
  end;

  TfrmKeyboardFonts = class(TTikeForm)
    lblDevEnv: TLabel;
    fcbCode: TscFontComboBox;
    editCodeSize: TEdit;
    lblCodeFont: TLabel;
    lblDevEnvName: TLabel;
    lblDevEnvSize: TLabel;
    fcbChar: TscFontComboBox;
    editCharSize: TEdit;
    lblCharFont: TLabel;
    fcbOSK: TscFontComboBox;
    editOSKSize: TEdit;
    lblOSKFont: TLabel;
    editTouchLayoutPhoneSize: TEdit;
    lblTouchLayoutPhoneFont: TLabel;
    editTouchLayoutTabletSize: TEdit;
    lblTouchLayoutTabletFont: TLabel;
    editTouchLayoutDesktopSize: TEdit;
    lblTouchLayoutDesktopFont: TLabel;
    cmdOK: TButton;
    cmdCancel: TButton;
    lblProp: TLabel;
    lblPropName: TLabel;
    lblPropSize: TLabel;
    lblOSKFontSize: TLabel;
    lblTextEditorFontSize: TLabel;
    lblDebuggerFontSize: TLabel;
    lblTouchLayoutPhoneFontSize: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    fcbTouchLayoutPhone: TscFontComboBox;
    fcbTouchLayoutTablet: TscFontComboBox;
    fcbTouchLayoutDesktop: TscFontComboBox;
    lblDisplayMap: TLabel;
    editDisplayMapSize: TEdit;
    fcbDisplayMap: TscFontComboBox;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
  private
    FControls: array[TKeyboardFont] of TKeyboardFontControl;
    function GetFontInfo(Index: TKeyboardFont): TKeyboardFontInfo;
    procedure SetFontInfo(Index: TKeyboardFont; const Value: TKeyboardFontInfo);
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    property FontInfo[Index: TKeyboardFont]: TKeyboardFontInfo read GetFontInfo write SetFontInfo;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics;

{$R *.dfm}

procedure TfrmKeyboardFonts.cmdOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmKeyboardFonts.FormCreate(Sender: TObject);
begin
  inherited;

  // Note: filename and path not currently used

  FControls[kfontCode].CaptionLabel := lblCodeFont;
  FControls[kfontCode].NameCombo    := fcbCode;
  FControls[kfontCode].SizeEdit     := editCodeSize;

  FControls[kfontChar].CaptionLabel := lblCharFont;
  FControls[kfontChar].NameCombo    := fcbChar;
  FControls[kfontChar].SizeEdit     := editCharSize;

  FControls[kfontOSK].CaptionLabel := lblOSKFont;
  FControls[kfontOSK].NameCombo    := fcbOSK;
  FControls[kfontOSK].SizeEdit     := editOSKSize;

  FControls[kfontTouchLayoutPhone].CaptionLabel := lblTouchLayoutPhoneFont;
  FControls[kfontTouchLayoutPhone].NameCombo    := fcbTouchLayoutPhone;
  FControls[kfontTouchLayoutPhone].SizeEdit     := editTouchLayoutPhoneSize;

  FControls[kfontTouchLayoutTablet].CaptionLabel := lblTouchLayoutTabletFont;
  FControls[kfontTouchLayoutTablet].NameCombo    := fcbTouchLayoutTablet;
  FControls[kfontTouchLayoutTablet].SizeEdit     := editTouchLayoutTabletSize;

  FControls[kfontTouchLayoutDesktop].CaptionLabel := lblTouchLayoutDesktopFont;
  FControls[kfontTouchLayoutDesktop].NameCombo    := fcbTouchLayoutDesktop;
  FControls[kfontTouchLayoutDesktop].SizeEdit     := editTouchLayoutDesktopSize;

  FControls[kfontDisplayMap].CaptionLabel := lblDisplayMap;
  FControls[kfontDisplayMap].NameCombo    := fcbDisplayMap;
  FControls[kfontDisplayMap].SizeEdit     := editDisplayMapSize;
end;

function TfrmKeyboardFonts.GetFontInfo(Index: TKeyboardFont): TKeyboardFontInfo;
begin
  Result.Name := FControls[Index].NameCombo.FontName;   // I4820
  Result.Size := FControls[Index].SizeEdit.Text;
  Result.Enabled := FControls[Index].NameCombo.Enabled;
  //Result.Filename := IncludeTrailingPathDelimiter(FControls[Index].PathEdit.Text) + FControls[Index].FilenameEdit.Text;
end;

function TfrmKeyboardFonts.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_KeyboardFonts;
end;

procedure TfrmKeyboardFonts.SetFontInfo(Index: TKeyboardFont; const Value: TKeyboardFontInfo);
begin
  FControls[Index].NameCombo.Enabled := Value.Enabled;
  FControls[Index].SizeEdit.Enabled := Value.Enabled;
  FControls[Index].CaptionLabel.Enabled := Value.Enabled;

  if Value.Enabled then
  begin
    FControls[Index].NameCombo.FontName := Value.Name;   // I4820
    FControls[Index].SizeEdit.Text := Value.Size;
  end;
end;

end.
