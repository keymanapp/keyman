inherited frmSelectKey: TfrmSelectKey
  Left = 300
  Top = 258
  BorderIcons = [biHelp]
  BorderStyle = bsDialog
  Caption = 'Select Key'
  ClientHeight = 75
  ClientWidth = 237
  Font.Name = 'MS Sans Serif'
  KeyPreview = True
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  ExplicitWidth = 243
  ExplicitHeight = 104
  PixelsPerInch = 96
  TextHeight = 13
  object lblTitle: TLabel
    Left = 16
    Top = 16
    Width = 204
    Height = 13
    Caption = 'Press any letter, number or punctuation key'
  end
  object cmdCancel: TButton
    Left = 84
    Top = 44
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 0
  end
end
