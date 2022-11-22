object frmAddKeyboardFeature: TfrmAddKeyboardFeature
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add Feature'
  ClientHeight = 332
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbFeature: TListBox
    Left = 16
    Top = 16
    Width = 337
    Height = 265
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbFeatureClick
    OnDblClick = lbFeatureDblClick
  end
  object cmdOK: TButton
    Left = 107
    Top = 295
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object cmdCancel: TButton
    Left = 188
    Top = 295
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
