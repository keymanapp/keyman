object frmStockMessageEditorMoveToGroup: TfrmStockMessageEditorMoveToGroup
  Left = 0
  Top = 0
  Caption = 'frmStockMessageEditorMoveToGroup'
  ClientHeight = 273
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbSections: TListBox
    Left = 12
    Top = 12
    Width = 225
    Height = 209
    ItemHeight = 13
    TabOrder = 0
  end
  object cmdOK: TButton
    Left = 40
    Top = 236
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object cmdCancel: TButton
    Left = 128
    Top = 236
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
