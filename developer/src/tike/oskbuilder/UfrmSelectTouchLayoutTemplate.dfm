object frmSelectTouchLayoutTemplate: TfrmSelectTouchLayoutTemplate
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Touch Layout Template'
  ClientHeight = 242
  ClientWidth = 226
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object cmdOk: TButton
    Left = 35
    Top = 200
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = cmdOkClick
  end
  object lbTemplate: TListBox
    Left = 8
    Top = 8
    Width = 209
    Height = 170
    ItemHeight = 13
    TabOrder = 0
  end
  object cmdCancel: TButton
    Left = 116
    Top = 200
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
