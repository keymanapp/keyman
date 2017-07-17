object Form1: TForm1
  Left = 2088
  Top = 345
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'String Security Descriptor by Dezipaitor'
  ClientHeight = 294
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Set ACL'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo_SD: TMemo
    Left = 8
    Top = 40
    Width = 369
    Height = 241
    Lines.Strings = (
      'O:BAG:BAD:(A;;0x3;;;IU)(A;;0x3;;;SY)')
    TabOrder = 1
  end
  object ComboBoxMapping: TComboBox
    Left = 88
    Top = 8
    Width = 281
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
end
