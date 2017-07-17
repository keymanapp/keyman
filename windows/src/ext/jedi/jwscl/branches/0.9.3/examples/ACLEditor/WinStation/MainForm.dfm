object Form1: TForm1
  Left = 659
  Top = 253
  Width = 355
  Height = 313
  Caption = 'Winstation ACL Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    347
    279)
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 16
    Top = 40
    Width = 187
    Height = 102
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnChange = TreeView1Change
    OnExpanding = TreeView1Expanding
  end
  object Button1: TButton
    Left = 216
    Top = 112
    Width = 121
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Close'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 216
    Top = 48
    Width = 121
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Show ACL'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 216
    Top = 80
    Width = 121
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Show DACL (Text)'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 149
    Width = 187
    Height = 104
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
    WantReturns = False
  end
  object ComboBoxSession: TComboBox
    Left = 16
    Top = 16
    Width = 323
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 5
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 260
    Width = 347
    Height = 19
    Panels = <>
  end
end
