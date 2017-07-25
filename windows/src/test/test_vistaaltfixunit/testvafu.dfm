object Form1: TForm1
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 240
  Caption = 'Form1'
  ClientHeight = 284
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 56
    Top = 20
    Width = 75
    Height = 25
    Caption = '&Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 56
    Top = 56
    Width = 75
    Height = 25
    Caption = 'B&utton2'
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 60
    Top = 93
    Width = 97
    Height = 17
    Caption = '&CheckBox1'
    TabOrder = 2
  end
  object CheckBox2: TCheckBox
    Left = 60
    Top = 116
    Width = 97
    Height = 17
    Caption = 'C&heckBox2'
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 16
    Top = 172
    Width = 185
    Height = 101
    Caption = 'Panel1'
    TabOrder = 4
    object Button3: TButton
      Left = 44
      Top = 35
      Width = 75
      Height = 25
      Caption = 'Bu&tton3'
      TabOrder = 0
    end
    object CheckBox3: TCheckBox
      Left = 44
      Top = 12
      Width = 97
      Height = 17
      Caption = 'Ch&eckBox3'
      TabOrder = 1
    end
  end
  object XPManifest1: TXPManifest
    Left = 204
    Top = 148
  end
end
