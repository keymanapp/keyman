object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 763
  ClientWidth = 1105
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    1105
    763)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TTntMemo
    Left = 8
    Top = 39
    Width = 1089
    Height = 716
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Send Keys'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 89
    Top = 8
    Width = 133
    Height = 25
    Caption = 'Send Random Hotkey'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 228
    Top = 8
    Width = 153
    Height = 25
    Caption = 'Restart Keyman Continually'
    TabOrder = 3
    OnClick = Button3Click
  end
end
