object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 24
    Top = 120
    Width = 577
    Height = 172
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 207
    Top = 72
    Width = 75
    Height = 25
    Caption = 'DoSubst'
    TabOrder = 1
    OnClick = DoSubstClick
  end
  object Button2: TButton
    Left = 288
    Top = 72
    Width = 75
    Height = 25
    Caption = 'ClearSubst'
    TabOrder = 2
    OnClick = ClearSubstClick
  end
  object Button3: TButton
    Left = 369
    Top = 72
    Width = 75
    Height = 25
    Caption = 'GetCurrent'
    TabOrder = 3
    OnClick = GetCurrentClick
  end
end
