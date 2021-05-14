object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 243
  ClientWidth = 472
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
  object Label1: TLabel
    Left = 232
    Top = 136
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Get Subst'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Register Profile'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Unreg'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 136
    Top = 131
    Width = 75
    Height = 25
    Caption = 'Get Profile'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 136
    Top = 162
    Width = 75
    Height = 25
    Caption = 'Activate'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Memo1: TMemo
    Left = 232
    Top = 41
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 5
  end
  object Button6: TButton
    Left = 8
    Top = 210
    Width = 75
    Height = 25
    Caption = 'Button6'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 96
    Top = 210
    Width = 75
    Height = 25
    Caption = 'Button7'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 232
    Top = 168
  end
end
