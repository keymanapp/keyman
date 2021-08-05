object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 292
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 503
    Height = 13
    Caption = 
      'Test TShellUserRegistry, read HKCU\Software\Tavultesoft\Test [Te' +
      'st], either App hive or Shell User hive'
  end
  object Button2: TButton
    Left = 224
    Top = 148
    Width = 75
    Height = 25
    Caption = 'AppCU'
    TabOrder = 0
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 236
    Top = 208
    Width = 75
    Height = 25
    Caption = 'ShellUserCU'
    TabOrder = 1
    OnClick = Button3Click
  end
end
