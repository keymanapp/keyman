object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 348
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object editTDSFileName: TEdit
    Left = 12
    Top = 20
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object cmdBrowseTDS: TButton
    Left = 155
    Top = 18
    Width = 75
    Height = 25
    Caption = '&Browse...'
    TabOrder = 1
    OnClick = cmdBrowseTDSClick
  end
  object cmdAnalyze: TButton
    Left = 284
    Top = 20
    Width = 75
    Height = 25
    Caption = 'cmdAnalyze'
    TabOrder = 2
  end
  object memoData: TMemo
    Left = 4
    Top = 49
    Width = 637
    Height = 296
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Lucida Console'
    Font.Style = []
    Lines.Strings = (
      'memoData')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object dlgOpen: TOpenDialog
    Title = 'Open TDS file'
    Left = 240
    Top = 16
  end
end
