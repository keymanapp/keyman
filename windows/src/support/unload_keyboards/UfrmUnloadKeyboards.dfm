object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 566
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    651
    566)
  PixelsPerInch = 96
  TextHeight = 13
  object grid: TStringGrid
    Left = 8
    Top = 8
    Width = 631
    Height = 313
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 8
    DefaultRowHeight = 16
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 0
    OnClick = gridClick
  end
  object cmdUnload: TButton
    Left = 89
    Top = 530
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Unload'
    TabOrder = 1
    OnClick = cmdUnloadClick
  end
  object cmdExit: TButton
    Left = 564
    Top = 530
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'E&xit'
    TabOrder = 2
    OnClick = cmdExitClick
  end
  object cmdRefresh: TButton
    Left = 8
    Top = 530
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Refresh'
    TabOrder = 3
    OnClick = cmdRefreshClick
  end
  object cmdLoadKeyboardLayout: TButton
    Left = 184
    Top = 530
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Load 0409'
    TabOrder = 4
    OnClick = cmdLoadKeyboardLayoutClick
  end
  object Button1: TButton
    Left = 265
    Top = 530
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Show &Default'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 346
    Top = 530
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Button2'
    TabOrder = 6
    OnClick = Button2Click
  end
  object memoDetail: TMemo
    Left = 8
    Top = 327
    Width = 631
    Height = 197
    Anchors = [akLeft, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object Button3: TButton
    Left = 427
    Top = 530
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'EnumCategories'
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 483
    Top = 533
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'EnumTSF'
    TabOrder = 9
    OnClick = Button3Click
  end
end
