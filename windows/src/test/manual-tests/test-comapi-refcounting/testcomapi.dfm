object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 275
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    554
    275)
  PixelsPerInch = 96
  TextHeight = 13
  object cmdStartTest: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Start Test'
    TabOrder = 0
    OnClick = cmdStartTestClick
  end
  object memoLog: TMemo
    Left = 0
    Top = 47
    Width = 557
    Height = 230
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'memoLog')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
end
