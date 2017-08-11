object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Browser Renderer Settings Correction Tool for Keyman Desktop 8'
  ClientHeight = 253
  ClientWidth = 482
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
    Left = 8
    Top = 8
    Width = 440
    Height = 26
    Caption = 
      'This tool corrects the browser renderer settings for Keyman Desk' +
      'top 8.0 after downgrading from Keyman Desktop 9.0'
    WordWrap = True
  end
  object cmdCorrect: TButton
    Left = 8
    Top = 56
    Width = 209
    Height = 33
    Caption = 'Correct Browser Renderer Settings'
    TabOrder = 0
    OnClick = cmdCorrectClick
  end
end
