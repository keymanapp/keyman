object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 247
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 480
    Height = 206
    Align = alClient
    TabOrder = 0
    ExplicitTop = 0
    ExplicitHeight = 247
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 41
    Align = alTop
    TabOrder = 1
    ExplicitLeft = 160
    ExplicitTop = 120
    ExplicitWidth = 185
    object chkShiftDelay: TCheckBox
      Left = 8
      Top = 13
      Width = 97
      Height = 17
      Caption = 'Shift Delay Mode'
      TabOrder = 0
    end
  end
end
