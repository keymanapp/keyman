object Form2: TForm2
  Left = 373
  Top = 201
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'IEDownload Run-Time demo'
  ClientHeight = 264
  ClientWidth = 511
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 511
    Height = 57
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 9
      Width = 39
      Height = 13
      Caption = 'Address'
    end
    object Button1: TButton
      Left = 445
      Top = 26
      Width = 59
      Height = 25
      Caption = 'Go'
      TabOrder = 0
      OnClick = Button1Click
    end
    object edtAddress: TEdit
      Left = 8
      Top = 28
      Width = 421
      Height = 21
      TabOrder = 1
      Text = 'http://www.bsalsa.com/Downloads/2.exe'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 208
    Width = 511
    Height = 56
    Align = alBottom
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 13
      Width = 3
      Height = 13
    end
    object ProgressBar1: TProgressBar
      Left = 1
      Top = 38
      Width = 509
      Height = 17
      Align = alBottom
      Min = 0
      Max = 100
      TabOrder = 0
    end
  end
  object memDL: TMemo
    Left = 0
    Top = 57
    Width = 511
    Height = 151
    Align = alClient
    Lines.Strings = (
      '')
    ScrollBars = ssBoth
    TabOrder = 2
  end
end
