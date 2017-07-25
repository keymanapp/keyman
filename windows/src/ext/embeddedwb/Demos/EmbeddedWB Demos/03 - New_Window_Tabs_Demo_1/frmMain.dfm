object Form1: TForm1
  Left = 289
  Top = 201
  Caption = 'TEmbeddedWB - Open In A New Window Demo (Tabs)'
  ClientHeight = 418
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 563
    Height = 358
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = PageControl1Change
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 563
    Height = 41
    Align = alTop
    TabOrder = 1
    DesignSize = (
      563
      41)
    object Button1: TButton
      Left = 357
      Top = 11
      Width = 52
      Height = 22
      Anchors = [akTop]
      Caption = 'Go'
      TabOrder = 0
      OnClick = Button1Click
    end
    object IEAddress1: TIEAddress
      Left = 0
      Top = 11
      Width = 348
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      Anchors = [akLeft, akTop, akRight]
      ButtonColor = clBlack
      ButtonPressedColor = clBtnShadow
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      TabOrder = 1
      Text = 'http://www.bsalsa.com/'
    end
    object cbNewTab: TCheckBox
      Left = 429
      Top = 14
      Width = 121
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Open In A New Tab'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 399
    Width = 563
    Height = 19
    Panels = <>
  end
end
