object Form1: TForm1
  Left = 330
  Top = 294
  Width = 726
  Height = 482
  Caption = 'TEmbeddedWB - New Window Tabs Demo 2'
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 710
    Height = 38
    Align = alTop
    TabOrder = 0
    object btnGo: TButton
      Left = 380
      Top = 7
      Width = 43
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Go'
      TabOrder = 0
      OnClick = btnGoClick
    end
    object btnBack: TButton
      Left = 430
      Top = 7
      Width = 52
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Back'
      TabOrder = 1
      OnClick = btnBackClick
    end
    object btnForward: TButton
      Left = 489
      Top = 7
      Width = 49
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Forward'
      TabOrder = 2
      OnClick = btnForwardClick
    end
    object IEAddress1: TIEAddress
      Left = 8
      Top = 7
      Width = 361
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      Anchors = [akLeft, akTop, akRight]
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      TabOrder = 3
    end
    object cbOpenNew: TCheckBox
      Left = 616
      Top = 10
      Width = 97
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'OpenNewTab'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object btnStop: TButton
      Left = 545
      Top = 7
      Width = 49
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Stop'
      TabOrder = 5
      OnClick = btnStopClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 38
    Width = 710
    Height = 389
    Align = alClient
    TabOrder = 1
    OnChange = PageControl1Change
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 427
    Width = 710
    Height = 19
    Panels = <>
    SimplePanel = False
  end
end
