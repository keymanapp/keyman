object Form1: TForm1
  Left = 276
  Top = 220
  Width = 899
  Height = 698
  Caption = 'TEmbeddedWB - SDI Demo'
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
  object pnlAddressBar: TPanel
    Left = 0
    Top = 0
    Width = 891
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      891
      41)
    object btnGo: TButton
      Left = 833
      Top = 13
      Width = 41
      Height = 20
      Anchors = [akTop, akRight]
      Caption = 'GO'
      TabOrder = 0
      OnClick = btnGoClick
    end
    object IEAddress1: TIEAddress
      Left = 6
      Top = 12
      Width = 817
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      Anchors = [akLeft, akTop, akRight]
      EmbeddedWB = EmbeddedWB1
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      ShowFavicon = True
      TabOrder = 1
      TextOnLoad = tlUserDefine
      TextOnShow = 'http://www.bsalsa.com/support.html'
      Themes = tmXP
      UseAppIcon = True
    end
  end
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 91
    Width = 891
    Height = 554
    Align = alClient
    TabOrder = 1
    OnStatusTextChange = EmbeddedWB1StatusTextChange
    OnNewWindow2 = EmbeddedWB1NewWindow2
    OnNewWindow3 = EmbeddedWB1NewWindow3
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    PrintOptions.Margins.Left = 19.05
    PrintOptions.Margins.Right = 19.05
    PrintOptions.Margins.Top = 19.05
    PrintOptions.Margins.Bottom = 19.05
    PrintOptions.Header = '&w&bSeite &p von &P'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    UserAgent = 'Mozilla/4.0(Compatible-EmbeddedWB 14.56 http://bsalsa.com/ '
    ControlData = {
      4C000000163A0000B12400000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 645
    Width = 891
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 891
    Height = 50
    Align = alTop
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 273
      Height = 13
      Caption = '1. Right-click a link and then click "Open in New Window".'
      Transparent = True
    end
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 838
      Height = 13
      Caption = 
        '2. Result: The link will be opened in a new window within your a' +
        'pplication instead of in a top-level Internet Explorer window th' +
        'at is created as a separate (nonhosted) process.'
    end
  end
end
