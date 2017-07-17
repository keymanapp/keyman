object Form1: TForm1
  Left = 269
  Top = 107
  Width = 607
  Height = 428
  Caption = 'IE Downloag Mgr '
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 65
    Width = 591
    Height = 327
    TabStop = False
    Align = alClient
    TabOrder = 0
    Silent = False
    OnDownloadComplete = EmbeddedWB1DownloadComplete
    OnBeforeNavigate2 = EmbeddedWB1BeforeNavigate2
    OnFileDownload = EmbeddedWB1FileDownload
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    PrintOptions.Margins.Left = 19.05
    PrintOptions.Margins.Right = 19.05
    PrintOptions.Margins.Top = 19.05
    PrintOptions.Margins.Bottom = 19.05
    PrintOptions.Header = '&w&bPage &p of &P'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    UserAgent = 'EmbeddedWB 14.55 from: http://www.bsalsa.com/'
    ControlData = {
      4C0000005E330000901A00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 591
    Height = 65
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 421
      Top = 9
      Width = 33
      Height = 22
      Caption = 'Go'
      TabOrder = 0
      OnClick = Button1Click
    end
    object IEAddress1: TIEAddress
      Left = 0
      Top = 9
      Width = 417
      Height = 22
      AutoNavigateOnLoad = False
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      EmbeddedWB = EmbeddedWB1
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      TabOrder = 1
      TextOnLoad = tlUserDefine
      TextOnShow = 'http://bsalsa.com/downloads.html'
    end
    object rgMethod: TRadioGroup
      Left = 464
      Top = 0
      Width = 127
      Height = 59
      Caption = 'FileDownload Method'
      Items.Strings = (
        'Go (Url)'
        'Download (Moniker)')
      TabOrder = 2
    end
  end
end
