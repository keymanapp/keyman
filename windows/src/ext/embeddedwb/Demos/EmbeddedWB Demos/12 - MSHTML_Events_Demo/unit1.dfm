object Form1: TForm1
  Left = 262
  Top = 320
  Width = 831
  Height = 596
  Caption = 'TEmbeddedWB & MSHTMLEvents'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 105
    Width = 823
    Height = 457
    Align = alClient
    TabOrder = 0
    OnDownloadComplete = EmbeddedWB1DownloadComplete
    OnBeforeNavigate2 = EmbeddedWB1BeforeNavigate2
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    HTMLCode.Strings = (
      'http://www.bsalsa.com/support.html')
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.Header = '&w&bSeite &p von &P'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    ControlData = {
      4C00000017590000EA3700000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 823
    Height = 105
    Align = alTop
    TabOrder = 1
    DesignSize = (
      823
      105)
    object Label1: TLabel
      Left = 19
      Top = 14
      Width = 95
      Height = 13
      Caption = '> Click on any URL!'
      Transparent = True
    end
    object Memo1: TMemo
      Left = 184
      Top = 8
      Width = 632
      Height = 89
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
end
