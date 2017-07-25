object Form1: TForm1
  Left = 332
  Top = 255
  Width = 768
  Height = 558
  Caption = 'TEmbeddedWB - Print Preview From Template Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 41
    Width = 760
    Height = 483
    Align = alClient
    TabOrder = 0
    Silent = False
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    OnShowDialog = EmbeddedWB1ShowDialog
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
      4C0000001B470000DC2900000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 760
    Height = 41
    Align = alTop
    TabOrder = 1
    DesignSize = (
      760
      41)
    object SpeedButton1: TSpeedButton
      Left = 0
      Top = 0
      Width = 217
      Height = 41
      Caption = 'Print preview from template!'
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 653
      Top = 9
      Width = 73
      Height = 22
      Anchors = [akTop]
      Caption = 'GO'
      OnClick = SpeedButton2Click
    end
    object edURL: TIEAddress
      Left = 224
      Top = 10
      Width = 417
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      Anchors = [akLeft, akTop, akRight]
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      ParentShowHint = True
      TabOrder = 0
      Text = 'http://www.bsalsa.com'
    end
  end
end
