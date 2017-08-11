object ActiveFormX: TActiveFormX
  Left = 288
  Top = 269
  Width = 494
  Height = 346
  Caption = 'ActiveFormX'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = ActiveFormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 41
    Width = 486
    Height = 271
    Align = alClient
    TabOrder = 0
    Silent = False
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
    ControlData = {
      4C0000003B320000021C00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 486
    Height = 41
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 392
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Go'
      TabOrder = 0
      OnClick = Button1Click
    end
    object IEAddress1: TIEAddress
      Left = 8
      Top = 8
      Width = 377
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      TabOrder = 1
    end
  end
end
