object Form2: TForm2
  Left = 304
  Top = 235
  Width = 435
  Height = 363
  Caption = 'Form2'
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
    Left = 16
    Top = 24
    Width = 281
    Height = 233
    TabOrder = 0
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    HTMLCode.Strings = (
      'http://bsalsa.com/test/FlashTest.htm')
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
      4C000000F7260000B21E00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Memo1: TMemo
    Left = 312
    Top = 24
    Width = 97
    Height = 233
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button1: TButton
    Left = 176
    Top = 288
    Width = 75
    Height = 25
    Caption = 'close'
    TabOrder = 2
    OnClick = Button1Click
  end
end
