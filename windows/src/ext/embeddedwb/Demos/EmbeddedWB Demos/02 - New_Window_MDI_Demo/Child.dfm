object ChildFrm: TChildFrm
  Left = 281
  Top = 203
  Width = 417
  Height = 341
  Caption = 'Child Browser'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 0
    Width = 409
    Height = 288
    Align = alClient
    TabOrder = 0
    Silent = False
    OnStatusTextChange = EmbeddedWB1StatusTextChange
    OnDownloadComplete = EmbeddedWB1DownloadComplete
    OnNewWindow2 = EmbeddedWB1NewWindow2
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
    UserAgent = 'EmbeddedWB 14.55 from: http://www.bsalsa.com/'
    ControlData = {
      4C000000452A0000BB1F00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 288
    Width = 409
    Height = 19
    Panels = <>
    SimplePanel = False
  end
end
