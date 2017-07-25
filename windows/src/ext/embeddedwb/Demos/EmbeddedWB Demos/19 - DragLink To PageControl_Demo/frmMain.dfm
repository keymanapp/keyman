object Form1: TForm1
  Left = 292
  Top = 122
  Width = 473
  Height = 434
  Caption = 'Drag a Link to a PageControl TabSheet'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 457
    Height = 398
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object EmbeddedWB1: TEmbeddedWB
        Left = 0
        Top = 0
        Width = 449
        Height = 370
        Align = alClient
        TabOrder = 0
        OnDownloadComplete = EmbeddedWB1DownloadComplete
        DisableCtrlShortcuts = 'N'
        UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
        About = ' EmbeddedWB http://bsalsa.com/'
        HTMLCode.Strings = (
          'http://www.google.com/')
        PrintOptions.Margins.Left = 19.05
        PrintOptions.Margins.Right = 19.05
        PrintOptions.Margins.Top = 19.05
        PrintOptions.Margins.Bottom = 19.05
        PrintOptions.HTMLHeader.Strings = (
          '<HTML></HTML>')
        PrintOptions.Orientation = poPortrait
        ControlData = {
          4C00000027390000CA2700000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
end
