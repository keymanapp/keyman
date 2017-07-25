inherited frmPrintOSK: TfrmPrintOSK
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 151
  ClientWidth = 233
  Font.Name = 'Tahoma'
  OldCreateOrder = True
  Position = poScreenCenter
  ExplicitWidth = 239
  ExplicitHeight = 180
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TLabel
    Left = 43
    Top = 64
    Width = 138
    Height = 13
    Caption = 'Printing On Screen Keyboard'
  end
  object web: TKeymanEmbeddedWB
    Left = -20
    Top = -20
    Width = 10
    Height = 10
    TabStop = False
    TabOrder = 0
    Silent = False
    OnDocumentComplete = webDocumentComplete
    OnPrintTemplateTeardown = webPrintTemplateTeardown
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    DisableErrors.fpExceptions = False
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    ControlData = {
      4C00000009010000090100000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
