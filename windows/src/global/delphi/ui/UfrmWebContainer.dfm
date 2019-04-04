inherited frmWebContainer: TfrmWebContainer
  Left = 0
  Top = 0
  HelpContext = 0
  ActiveControl = web
  ClientHeight = 286
  ClientWidth = 426
  Font.Name = 'Tahoma'
  KeyPreview = True
  ExplicitWidth = 442
  ExplicitHeight = 325
  PixelsPerInch = 96
  TextHeight = 13
  object web: TKeymanEmbeddedWB
    Left = 296
    Top = 16
    Width = 122
    Height = 70
    TabOrder = 0
    OnBeforeNavigate2 = webBeforeNavigate2
    OnDocumentComplete = webDocumentComplete
    OnNewWindow3 = webNewWindow3
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [DisableTextSelect, DontUse3DBorders, EnableThemes]
    OnShowHelpRequest = webShowHelpRequest1
    OnShowContextMenu = webShowContextMenu2
    OnScriptError = webScriptError2
    About = ' EmbeddedWB http://bsalsa.com/'
    DisableErrors.fpExceptions = False
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    OnKeyDown = webKeyDown
    ControlData = {
      4C00000078330000482100000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
