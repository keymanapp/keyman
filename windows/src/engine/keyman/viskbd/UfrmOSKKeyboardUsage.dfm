inherited frmOSKKeyboardUsage: TfrmOSKKeyboardUsage
  Caption = 'frmOSKKeyboardUsage'
  ClientHeight = 339
  ClientWidth = 635
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 635
  ExplicitHeight = 339
  PixelsPerInch = 96
  TextHeight = 13
  object web: TKeymanEmbeddedWB
    Left = 0
    Top = 0
    Width = 635
    Height = 339
    Align = alClient
    TabOrder = 0
    OnBeforeNavigate2 = webBeforeNavigate2
    OnDocumentComplete = webDocumentComplete
    OnNewWindow3 = webNewWindow3
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [DisableTextSelect, DontUse3DBorders, EnablesFormsAutoComplete, EnableThemes]
    OnShowHelpRequest = webShowHelpRequest1
    OnShowContextMenu = webShowContextMenu
    OnScriptError = webScriptError
    About = ' EmbeddedWB http://bsalsa.com/'
    DisableErrors.fpExceptions = False
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    OnKeyDown = webKeyDown
    ExplicitLeft = 32
    ExplicitTop = 8
    ExplicitWidth = 461
    ExplicitHeight = 265
    ControlData = {
      4C000000A52F0000631B00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E12620C000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
