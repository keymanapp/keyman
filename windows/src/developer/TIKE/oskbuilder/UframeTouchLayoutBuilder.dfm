inherited frameTouchLayoutBuilder: TframeTouchLayoutBuilder
  BorderIcons = []
  BorderStyle = bsNone
  Caption = ''
  ClientHeight = 338
  ClientWidth = 651
  OnDestroy = FormDestroy
  ExplicitWidth = 651
  ExplicitHeight = 338
  PixelsPerInch = 96
  TextHeight = 13
  object web: TKeymanEmbeddedWB
    Left = 0
    Top = 0
    Width = 651
    Height = 338
    Align = alClient
    TabOrder = 0
    OnBeforeNavigate2 = webBeforeNavigate2
    OnDocumentComplete = webDocumentComplete
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    OnTranslateAccelerator = webTranslateAccelerator
    OnScriptError = webScriptError
    About = ' EmbeddedWB http://bsalsa.com/'
    DisableErrors.fpExceptions = False
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    VisualEffects.DisableSounds = True
    UserAgent = 
      'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Triden' +
      't/5.0; TIKE/9.0)'
    UserAgentMode = uaInternal
    ExplicitLeft = 176
    ExplicitTop = 96
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C00000048430000EF2200000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
