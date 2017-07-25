inherited frmHelp: TfrmHelp
  Action = actHelpContextRefresh
  Caption = 'Help'
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object web: TKeymanEmbeddedWB [0]
    Left = 0
    Top = 0
    Width = 480
    Height = 247
    Align = alClient
    TabOrder = 0
    Silent = False
    RegisterAsDropTarget = False
    OnBeforeNavigate2 = webBeforeNavigate2
    OnDocumentComplete = webDocumentComplete
    OnNewWindow3 = webNewWindow3
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    DropHandlingType = ddtCustom
    OnShowMessage = webShowMessage
    OnGetDropTarget = webGetDropTarget2
    About = ' EmbeddedWB http://bsalsa.com/'
    DisableErrors.fpExceptions = False
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    UserAgent = 'EmbeddedWB 14.52 from: http://www.bsalsa.com/'
    OnKeyDown = webKeyDown
    ExplicitLeft = 108
    ExplicitTop = 60
    ExplicitWidth = 300
    ExplicitHeight = 150
    ControlData = {
      4C000000072C00008F1D00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object ActionList1: TActionList
    Left = 244
    Top = 180
    object actHelpContextRefresh: TAction
      Caption = 'actHelpContextRefresh'
      OnUpdate = actHelpContextRefreshUpdate
    end
  end
end
