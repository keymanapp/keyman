inherited frmProject: TfrmProject
  Left = 193
  Top = 131
  ActiveControl = web
  Caption = 'frmProject'
  ClientHeight = 606
  ClientWidth = 862
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 862
  ExplicitHeight = 606
  PixelsPerInch = 96
  TextHeight = 13
  object web: TKeymanEmbeddedWB
    Left = 0
    Top = 0
    Width = 862
    Height = 606
    Align = alClient
    TabOrder = 0
    OnEnter = webEnter
    Silent = False
    RegisterAsDropTarget = False
    OnBeforeNavigate2 = webBeforeNavigate2
    OnDocumentComplete = webDocumentComplete
    OnNewWindow3 = webNewWindow3
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    DropHandlingType = ddtCustom
    OnShowHelpRequest = webShowHelpRequest
    OnGetDropTarget = webGetDropTarget2
    OnShowContextMenu = webShowContextMenu
    OnTranslateAccelerator = webTranslateAccelerator2
    OnScriptError = webScriptError
    About = ' EmbeddedWB http://bsalsa.com/'
    DisableErrors.fpExceptions = False
    OnMessage = webMessage
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    OnKeyDown = webKeyDown
    ControlData = {
      4C00000017590000A23E00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object dlgOpenFile: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Add File to Project'
    Left = 88
    Top = 56
  end
  object tmrRefresh: TTimer
    Enabled = False
    OnTimer = tmrRefreshTimer
    Left = 92
    Top = 112
  end
  object WebBrowserFocusMonitor1: TWebBrowserFocusMonitor
    WebBrowser = web
    Left = 140
    Top = 48
  end
end
