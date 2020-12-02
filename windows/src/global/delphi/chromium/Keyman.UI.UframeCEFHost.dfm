object frameCEFHost: TframeCEFHost
  Left = 193
  Top = 131
  Align = alClient
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 645
  ClientWidth = 878
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object cefwp: TCEFWindowParent
    Left = 0
    Top = 0
    Width = 878
    Height = 645
    Align = alClient
    TabOrder = 0
  end
  object tmrRefresh: TTimer
    Enabled = False
    Left = 604
    Top = 48
  end
  object tmrCreateBrowser: TTimer
    Enabled = False
    Interval = 300
    OnTimer = tmrCreateBrowserTimer
    Left = 620
    Top = 208
  end
  object cef: TChromium
    OnWidgetCompMsg = cefWidgetCompMsg
    OnProcessMessageReceived = cefProcessMessageReceived
    OnLoadEnd = cefLoadEnd
    OnLoadingStateChange = cefLoadingStateChange
    OnSetFocus = cefSetFocus
    OnRunContextMenu = cefRunContextMenu
    OnPreKeyEvent = cefPreKeyEvent
    OnTitleChange = cefTitleChange
    OnBeforePopup = cefBeforePopup
    OnAfterCreated = cefAfterCreated
    OnBeforeClose = cefBeforeClose
    OnClose = cefClose
    OnBeforeBrowse = cefBeforeBrowse
    Left = 424
    Top = 312
  end
end
