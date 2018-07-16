inherited frmHelp: TfrmHelp
  Action = actHelpContextRefresh
  Caption = 'Help'
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object cef: TChromiumWindow [0]
    Left = 0
    Top = 0
    Width = 480
    Height = 247
    Align = alClient
    Color = clInfoBk
    TabOrder = 0
    OnClose = cefClose
    OnBeforeClose = cefBeforeClose
    OnAfterCreated = cefAfterCreated
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 100
    ExplicitHeight = 41
  end
  object ActionList1: TActionList
    Left = 244
    Top = 180
    object actHelpContextRefresh: TAction
      Caption = 'actHelpContextRefresh'
      OnUpdate = actHelpContextRefreshUpdate
    end
  end
  object tmrCreateBrowser: TTimer
    Enabled = False
    Interval = 300
    OnTimer = tmrCreateBrowserTimer
    Left = 232
    Top = 128
  end
end
