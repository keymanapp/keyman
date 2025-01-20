object frmKeyman7Main: TfrmKeyman7Main
  Left = 313
  Top = 353
  Caption = 'frmKeyman7Main'
  ClientHeight = 197
  ClientWidth = 571
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
  object mnu: TPopupMenu
    OwnerDraw = True
    Left = 24
    Top = 4
  end
  object tmrTestKeymanFunctioning: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrTestKeymanFunctioningTimer
    Left = 28
    Top = 40
  end
  object tmrBackgroundUpdateCheck: TTimer
    Enabled = False
    Interval = 300000
    OnTimer = tmrBackgroundUpdateCheckTimer
    Left = 280
    Top = 104
  end
  object tmrCheckInputPane: TTimer
    Interval = 500
    OnTimer = tmrCheckInputPaneTimer
    Left = 96
    Top = 104
  end
  object tmrRefresh: TTimer
    Enabled = False
    Interval = 125
    OnTimer = tmrRefreshTimer
    Left = 440
    Top = 40
  end
end
