inherited frmMain: TfrmMain
  Left = 53
  Top = 40
  HelpContext = 1050
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  ClientHeight = 494
  ClientWidth = 846
  Constraints.MinHeight = 530
  Constraints.MinWidth = 600
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = TntFormClose
  OnCloseQuery = TntFormCloseQuery
  ExplicitWidth = 862
  ExplicitHeight = 533
  PixelsPerInch = 96
  TextHeight = 13
  object AppEvents: TApplicationEvents
    OnMessage = AppEventsMessage
    Left = 416
    Top = 256
  end
end
