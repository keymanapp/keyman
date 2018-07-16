inherited frmProject: TfrmProject
  Left = 193
  Top = 131
  ActiveControl = cef
  Caption = 'frmProject'
  ClientHeight = 606
  ClientWidth = 862
  OldCreateOrder = True
  ExplicitWidth = 862
  ExplicitHeight = 606
  PixelsPerInch = 96
  TextHeight = 13
  object cef: TChromiumWindow
    Left = 0
    Top = 0
    Width = 862
    Height = 606
    Align = alClient
    TabOrder = 0
    OnClose = cefClose
    OnBeforeClose = cefBeforeClose
    OnAfterCreated = cefAfterCreated
<<<<<<< HEAD
=======
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 457
    ExplicitHeight = 505
>>>>>>> 93075f43f52eac4dc666b529c9e9a10b0c5d39d9
  end
  object dlgOpenFile: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Add File to Project'
    Left = 536
    Top = 40
  end
  object tmrRefresh: TTimer
    Enabled = False
    OnTimer = tmrRefreshTimer
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
end
