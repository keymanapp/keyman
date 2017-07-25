object frmOnlineUpdateIcon: TfrmOnlineUpdateIcon
  Left = 0
  Top = 0
  Caption = 'Online Update Check Hidden Form'
  ClientHeight = 292
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object trayMenu: TPopupMenu
    Left = 272
    Top = 152
    object cmdView: TMenuItem
      Caption = 'View and &install updates'
      Default = True
      OnClick = cmdViewClick
    end
    object cmdExit: TMenuItem
      Caption = 'E&xit online update check'
      OnClick = cmdExitClick
    end
  end
  object tmrShowBalloon: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = tmrShowBalloonTimer
    Left = 328
    Top = 152
  end
end
