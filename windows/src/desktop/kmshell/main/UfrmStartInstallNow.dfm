object frmStartInstallNow: TfrmStartInstallNow
  Left = 0
  Top = 0
  Caption = 'Keyman Update'
  ClientHeight = 225
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object UpdateMessage: TLabel
    Left = 56
    Top = 88
    Width = 289
    Height = 38
    Caption = 'Keyman and Windows will be restarted'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object UpdateNow: TLabel
    Left = 56
    Top = 40
    Width = 115
    Height = 25
    Caption = 'Update Now'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Install: TButton
    Left = 228
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 0
    OnClick = InstallClick
  end
  object Later: TButton
    Left = 336
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = LaterClick
  end
end
