object frmStartInstall: TfrmStartInstall
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
  object InstallUpdate: TLabel
    Left = 128
    Top = 96
    Width = 175
    Height = 19
    Caption = 'Keyman update available'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Install: TButton
    Left = 228
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Install'
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
