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
  object InstallUpdate: TLabel
    Left = 69
    Top = 72
    Width = 296
    Height = 38
    Caption = 
      'Installing Now will require a Keyman and Windows Restart Continu' +
      'e?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
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
