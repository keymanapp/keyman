object frmInstallOptions: TfrmInstallOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Install Options'
  ClientHeight = 213
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 16
  object chkStartWithWindows: TCheckBox
    Left = 16
    Top = 16
    Width = 281
    Height = 17
    Caption = 'Start Keyman Desktop when Windows starts'
    TabOrder = 0
  end
  object chkStartAfterInstall: TCheckBox
    Left = 16
    Top = 44
    Width = 325
    Height = 17
    Caption = 'Start Keyman Desktop when installation completes'
    TabOrder = 1
  end
  object chkCheckForUpdates: TCheckBox
    Left = 16
    Top = 72
    Width = 269
    Height = 17
    Caption = 'Check for updates online periodically'
    TabOrder = 2
  end
  object chkCheckForUpdatesInstall: TCheckBox
    Left = 44
    Top = 100
    Width = 273
    Height = 17
    Caption = 'Check for updates before installing'
    TabOrder = 3
  end
  object chkUpgradeKeyman7: TCheckBox
    Left = 16
    Top = 127
    Width = 441
    Height = 17
    Caption = 
      'Upgrade Keyman Desktop 6, 7, 8 or 9 keyboards to Keyman Desktop ' +
      '10'
    TabOrder = 4
  end
  object cmdOK: TButton
    Left = 138
    Top = 172
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object cmdCancel: TButton
    Left = 226
    Top = 172
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
