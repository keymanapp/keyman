object frmInstallOptions: TfrmInstallOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'ssOptionsTitle'
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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object chkStartWithWindows: TCheckBox
    Left = 16
    Top = 16
    Width = 473
    Height = 17
    Caption = 'ssOptionsStartWithWindows'
    TabOrder = 0
  end
  object chkStartAfterInstall: TCheckBox
    Left = 16
    Top = 44
    Width = 473
    Height = 17
    Caption = 'ssOptionsStartAfterInstall'
    TabOrder = 1
  end
  object chkCheckForUpdates: TCheckBox
    Left = 16
    Top = 72
    Width = 473
    Height = 17
    Caption = 'ssOptionsCheckForUpdates'
    TabOrder = 2
  end
  object chkCheckForUpdatesInstall: TCheckBox
    Left = 44
    Top = 100
    Width = 445
    Height = 17
    Caption = 'ssOptionsCheckForUpdatesBeforeInstall'
    TabOrder = 3
  end
  object chkUpgradeKeyman7: TCheckBox
    Left = 16
    Top = 127
    Width = 473
    Height = 17
    Caption = 'ssOptionsUpgradeKeyboards'
    TabOrder = 4
  end
  object cmdOK: TButton
    Left = 172
    Top = 172
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object cmdCancel: TButton
    Left = 260
    Top = 172
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
