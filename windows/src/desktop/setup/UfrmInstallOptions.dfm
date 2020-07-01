object frmInstallOptions: TfrmInstallOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'ssOptionsTitle'
  ClientHeight = 386
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
  object Label1: TLabel
    Left = 9
    Top = 8
    Width = 125
    Height = 16
    Caption = 'Installation options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 9
    Top = 214
    Width = 158
    Height = 16
    Caption = 'Default runtime settings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object chkStartWithWindows: TCheckBox
    Left = 9
    Top = 236
    Width = 473
    Height = 17
    Caption = 'ssOptionsStartWithWindows'
    TabOrder = 0
  end
  object chkStartAfterInstall: TCheckBox
    Left = 9
    Top = 30
    Width = 473
    Height = 17
    Caption = 'ssOptionsStartAfterInstall'
    TabOrder = 1
  end
  object chkCheckForUpdates: TCheckBox
    Left = 9
    Top = 282
    Width = 473
    Height = 17
    Caption = 'ssOptionsCheckForUpdates'
    TabOrder = 2
  end
  object chkUpgradeKeyman7: TCheckBox
    Left = 9
    Top = 53
    Width = 473
    Height = 17
    Caption = 'ssOptionsUpgradeKeyboards'
    TabOrder = 3
  end
  object cmdOK: TButton
    Left = 180
    Top = 353
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 268
    Top = 353
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chkAutomaticallyReportUsage: TCheckBox
    Left = 9
    Top = 259
    Width = 473
    Height = 17
    Caption = 'ssOptionsAutomaticallyReportUsage'
    TabOrder = 6
  end
  object sbTargets: TScrollBox
    Left = 8
    Top = 76
    Width = 481
    Height = 132
    BorderStyle = bsNone
    ParentBackground = True
    TabOrder = 7
  end
end
