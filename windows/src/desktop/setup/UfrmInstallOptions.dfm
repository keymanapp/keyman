object frmInstallOptions: TfrmInstallOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'ssOptionsTitle'
  ClientHeight = 386
  ClientWidth = 646
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    646
    386)
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 8
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
    Left = 8
    Top = 78
    Width = 159
    Height = 16
    Caption = 'Default Keyman settings'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 172
    Width = 233
    Height = 16
    Caption = 'Select modules to install or upgrade'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 404
    Top = 172
    Width = 205
    Height = 16
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'Associated Keyboard Language'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object chkStartWithWindows: TCheckBox
    Left = 8
    Top = 100
    Width = 473
    Height = 17
    Caption = 'ssOptionsStartWithWindows'
    TabOrder = 0
  end
  object chkStartAfterInstall: TCheckBox
    Left = 8
    Top = 30
    Width = 473
    Height = 17
    Caption = 'ssOptionsStartAfterInstall'
    TabOrder = 1
  end
  object chkCheckForUpdates: TCheckBox
    Left = 8
    Top = 146
    Width = 473
    Height = 17
    Caption = 'ssOptionsCheckForUpdates'
    TabOrder = 2
  end
  object chkUpgradeKeyman7: TCheckBox
    Left = 8
    Top = 53
    Width = 473
    Height = 17
    Caption = 'ssOptionsUpgradeKeyboards'
    TabOrder = 3
  end
  object cmdOK: TButton
    Left = 242
    Top = 353
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 330
    Top = 353
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chkAutomaticallyReportUsage: TCheckBox
    Left = 8
    Top = 123
    Width = 473
    Height = 17
    Caption = 'ssOptionsAutomaticallyReportUsage'
    TabOrder = 6
  end
  object sbTargets: TScrollBox
    Left = 6
    Top = 192
    Width = 619
    Height = 151
    BorderStyle = bsNone
    ParentBackground = True
    TabOrder = 7
  end
end
