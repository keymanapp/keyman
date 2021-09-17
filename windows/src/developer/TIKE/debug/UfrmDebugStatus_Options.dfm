inherited frmDebugStatus_Options: TfrmDebugStatus_Options
  Caption = 'Options'
  ClientHeight = 172
  ClientWidth = 429
  ExplicitWidth = 429
  ExplicitHeight = 172
  PixelsPerInch = 96
  TextHeight = 13
  object gbPlatform: TGroupBox
    Left = 8
    Top = 8
    Width = 225
    Height = 158
    Caption = 'Platform'
    TabOrder = 0
    object lblPlatformOS: TLabel
      Left = 16
      Top = 21
      Width = 90
      Height = 13
      Caption = '&Operating System:'
    end
    object lblPlatformUI: TLabel
      Left = 16
      Top = 48
      Width = 74
      Height = 13
      Caption = '&User Interface:'
    end
    object lblPlatformFormFactor: TLabel
      Left = 16
      Top = 75
      Width = 62
      Height = 13
      Caption = '&Form Factor:'
    end
    object lblPlatformApplication: TLabel
      Left = 16
      Top = 102
      Width = 56
      Height = 13
      Caption = '&Application:'
    end
    object lblPlatformBrowser: TLabel
      Left = 16
      Top = 129
      Width = 43
      Height = 13
      Caption = '&Browser:'
    end
    object cbPlatformOS: TComboBox
      Left = 118
      Top = 18
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnClick = cbPlatformClick
    end
    object cbPlatformUI: TComboBox
      Left = 118
      Top = 45
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnClick = cbPlatformClick
    end
    object cbPlatformFormFactor: TComboBox
      Left = 118
      Top = 72
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      OnClick = cbPlatformClick
    end
    object cbPlatformApplication: TComboBox
      Left = 118
      Top = 99
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnClick = cbPlatformClick
    end
    object cbPlatformBrowser: TComboBox
      Left = 118
      Top = 126
      Width = 97
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnClick = cbPlatformClick
    end
  end
end
