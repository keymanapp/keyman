inherited frmDebugStatus_Platform: TfrmDebugStatus_Platform
  Caption = 'Platform'
  ClientHeight = 172
  ClientWidth = 429
  ExplicitWidth = 429
  ExplicitHeight = 172
  PixelsPerInch = 96
  TextHeight = 13
  object lblPlatformOS: TLabel
    Left = 8
    Top = 11
    Width = 90
    Height = 13
    Caption = '&Operating System:'
  end
  object lblPlatformUI: TLabel
    Left = 8
    Top = 38
    Width = 74
    Height = 13
    Caption = '&User Interface:'
  end
  object lblPlatformFormFactor: TLabel
    Left = 8
    Top = 65
    Width = 62
    Height = 13
    Caption = '&Form Factor:'
  end
  object lblPlatformApplication: TLabel
    Left = 8
    Top = 92
    Width = 56
    Height = 13
    Caption = '&Application:'
  end
  object lblPlatformBrowser: TLabel
    Left = 8
    Top = 119
    Width = 43
    Height = 13
    Caption = '&Browser:'
  end
  object cbPlatformOS: TComboBox
    Left = 110
    Top = 8
    Width = 97
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnClick = cbPlatformClick
  end
  object cbPlatformUI: TComboBox
    Left = 110
    Top = 35
    Width = 97
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnClick = cbPlatformClick
  end
  object cbPlatformFormFactor: TComboBox
    Left = 110
    Top = 62
    Width = 97
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnClick = cbPlatformClick
  end
  object cbPlatformApplication: TComboBox
    Left = 110
    Top = 89
    Width = 97
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnClick = cbPlatformClick
  end
  object cbPlatformBrowser: TComboBox
    Left = 110
    Top = 116
    Width = 97
    Height = 21
    Style = csDropDownList
    TabOrder = 4
    OnClick = cbPlatformClick
  end
end
