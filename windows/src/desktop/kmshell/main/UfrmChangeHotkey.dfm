inherited frmChangeHotkey: TfrmChangeHotkey
  Left = 0
  Top = 0
  ActiveControl = hkHotkey
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsDialog
  Caption = 'Change Hotkey'
  ClientHeight = 217
  ClientWidth = 305
  Font.Name = 'Tahoma'
  Position = poScreenCenter
  ExplicitWidth = 311
  ExplicitHeight = 246
  PixelsPerInch = 96
  TextHeight = 13
  object lblHotkey: TLabel
    Left = 12
    Top = 12
    Width = 281
    Height = 49
    AutoSize = False
    Caption = 'lblHotkey'
    WordWrap = True
  end
  object hkHotkey: THotKey
    Left = 75
    Top = 152
    Width = 155
    Height = 19
    HotKey = 49217
    Modifiers = [hkCtrl, hkAlt]
    TabOrder = 4
    OnChange = hkHotkeyChange
  end
  object cmdOK: TButton
    Left = 140
    Top = 180
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 220
    Top = 180
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object rbAltLeftShift: TRadioButton
    Left = 12
    Top = 84
    Width = 113
    Height = 17
    Caption = 'Left Alt + Shift'
    TabOrder = 1
    OnClick = rbNoneClick
  end
  object rbNone: TRadioButton
    Left = 12
    Top = 61
    Width = 113
    Height = 17
    Caption = 'No Hotkey'
    TabOrder = 0
    OnClick = rbNoneClick
  end
  object rbCtrlShift: TRadioButton
    Left = 12
    Top = 107
    Width = 113
    Height = 17
    Caption = 'Ctrl + Shift'
    TabOrder = 2
    OnClick = rbNoneClick
  end
  object rbCustom: TRadioButton
    Left = 12
    Top = 129
    Width = 113
    Height = 17
    Caption = 'Custom'
    TabOrder = 3
    OnClick = rbNoneClick
  end
end
