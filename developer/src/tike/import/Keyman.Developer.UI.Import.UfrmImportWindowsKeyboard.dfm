inherited Form2: TForm2
  Left = 294
  Top = 305
  HelpContext = 1201
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Windows Keyboard to Import'
  ClientHeight = 337
  ClientWidth = 329
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  ExplicitWidth = 335
  ExplicitHeight = 366
  PixelsPerInch = 96
  TextHeight = 13
  object lblKeyboards: TLabel
    Left = 8
    Top = 8
    Width = 99
    Height = 13
    Caption = 'Windows &keyboards:'
    FocusControl = lbKeyboards
  end
  object lbKeyboards: TListBox
    Left = 8
    Top = 27
    Width = 313
    Height = 254
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbKeyboardsClick
    OnDblClick = lbKeyboardsDblClick
  end
  object cmdOK: TButton
    Left = 89
    Top = 300
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object cmdCancel: TButton
    Left = 168
    Top = 300
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
