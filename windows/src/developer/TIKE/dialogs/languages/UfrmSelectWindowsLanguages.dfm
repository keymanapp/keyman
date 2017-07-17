inherited frmSelectWindowsLanguages: TfrmSelectWindowsLanguages
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Languages'
  ClientHeight = 409
  ClientWidth = 369
  Position = poScreenCenter
  ExplicitWidth = 375
  ExplicitHeight = 438
  PixelsPerInch = 96
  TextHeight = 13
  object lbLanguages: TListBox
    Left = 12
    Top = 12
    Width = 345
    Height = 341
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
    OnClick = lbLanguagesClick
  end
  object cmdOK: TButton
    Left = 106
    Top = 372
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object cmdCancel: TButton
    Left = 190
    Top = 372
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
