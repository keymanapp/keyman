inherited frmEditPackageWebFonts: TfrmEditPackageWebFonts
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Web Fonts'
  ClientHeight = 303
  ClientWidth = 289
  Position = poScreenCenter
  ExplicitWidth = 295
  ExplicitHeight = 332
  PixelsPerInch = 96
  TextHeight = 13
  object cmdOK: TButton
    Left = 125
    Top = 270
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 206
    Top = 270
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object clbFonts: TCheckListBox
    Left = 8
    Top = 8
    Width = 273
    Height = 256
    ItemHeight = 13
    TabOrder = 2
  end
end
