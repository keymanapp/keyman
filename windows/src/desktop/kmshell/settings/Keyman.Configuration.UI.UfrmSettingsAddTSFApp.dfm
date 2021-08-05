inherited frmSettingsAddTSFApp: TfrmSettingsAddTSFApp
  Left = 0
  Top = 0
  ActiveControl = editFilename
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Text Services Framework Application Overrides'
  ClientHeight = 139
  ClientWidth = 442
  Font.Name = 'Tahoma'
  Position = poOwnerFormCenter
  ExplicitWidth = 448
  ExplicitHeight = 168
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 425
    Height = 26
    Caption = 
      'Add per-application overrides for Text Services Framework suppor' +
      't. Use the executable filename of the application.  Set value to' +
      ' 1 to enable support and 0 to disable.'
    WordWrap = True
  end
  object lblFilename: TLabel
    Left = 8
    Top = 51
    Width = 46
    Height = 13
    Caption = 'Filename:'
  end
  object Label2: TLabel
    Left = 8
    Top = 78
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object cmdOK: TButton
    Left = 275
    Top = 106
    Width = 77
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 356
    Top = 106
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object editFilename: TEdit
    Left = 80
    Top = 48
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object cbValue: TComboBox
    Left = 80
    Top = 75
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    Items.Strings = (
      '0 - Disabled'
      '1 - Enabled')
  end
end
