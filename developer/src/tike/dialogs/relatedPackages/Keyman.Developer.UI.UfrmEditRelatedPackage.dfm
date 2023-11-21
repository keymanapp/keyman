inherited frmEditRelatedPackage: TfrmEditRelatedPackage
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Related Package'
  ClientHeight = 163
  ClientWidth = 289
  Position = poScreenCenter
  ExplicitWidth = 295
  ExplicitHeight = 192
  PixelsPerInch = 96
  TextHeight = 13
  object lblPackageID: TLabel
    Left = 8
    Top = 11
    Width = 98
    Height = 13
    Caption = 'Related &Package ID:'
    FocusControl = editPackageID
  end
  object cmdOK: TButton
    Left = 124
    Top = 127
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object cmdCancel: TButton
    Left = 205
    Top = 127
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object editPackageID: TEdit
    Left = 112
    Top = 8
    Width = 131
    Height = 21
    TabOrder = 0
    OnChange = FieldChange
  end
  object chkDeprecates: TCheckBox
    Left = 112
    Top = 35
    Width = 105
    Height = 17
    Caption = 'Deprecated'
    TabOrder = 1
    OnClick = FieldChange
  end
end
