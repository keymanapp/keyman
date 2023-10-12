inherited frmEditLanguageExample: TfrmEditLanguageExample
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Language Example'
  ClientHeight = 163
  ClientWidth = 289
  Position = poScreenCenter
  ExplicitWidth = 295
  ExplicitHeight = 192
  PixelsPerInch = 96
  TextHeight = 13
  object lblLanguageTag: TLabel
    Left = 8
    Top = 11
    Width = 70
    Height = 13
    Caption = '&Language tag:'
    FocusControl = editLanguageID
  end
  object lblExampleNote: TLabel
    Left = 8
    Top = 92
    Width = 27
    Height = 13
    Caption = '&Note:'
    FocusControl = editExampleNote
  end
  object lblExampleKeys: TLabel
    Left = 8
    Top = 38
    Width = 71
    Height = 13
    Caption = 'Key &sequence:'
    FocusControl = editExampleKeys
  end
  object lblExampleText: TLabel
    Left = 8
    Top = 65
    Width = 72
    Height = 13
    Caption = 'Expected &text:'
    FocusControl = editExampleText
  end
  object cmdOK: TButton
    Left = 124
    Top = 127
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object cmdCancel: TButton
    Left = 205
    Top = 127
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object editExampleNote: TEdit
    Left = 94
    Top = 89
    Width = 186
    Height = 21
    TabOrder = 3
    OnChange = FieldChange
  end
  object editLanguageID: TEdit
    Left = 94
    Top = 8
    Width = 131
    Height = 21
    TabOrder = 0
    OnChange = FieldChange
  end
  object editExampleKeys: TEdit
    Left = 94
    Top = 35
    Width = 186
    Height = 21
    TabOrder = 1
    OnChange = FieldChange
  end
  object editExampleText: TEdit
    Left = 94
    Top = 62
    Width = 186
    Height = 21
    TabOrder = 2
    OnChange = FieldChange
  end
end
