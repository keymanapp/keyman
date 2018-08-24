inherited frmSelectBCP47Language: TfrmSelectBCP47Language
  ActiveControl = cbLanguageTag
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select BCP 47 Tag'
  ClientHeight = 216
  ClientWidth = 436
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitHeight = 245
  PixelsPerInch = 96
  TextHeight = 13
  object lblLanguageTag: TLabel
    Left = 8
    Top = 11
    Width = 70
    Height = 13
    Caption = '&Language tag:'
    FocusControl = cbLanguageTag
  end
  object lblScriptTag: TLabel
    Left = 8
    Top = 38
    Width = 50
    Height = 13
    Caption = '&Script tag:'
    FocusControl = cbScriptTag
  end
  object lblRegionTag: TLabel
    Left = 8
    Top = 65
    Width = 56
    Height = 13
    Caption = '&Region tag:'
    FocusControl = cbRegionTag
  end
  object lblBCP47Code: TLabel
    Left = 8
    Top = 92
    Width = 66
    Height = 13
    Caption = 'BCP 47 Code:'
    FocusControl = editBCP47Code
  end
  object lblValidateCode: TLabel
    Left = 84
    Top = 116
    Width = 344
    Height = 13
    AutoSize = False
  end
  object lblLanguageName: TLabel
    Left = 234
    Top = 11
    Width = 74
    Height = 13
    Caption = 'language-name'
    FocusControl = cbLanguageTag
  end
  object lblScriptName: TLabel
    Left = 234
    Top = 38
    Width = 56
    Height = 13
    Caption = 'script-name'
    FocusControl = cbLanguageTag
  end
  object lblRegionName: TLabel
    Left = 234
    Top = 65
    Width = 60
    Height = 13
    Caption = 'region-name'
    FocusControl = cbLanguageTag
  end
  object Label1: TLabel
    Left = 8
    Top = 153
    Width = 80
    Height = 13
    Caption = 'Language &name:'
    FocusControl = editLanguageName
  end
  object cmdOK: TButton
    Left = 272
    Top = 183
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object cmdCancel: TButton
    Left = 353
    Top = 183
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object cbLanguageTag: TComboBox
    Left = 94
    Top = 8
    Width = 121
    Height = 21
    MaxLength = 3
    Sorted = True
    TabOrder = 0
    OnChange = cbLanguageTagChange
  end
  object cbScriptTag: TComboBox
    Left = 94
    Top = 35
    Width = 121
    Height = 21
    MaxLength = 4
    Sorted = True
    TabOrder = 1
    OnChange = cbScriptTagChange
  end
  object cbRegionTag: TComboBox
    Left = 94
    Top = 62
    Width = 121
    Height = 21
    MaxLength = 3
    Sorted = True
    TabOrder = 2
    OnChange = cbRegionTagChange
  end
  object editBCP47Code: TEdit
    Left = 94
    Top = 89
    Width = 229
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
  object lblLinkToW3C: TLinkLabel
    Left = 8
    Top = 191
    Width = 194
    Height = 17
    Caption = 
      '<a href="https://www.w3.org/International/questions/qa-choosing-' +
      'language-tags">Learn about how to select a BCP 47 tag</a>'
    TabOrder = 6
    OnLinkClick = lblLinkToW3CLinkClick
  end
  object editLanguageName: TEdit
    Left = 94
    Top = 150
    Width = 186
    Height = 21
    TabOrder = 4
    OnChange = editLanguageNameChange
  end
  object cmdResetLanguageName: TButton
    Left = 286
    Top = 149
    Width = 75
    Height = 23
    Caption = 'R&eset'
    TabOrder = 5
    OnClick = cmdResetLanguageNameClick
  end
end
