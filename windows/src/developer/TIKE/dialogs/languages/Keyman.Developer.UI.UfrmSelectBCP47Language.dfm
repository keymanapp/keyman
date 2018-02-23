inherited frmSelectBCP47Language: TfrmSelectBCP47Language
  ActiveControl = editLanguageTag
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select BCP 47 Tag'
  ClientHeight = 186
  ClientWidth = 436
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitHeight = 215
  PixelsPerInch = 96
  TextHeight = 13
  object lblLanguageTag: TLabel
    Left = 8
    Top = 11
    Width = 70
    Height = 13
    Caption = '&Language tag:'
    FocusControl = editLanguageTag
  end
  object lblScriptTag: TLabel
    Left = 8
    Top = 38
    Width = 50
    Height = 13
    Caption = '&Script tag:'
    FocusControl = editScriptTag
  end
  object lblRegionTag: TLabel
    Left = 8
    Top = 65
    Width = 56
    Height = 13
    Caption = '&Region tag:'
    FocusControl = editRegionTag
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
    Left = 224
    Top = 11
    Width = 74
    Height = 13
    Caption = 'language-name'
    FocusControl = editLanguageTag
  end
  object lblScriptName: TLabel
    Left = 224
    Top = 38
    Width = 56
    Height = 13
    Caption = 'script-name'
    FocusControl = editLanguageTag
  end
  object lblRegionName: TLabel
    Left = 224
    Top = 65
    Width = 60
    Height = 13
    Caption = 'region-name'
    FocusControl = editLanguageTag
  end
  object cmdOK: TButton
    Left = 272
    Top = 151
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object cmdCancel: TButton
    Left = 353
    Top = 151
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object editLanguageTag: TEdit
    Left = 84
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 2
    OnChange = editLanguageTagChange
  end
  object editScriptTag: TEdit
    Left = 84
    Top = 35
    Width = 121
    Height = 21
    TabOrder = 3
    OnChange = editScriptTagChange
  end
  object editRegionTag: TEdit
    Left = 84
    Top = 62
    Width = 121
    Height = 21
    TabOrder = 4
    OnChange = editRegionTagChange
  end
  object editBCP47Code: TEdit
    Left = 84
    Top = 89
    Width = 229
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 5
  end
  object lblLinkToW3C: TLinkLabel
    Left = 8
    Top = 159
    Width = 194
    Height = 17
    Caption = 
      '<a href="https://www.w3.org/International/questions/qa-choosing-' +
      'language-tags">Learn about how to select a BCP 47 tag</a>'
    TabOrder = 6
    OnLinkClick = lblLinkToW3CLinkClick
  end
end
