inherited frmBitmapEditorText: TfrmBitmapEditorText
  ActiveControl = editText
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Insert Text'
  ClientHeight = 189
  ClientWidth = 377
  Position = poDesigned
  OnDestroy = FormDestroy
  ExplicitWidth = 383
  ExplicitHeight = 218
  PixelsPerInch = 96
  TextHeight = 13
  object lblText: TLabel
    Left = 12
    Top = 87
    Width = 22
    Height = 13
    Caption = '&Text'
    FocusControl = editText
  end
  object lblFont: TLabel
    Left = 12
    Top = 23
    Width = 22
    Height = 13
    Caption = '&Font'
  end
  object lblQuality: TLabel
    Left = 12
    Top = 51
    Width = 34
    Height = 13
    Caption = '&Quality'
  end
  object editText: TEdit
    Left = 64
    Top = 84
    Width = 301
    Height = 21
    TabOrder = 3
    OnChange = editTextChange
  end
  object cmdOK: TButton
    Left = 58
    Top = 152
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object cmdCancel: TButton
    Left = 138
    Top = 152
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object cbDisplayQuality: TComboBox
    Left = 64
    Top = 48
    Width = 233
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnClick = cbDisplayQualityClick
    Items.Strings = (
      'Plain'
      'Anti-aliased'
      'Cleartype')
  end
  object editFont: TEdit
    Left = 64
    Top = 20
    Width = 204
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object cmdFont: TButton
    Left = 304
    Top = 20
    Width = 61
    Height = 21
    Caption = '&Change...'
    TabOrder = 1
    OnClick = cmdFontClick
  end
  object cpTextColor: TmbColorPreview
    Left = 274
    Top = 20
    Width = 24
    Height = 21
    Hint = 'Text Color'
    Color = clNone
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    OnApply = dlgFontApply
    Left = 16
    Top = 124
  end
end
