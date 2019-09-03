inherited frmNewModelProjectParameters: TfrmNewModelProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New Wordlist Lexical Model Project'
  ClientHeight = 309
  ClientWidth = 625
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 631
  ExplicitHeight = 338
  DesignSize = (
    625
    309)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileName: TLabel
    Left = 12
    Top = 275
    Width = 46
    Height = 13
    Caption = '&Model ID:'
    FocusControl = editModelID
  end
  object lblPath: TLabel
    Left = 12
    Top = 248
    Width = 26
    Height = 13
    Caption = '&Path:'
    FocusControl = editPath
  end
  object lblAuthorID: TLabel
    Left = 12
    Top = 11
    Width = 51
    Height = 13
    Caption = 'Aut&hor ID:'
    FocusControl = editAuthorID
  end
  object lblCoypright: TLabel
    Left = 12
    Top = 113
    Width = 51
    Height = 13
    Caption = '&Copyright:'
    FocusControl = editCopyright
  end
  object lblVersion: TLabel
    Left = 12
    Top = 140
    Width = 39
    Height = 13
    Caption = '&Version:'
    FocusControl = editVersion
  end
  object lblAuthor: TLabel
    Left = 12
    Top = 167
    Width = 67
    Height = 13
    Caption = 'A&uthor Name:'
    FocusControl = editAuthor
  end
  object lblLanguages: TLabel
    Left = 339
    Top = 11
    Width = 52
    Height = 13
    Caption = '&Languages'
    FocusControl = gridLanguages
  end
  object lblBCP47: TLabel
    Left = 12
    Top = 38
    Width = 95
    Height = 13
    Caption = 'Primary BCP47 &Tag:'
  end
  object lblUniq: TLabel
    Left = 12
    Top = 65
    Width = 67
    Height = 13
    Caption = 'Uni&que Name:'
    FocusControl = editUniq
  end
  object editModelID: TEdit
    Left = 120
    Top = 272
    Width = 149
    Height = 21
    ParentColor = True
    ReadOnly = True
    TabOrder = 11
    OnChange = editModelIDChange
  end
  object cmdBrowse: TButton
    Left = 275
    Top = 272
    Width = 78
    Height = 21
    Caption = '&Browse...'
    TabOrder = 12
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 245
    Width = 233
    Height = 21
    TabOrder = 10
    OnChange = editPathChange
  end
  object editAuthorID: TEdit
    Left = 120
    Top = 8
    Width = 205
    Height = 21
    TabOrder = 0
    OnChange = editModelIDComponentChange
  end
  object editCopyright: TEdit
    Left = 120
    Top = 110
    Width = 205
    Height = 21
    TabOrder = 3
    Text = #169
    OnChange = editCopyrightChange
  end
  object editVersion: TEdit
    Left = 120
    Top = 137
    Width = 205
    Height = 21
    TabOrder = 4
    Text = '1.0'
    OnChange = editVersionChange
  end
  object editAuthor: TEdit
    Left = 120
    Top = 164
    Width = 205
    Height = 21
    TabOrder = 5
    OnChange = editAuthorChange
  end
  object cmdOK: TButton
    Left = 463
    Top = 270
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 13
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 542
    Top = 270
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 14
  end
  object gridLanguages: TStringGrid
    Left = 339
    Top = 32
    Width = 278
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 9
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 6
    OnClick = gridLanguagesClick
    OnDblClick = gridLanguagesDblClick
    ColWidths = (
      78
      64)
  end
  object cmdAddLanguage: TButton
    Left = 340
    Top = 191
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 7
    OnClick = cmdAddLanguageClick
  end
  object cmdEditLanguage: TButton
    Left = 419
    Top = 191
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Ed&it...'
    TabOrder = 8
    OnClick = cmdEditLanguageClick
  end
  object cmdRemoveLanguage: TButton
    Left = 498
    Top = 191
    Width = 72
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Remove'
    TabOrder = 9
    OnClick = cmdRemoveLanguageClick
  end
  object editUniq: TEdit
    Left = 120
    Top = 62
    Width = 205
    Height = 21
    TabOrder = 2
    OnChange = editModelIDComponentChange
  end
  object cbBCP47: TComboBox
    Left = 120
    Top = 35
    Width = 205
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnClick = editModelIDComponentChange
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'kpj'
    Filter = 'Project files (*.kpj)|*.kpj|All files (*.*)|*.*'
    FilterIndex = 0
    Title = 'Create New Project'
    OnCanClose = dlgSaveCanClose
    Left = 396
    Top = 240
  end
end
