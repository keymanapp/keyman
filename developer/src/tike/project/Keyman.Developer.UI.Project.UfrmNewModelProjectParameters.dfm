inherited frmNewModelProjectParameters: TfrmNewModelProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New Wordlist Lexical Model Project'
  ClientHeight = 402
  ClientWidth = 756
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 762
  ExplicitHeight = 431
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileName: TLabel
    Left = 8
    Top = 311
    Width = 46
    Height = 13
    Caption = 'Model &ID:'
    FocusControl = editModelID
  end
  object lblPath: TLabel
    Left = 8
    Top = 149
    Width = 26
    Height = 13
    Caption = '&Path:'
    FocusControl = editPath
  end
  object lblAuthorID: TLabel
    Left = 8
    Top = 230
    Width = 51
    Height = 13
    Caption = 'Aut&hor ID:'
    FocusControl = editAuthorID
  end
  object lblCoypright: TLabel
    Left = 8
    Top = 65
    Width = 51
    Height = 13
    Caption = '&Copyright:'
    FocusControl = editCopyright
  end
  object lblVersion: TLabel
    Left = 8
    Top = 120
    Width = 39
    Height = 13
    Caption = '&Version:'
    FocusControl = editVersion
  end
  object lblAuthor: TLabel
    Left = 8
    Top = 11
    Width = 67
    Height = 13
    Caption = 'A&uthor Name:'
    FocusControl = editAuthor
  end
  object lblLanguages: TLabel
    Left = 348
    Top = 114
    Width = 52
    Height = 13
    Caption = '&Languages'
    FocusControl = gridLanguages
  end
  object lblBCP47: TLabel
    Left = 8
    Top = 257
    Width = 90
    Height = 13
    Caption = '&Primary Language:'
  end
  object lblUniq: TLabel
    Left = 8
    Top = 284
    Width = 67
    Height = 13
    Caption = 'Uni&que Name:'
    FocusControl = editUniq
  end
  object lblModelName: TLabel
    Left = 8
    Top = 38
    Width = 62
    Height = 13
    Caption = '&Model Name:'
    FocusControl = editModelName
  end
  object Bevel1: TBevel
    Left = 8
    Top = 205
    Width = 317
    Height = 4
  end
  object lblProjectFilename: TLabel
    Left = 8
    Top = 338
    Width = 77
    Height = 13
    Caption = 'Project &filename'
    FocusControl = editProjectFilename
  end
  object Label1: TLabel
    Left = 8
    Top = 92
    Width = 68
    Height = 13
    Caption = 'Fu&ll copyright:'
    FocusControl = editFullCopyright
  end
  object Label2: TLabel
    Left = 347
    Top = 10
    Width = 53
    Height = 13
    Caption = '&Description'
    FocusControl = memoDescription
  end
  object editModelID: TEdit
    Left = 120
    Top = 308
    Width = 205
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 15
    OnChange = editModelIDChange
  end
  object cmdBrowse: TButton
    Left = 120
    Top = 173
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 6
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 146
    Width = 205
    Height = 21
    TabOrder = 5
    OnChange = editPathChange
  end
  object editAuthorID: TEdit
    Left = 120
    Top = 227
    Width = 205
    Height = 21
    TabOrder = 12
    OnChange = editModelIDComponentChange
  end
  object editCopyright: TEdit
    Left = 120
    Top = 62
    Width = 205
    Height = 21
    TabOrder = 2
    OnChange = editCopyrightChange
  end
  object editVersion: TEdit
    Left = 120
    Top = 117
    Width = 205
    Height = 21
    TabOrder = 4
    Text = '1.0'
    OnChange = editVersionChange
  end
  object editAuthor: TEdit
    Left = 120
    Top = 8
    Width = 205
    Height = 21
    TabOrder = 0
    OnChange = editAuthorChange
  end
  object cmdOK: TButton
    Left = 302
    Top = 367
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 17
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 381
    Top = 367
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 18
  end
  object gridLanguages: TStringGrid
    Left = 347
    Top = 135
    Width = 317
    Height = 102
    ColCount = 2
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 9
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 8
    OnClick = gridLanguagesClick
    OnDblClick = gridLanguagesDblClick
    ColWidths = (
      78
      64)
  end
  object cmdAddLanguage: TButton
    Left = 671
    Top = 135
    Width = 73
    Height = 25
    Caption = '&Add...'
    TabOrder = 9
    OnClick = cmdAddLanguageClick
  end
  object cmdEditLanguage: TButton
    Left = 671
    Top = 166
    Width = 73
    Height = 25
    Caption = '&Edit...'
    TabOrder = 10
    OnClick = cmdEditLanguageClick
  end
  object cmdRemoveLanguage: TButton
    Left = 671
    Top = 197
    Width = 73
    Height = 25
    Caption = '&Remove'
    TabOrder = 11
    OnClick = cmdRemoveLanguageClick
  end
  object editUniq: TEdit
    Left = 120
    Top = 281
    Width = 205
    Height = 21
    TabOrder = 14
    OnChange = editModelIDComponentChange
  end
  object cbBCP47: TComboBox
    Left = 120
    Top = 254
    Width = 205
    Height = 21
    Style = csDropDownList
    TabOrder = 13
    OnClick = editModelIDComponentChange
  end
  object editModelName: TEdit
    Left = 120
    Top = 35
    Width = 205
    Height = 21
    TabOrder = 1
    OnChange = editModelNameChange
  end
  object editProjectFilename: TEdit
    Left = 120
    Top = 335
    Width = 284
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 16
  end
  object editFullCopyright: TEdit
    Left = 120
    Top = 89
    Width = 205
    Height = 21
    TabOrder = 3
    OnChange = editFullCopyrightChange
  end
  object memoDescription: TMemo
    Left = 347
    Top = 31
    Width = 398
    Height = 79
    TabOrder = 7
    OnChange = memoDescriptionChange
  end
end
