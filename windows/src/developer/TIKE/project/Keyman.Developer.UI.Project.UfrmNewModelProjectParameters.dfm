inherited frmNewModelProjectParameters: TfrmNewModelProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New Wordlist Lexical Model Project'
  ClientHeight = 510
  ClientWidth = 412
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitTop = -8
  ExplicitWidth = 418
  ExplicitHeight = 539
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileName: TLabel
    Left = 8
    Top = 423
    Width = 46
    Height = 13
    Caption = 'Model &ID:'
    FocusControl = editModelID
  end
  object lblPath: TLabel
    Left = 8
    Top = 287
    Width = 26
    Height = 13
    Caption = '&Path:'
    FocusControl = editPath
  end
  object lblAuthorID: TLabel
    Left = 8
    Top = 342
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
    Left = 9
    Top = 146
    Width = 52
    Height = 13
    Caption = '&Languages'
    FocusControl = gridLanguages
  end
  object lblBCP47: TLabel
    Left = 8
    Top = 369
    Width = 90
    Height = 13
    Caption = '&Primary Language:'
  end
  object lblUniq: TLabel
    Left = 8
    Top = 396
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
    Top = 317
    Width = 397
    Height = 2
  end
  object lblProjectFilename: TLabel
    Left = 8
    Top = 450
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
  object editModelID: TEdit
    Left = 120
    Top = 420
    Width = 205
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 14
    OnChange = editModelIDChange
  end
  object cmdBrowse: TButton
    Left = 332
    Top = 284
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 10
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 284
    Width = 205
    Height = 21
    TabOrder = 9
    OnChange = editPathChange
  end
  object editAuthorID: TEdit
    Left = 120
    Top = 339
    Width = 205
    Height = 21
    TabOrder = 11
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
    Left = 252
    Top = 479
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 16
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 331
    Top = 479
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 17
  end
  object gridLanguages: TStringGrid
    Left = 8
    Top = 167
    Width = 317
    Height = 102
    ColCount = 2
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 9
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 5
    OnClick = gridLanguagesClick
    OnDblClick = gridLanguagesDblClick
    ColWidths = (
      78
      64)
  end
  object cmdAddLanguage: TButton
    Left = 332
    Top = 167
    Width = 73
    Height = 25
    Caption = '&Add...'
    TabOrder = 6
    OnClick = cmdAddLanguageClick
  end
  object cmdEditLanguage: TButton
    Left = 332
    Top = 198
    Width = 73
    Height = 25
    Caption = '&Edit...'
    TabOrder = 7
    OnClick = cmdEditLanguageClick
  end
  object cmdRemoveLanguage: TButton
    Left = 332
    Top = 229
    Width = 73
    Height = 25
    Caption = '&Remove'
    TabOrder = 8
    OnClick = cmdRemoveLanguageClick
  end
  object editUniq: TEdit
    Left = 120
    Top = 393
    Width = 205
    Height = 21
    TabOrder = 13
    OnChange = editModelIDComponentChange
  end
  object cbBCP47: TComboBox
    Left = 120
    Top = 366
    Width = 205
    Height = 21
    Style = csDropDownList
    TabOrder = 12
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
    Top = 447
    Width = 284
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 15
  end
  object editFullCopyright: TEdit
    Left = 120
    Top = 89
    Width = 205
    Height = 21
    TabOrder = 3
    OnChange = editFullCopyrightChange
  end
end
