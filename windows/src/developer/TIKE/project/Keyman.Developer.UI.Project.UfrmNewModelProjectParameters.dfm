inherited frmNewModelProjectParameters: TfrmNewModelProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New Wordlist Lexical Model Project'
  ClientHeight = 484
  ClientWidth = 412
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 418
  ExplicitHeight = 513
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileName: TLabel
    Left = 8
    Top = 395
    Width = 46
    Height = 13
    Caption = 'Model &ID:'
    FocusControl = editModelID
  end
  object lblPath: TLabel
    Left = 8
    Top = 259
    Width = 26
    Height = 13
    Caption = '&Path:'
    FocusControl = editPath
  end
  object lblAuthorID: TLabel
    Left = 8
    Top = 314
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
    Top = 92
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
    Top = 118
    Width = 52
    Height = 13
    Caption = '&Languages'
    FocusControl = gridLanguages
  end
  object lblBCP47: TLabel
    Left = 8
    Top = 341
    Width = 90
    Height = 13
    Caption = '&Primary Language:'
  end
  object lblUniq: TLabel
    Left = 8
    Top = 368
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
    Top = 289
    Width = 397
    Height = 2
  end
  object lblProjectFilename: TLabel
    Left = 8
    Top = 422
    Width = 77
    Height = 13
    Caption = 'Project &filename'
    FocusControl = editProjectFilename
  end
  object editModelID: TEdit
    Left = 120
    Top = 392
    Width = 205
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 13
    OnChange = editModelIDChange
  end
  object cmdBrowse: TButton
    Left = 332
    Top = 256
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 9
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 256
    Width = 205
    Height = 21
    TabOrder = 8
    OnChange = editPathChange
  end
  object editAuthorID: TEdit
    Left = 120
    Top = 311
    Width = 205
    Height = 21
    TabOrder = 10
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
    Top = 89
    Width = 205
    Height = 21
    TabOrder = 3
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
    Top = 451
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 15
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 331
    Top = 451
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 16
  end
  object gridLanguages: TStringGrid
    Left = 8
    Top = 139
    Width = 317
    Height = 102
    ColCount = 2
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 9
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 4
    OnClick = gridLanguagesClick
    OnDblClick = gridLanguagesDblClick
    ColWidths = (
      78
      64)
  end
  object cmdAddLanguage: TButton
    Left = 332
    Top = 139
    Width = 73
    Height = 25
    Caption = '&Add...'
    TabOrder = 5
    OnClick = cmdAddLanguageClick
  end
  object cmdEditLanguage: TButton
    Left = 332
    Top = 170
    Width = 73
    Height = 25
    Caption = '&Edit...'
    TabOrder = 6
    OnClick = cmdEditLanguageClick
  end
  object cmdRemoveLanguage: TButton
    Left = 332
    Top = 201
    Width = 73
    Height = 25
    Caption = '&Remove'
    TabOrder = 7
    OnClick = cmdRemoveLanguageClick
  end
  object editUniq: TEdit
    Left = 120
    Top = 365
    Width = 205
    Height = 21
    TabOrder = 12
    OnChange = editModelIDComponentChange
  end
  object cbBCP47: TComboBox
    Left = 120
    Top = 338
    Width = 205
    Height = 21
    Style = csDropDownList
    TabOrder = 11
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
    Top = 419
    Width = 284
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 14
  end
  object dlgBrowse: TBrowse4Folder
    InitialDir = 'C:\Users\mcdurdin\Desktop\'
    Options = [OnlySelectFileSysAncestors, ShowEditBox, UseNewDialogStyle]
    Root = Desktop
    Title = 'Select folder to save project to'
    Left = 168
    Top = 168
  end
end
