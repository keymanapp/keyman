inherited frmNewModelProjectParameters: TfrmNewModelProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New Wordlist Lexical Model Project'
  ClientHeight = 607
  ClientWidth = 684
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 690
  ExplicitHeight = 636
  PixelsPerInch = 96
  TextHeight = 13
  object lblModelID: TLabel
    Left = 9
    Top = 519
    Width = 46
    Height = 13
    Caption = 'Model &ID:'
    FocusControl = editModelID
  end
  object lblPath: TLabel
    Left = 8
    Top = 389
    Width = 26
    Height = 13
    Caption = '&Path:'
    FocusControl = editPath
  end
  object lblAuthorID: TLabel
    Left = 9
    Top = 438
    Width = 51
    Height = 13
    Caption = 'Aut&hor ID:'
    FocusControl = editAuthorID
  end
  object lblCopyright: TLabel
    Left = 8
    Top = 177
    Width = 51
    Height = 13
    Caption = '&Copyright:'
    FocusControl = editCopyright
  end
  object lblVersion: TLabel
    Left = 8
    Top = 232
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
    Left = 8
    Top = 259
    Width = 52
    Height = 13
    Caption = '&Languages'
    FocusControl = gridLanguages
  end
  object lblBCP47: TLabel
    Left = 9
    Top = 465
    Width = 90
    Height = 13
    Caption = '&Primary Language:'
  end
  object lblUniq: TLabel
    Left = 9
    Top = 492
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
    Top = 421
    Width = 405
    Height = 2
  end
  object lblProjectFilename: TLabel
    Left = 9
    Top = 546
    Width = 77
    Height = 13
    Caption = 'Project &filename'
    FocusControl = editProjectFilename
  end
  object lblFullCopyright: TLabel
    Left = 8
    Top = 204
    Width = 68
    Height = 13
    Caption = 'Fu&ll copyright:'
    FocusControl = editFullCopyright
  end
  object lblDescription: TLabel
    Left = 8
    Top = 68
    Width = 53
    Height = 13
    Caption = '&Description'
    FocusControl = memoDescription
  end
  object lblDescriptionMarkdown: TLabel
    Left = 120
    Top = 150
    Width = 197
    Height = 13
    Caption = 'Markdown accepted, no embedded HTML'
    Transparent = True
  end
  object lblModelNameValidation: TLabel
    Left = 426
    Top = 38
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    WordWrap = True
  end
  object lblDescriptionValidation: TLabel
    Left = 426
    Top = 68
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = memoDescription
    WordWrap = True
  end
  object lblCopyrightValidation: TLabel
    Left = 426
    Top = 180
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editCopyright
    WordWrap = True
  end
  object lblVersionValidation: TLabel
    Left = 426
    Top = 233
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editVersion
    WordWrap = True
  end
  object lblAuthorValidation: TLabel
    Left = 426
    Top = 11
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editAuthor
    WordWrap = True
  end
  object lblFullCopyrightValidation: TLabel
    Left = 426
    Top = 206
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editFullCopyright
    WordWrap = True
  end
  object lblLanguagesValidation: TLabel
    Left = 426
    Top = 259
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    WordWrap = True
  end
  object lblPathValidation: TLabel
    Left = 426
    Top = 389
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editPath
    WordWrap = True
  end
  object lblModelIDValidation: TLabel
    Left = 426
    Top = 519
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    WordWrap = True
  end
  object lblProjectFilenameValidation: TLabel
    Left = 426
    Top = 546
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editProjectFilename
    WordWrap = True
  end
  object lblAuthorIDValidation: TLabel
    Left = 426
    Top = 438
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editPath
    WordWrap = True
  end
  object lblBCP47Validation: TLabel
    Left = 426
    Top = 465
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editPath
    WordWrap = True
  end
  object lblUniqValidation: TLabel
    Left = 426
    Top = 492
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editPath
    WordWrap = True
  end
  object editModelID: TEdit
    Left = 121
    Top = 516
    Width = 292
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 15
    OnChange = editModelIDChange
  end
  object cmdBrowse: TButton
    Left = 340
    Top = 386
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 11
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 386
    Width = 214
    Height = 21
    TabOrder = 10
    OnChange = editPathChange
    OnExit = ControlExit
  end
  object editAuthorID: TEdit
    Left = 121
    Top = 435
    Width = 292
    Height = 21
    TabOrder = 12
    OnChange = editModelIDComponentChange
    OnExit = ControlExit
  end
  object editCopyright: TEdit
    Left = 120
    Top = 174
    Width = 293
    Height = 21
    TabOrder = 3
    OnChange = editCopyrightChange
    OnExit = ControlExit
  end
  object editVersion: TEdit
    Left = 120
    Top = 229
    Width = 293
    Height = 21
    TabOrder = 5
    Text = '1.0'
    OnChange = editVersionChange
    OnExit = ControlExit
  end
  object editAuthor: TEdit
    Left = 120
    Top = 8
    Width = 293
    Height = 21
    TabOrder = 0
    OnChange = editAuthorChange
    OnExit = ControlExit
  end
  object cmdOK: TButton
    Left = 120
    Top = 574
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 17
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 199
    Top = 574
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 18
  end
  object gridLanguages: TStringGrid
    Left = 121
    Top = 259
    Width = 292
    Height = 81
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
    Left = 121
    Top = 346
    Width = 73
    Height = 25
    Caption = '&Add...'
    TabOrder = 7
    OnClick = cmdAddLanguageClick
    OnExit = cmdAddLanguageExit
  end
  object cmdEditLanguage: TButton
    Left = 200
    Top = 346
    Width = 73
    Height = 25
    Caption = '&Edit...'
    TabOrder = 8
    OnClick = cmdEditLanguageClick
  end
  object cmdRemoveLanguage: TButton
    Left = 279
    Top = 346
    Width = 73
    Height = 25
    Caption = '&Remove'
    TabOrder = 9
    OnClick = cmdRemoveLanguageClick
  end
  object editUniq: TEdit
    Left = 121
    Top = 489
    Width = 292
    Height = 21
    TabOrder = 14
    OnChange = editModelIDComponentChange
    OnExit = ControlExit
  end
  object cbBCP47: TComboBox
    Left = 121
    Top = 462
    Width = 292
    Height = 21
    Style = csDropDownList
    TabOrder = 13
    OnClick = editModelIDComponentChange
    OnExit = ControlExit
  end
  object editModelName: TEdit
    Left = 120
    Top = 35
    Width = 293
    Height = 21
    TabOrder = 1
    OnChange = editModelNameChange
    OnExit = ControlExit
  end
  object editProjectFilename: TEdit
    Left = 121
    Top = 543
    Width = 292
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 16
  end
  object editFullCopyright: TEdit
    Left = 120
    Top = 201
    Width = 293
    Height = 21
    TabOrder = 4
    OnChange = editFullCopyrightChange
    OnExit = ControlExit
  end
  object memoDescription: TMemo
    Left = 120
    Top = 65
    Width = 293
    Height = 79
    TabOrder = 2
    OnChange = memoDescriptionChange
    OnExit = ControlExit
  end
end
