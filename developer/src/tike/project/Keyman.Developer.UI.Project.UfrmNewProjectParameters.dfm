inherited frmNewProjectParameters: TfrmNewProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New Basic Keyboard Project'
  ClientHeight = 582
  ClientWidth = 693
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 699
  ExplicitHeight = 611
  PixelsPerInch = 96
  TextHeight = 13
  object lblKeyboardID: TLabel
    Left = 12
    Top = 496
    Width = 64
    Height = 13
    Caption = '&Keyboard ID:'
    FocusControl = editKeyboardID
  end
  object lblPath: TLabel
    Left = 12
    Top = 469
    Width = 26
    Height = 13
    Caption = '&Path:'
    FocusControl = editPath
  end
  object lblKeyboardName: TLabel
    Left = 12
    Top = 11
    Width = 80
    Height = 13
    Caption = 'Keyboard &Name:'
    FocusControl = editKeyboardName
  end
  object lblCoypright: TLabel
    Left = 12
    Top = 150
    Width = 51
    Height = 13
    Caption = '&Copyright:'
    FocusControl = editCopyright
  end
  object lblVersion: TLabel
    Left = 12
    Top = 203
    Width = 39
    Height = 13
    Caption = '&Version:'
    FocusControl = editVersion
  end
  object lblAuthor: TLabel
    Left = 12
    Top = 123
    Width = 37
    Height = 13
    Caption = 'A&uthor:'
    FocusControl = editAuthor
  end
  object lblTargets: TLabel
    Left = 12
    Top = 230
    Width = 41
    Height = 13
    Caption = '&Targets:'
    FocusControl = clbTargets
  end
  object lblKeyboardLanguages: TLabel
    Left = 12
    Top = 332
    Width = 52
    Height = 13
    Caption = '&Languages'
    FocusControl = gridKeyboardLanguages
  end
  object lblProjectFilename: TLabel
    Left = 12
    Top = 523
    Width = 77
    Height = 13
    Caption = 'Project &filename'
    FocusControl = editProjectFilename
  end
  object lblFullCopyright: TLabel
    Left = 12
    Top = 176
    Width = 68
    Height = 13
    Caption = 'Fu&ll copyright:'
    FocusControl = editFullCopyright
  end
  object lblDescription: TLabel
    Left = 12
    Top = 38
    Width = 53
    Height = 13
    Caption = '&Description'
    FocusControl = memoDescription
  end
  object lblKeyboardNameValidation: TLabel
    Left = 424
    Top = 11
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editKeyboardName
    WordWrap = True
  end
  object lblDescriptionValidation: TLabel
    Left = 424
    Top = 38
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = memoDescription
    WordWrap = True
  end
  object lblKeyboardIDValidation: TLabel
    Left = 424
    Top = 496
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editKeyboardID
    WordWrap = True
  end
  object lblPathValidation: TLabel
    Left = 424
    Top = 469
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editPath
    WordWrap = True
  end
  object lblCopyrightValidation: TLabel
    Left = 424
    Top = 150
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editCopyright
    WordWrap = True
  end
  object lblVersionValidation: TLabel
    Left = 424
    Top = 203
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editVersion
    WordWrap = True
  end
  object lblAuthorValidation: TLabel
    Left = 424
    Top = 123
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editAuthor
    WordWrap = True
  end
  object lblTargetsValidation: TLabel
    Left = 424
    Top = 230
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = clbTargets
    WordWrap = True
  end
  object lblLanguagesValidation: TLabel
    Left = 424
    Top = 332
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = gridKeyboardLanguages
    WordWrap = True
  end
  object lblProjectFilenameValidation: TLabel
    Left = 424
    Top = 523
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editProjectFilename
    WordWrap = True
  end
  object lblFullCopyrightValidation: TLabel
    Left = 424
    Top = 176
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editFullCopyright
    WordWrap = True
  end
  object lblDescriptionMarkdown: TLabel
    Left = 121
    Top = 101
    Width = 197
    Height = 13
    Caption = 'Markdown accepted, no embedded HTML'
    Transparent = True
  end
  object editKeyboardID: TEdit
    Left = 120
    Top = 493
    Width = 293
    Height = 21
    TabOrder = 13
    OnChange = editKeyboardIDChange
    OnExit = ControlExit
  end
  object cmdBrowse: TButton
    Left = 340
    Top = 466
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 12
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 466
    Width = 205
    Height = 21
    TabOrder = 11
    OnChange = editPathChange
    OnExit = ControlExit
  end
  object editKeyboardName: TEdit
    Left = 120
    Top = 8
    Width = 293
    Height = 21
    TabOrder = 0
    OnChange = editKeyboardNameChange
    OnExit = ControlExit
  end
  object editCopyright: TEdit
    Left = 120
    Top = 146
    Width = 293
    Height = 21
    TabOrder = 3
    Text = 'Copyright '#169
    OnChange = editCopyrightChange
    OnExit = ControlExit
  end
  object editVersion: TEdit
    Left = 120
    Top = 200
    Width = 293
    Height = 21
    TabOrder = 5
    Text = '1.0'
    OnChange = editVersionChange
    OnExit = ControlExit
  end
  object editAuthor: TEdit
    Left = 120
    Top = 120
    Width = 293
    Height = 21
    TabOrder = 2
    OnChange = editAuthorChange
    OnExit = ControlExit
  end
  object cmdOK: TButton
    Left = 120
    Top = 549
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 15
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 199
    Top = 549
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 16
  end
  object clbTargets: TCheckListBox
    Left = 120
    Top = 227
    Width = 293
    Height = 97
    OnClickCheck = clbTargetsClickCheck
    ItemHeight = 13
    TabOrder = 6
    OnExit = ControlExit
  end
  object gridKeyboardLanguages: TStringGrid
    Left = 120
    Top = 330
    Width = 293
    Height = 94
    ColCount = 2
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 9
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 7
    OnClick = gridKeyboardLanguagesClick
    OnDblClick = gridKeyboardLanguagesDblClick
    ColWidths = (
      78
      64)
  end
  object cmdKeyboardAddLanguage: TButton
    Left = 120
    Top = 430
    Width = 73
    Height = 25
    Caption = '&Add...'
    TabOrder = 8
    OnClick = cmdKeyboardAddLanguageClick
  end
  object cmdKeyboardEditLanguage: TButton
    Left = 199
    Top = 430
    Width = 73
    Height = 25
    Caption = 'Ed&it...'
    TabOrder = 9
    OnClick = cmdKeyboardEditLanguageClick
  end
  object cmdKeyboardRemoveLanguage: TButton
    Left = 278
    Top = 430
    Width = 72
    Height = 25
    Caption = '&Remove'
    TabOrder = 10
    OnClick = cmdKeyboardRemoveLanguageClick
  end
  object editProjectFilename: TEdit
    Left = 120
    Top = 520
    Width = 293
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 14
  end
  object editFullCopyright: TEdit
    Left = 120
    Top = 173
    Width = 293
    Height = 21
    TabOrder = 4
    Text = 'Copyright '#169' YYYY'
    OnChange = editFullCopyrightChange
    OnExit = ControlExit
  end
  object memoDescription: TMemo
    Left = 120
    Top = 35
    Width = 293
    Height = 62
    TabOrder = 1
    OnChange = memoDescriptionChange
    OnExit = ControlExit
  end
end
