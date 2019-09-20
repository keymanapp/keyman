inherited frmNewProjectParameters: TfrmNewProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New Basic Keyboard Project'
  ClientHeight = 330
  ClientWidth = 625
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 631
  ExplicitHeight = 359
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileName: TLabel
    Left = 12
    Top = 275
    Width = 64
    Height = 13
    Caption = '&Keyboard ID:'
    FocusControl = editKeyboardID
  end
  object lblPath: TLabel
    Left = 12
    Top = 248
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
    Top = 38
    Width = 51
    Height = 13
    Caption = '&Copyright:'
    FocusControl = editCopyright
  end
  object lblVersion: TLabel
    Left = 12
    Top = 65
    Width = 39
    Height = 13
    Caption = '&Version:'
    FocusControl = editVersion
  end
  object lblAuthor: TLabel
    Left = 12
    Top = 92
    Width = 37
    Height = 13
    Caption = 'A&uthor:'
    FocusControl = editAuthor
  end
  object lblTargets: TLabel
    Left = 12
    Top = 119
    Width = 41
    Height = 13
    Caption = '&Targets:'
    FocusControl = clbTargets
  end
  object lblKeyboardLanguages: TLabel
    Left = 339
    Top = 11
    Width = 52
    Height = 13
    Caption = '&Languages'
    FocusControl = gridKeyboardLanguages
  end
  object lblProjectFilename: TLabel
    Left = 12
    Top = 302
    Width = 77
    Height = 13
    Caption = 'Project &filename'
    FocusControl = editProjectFilename
  end
  object editKeyboardID: TEdit
    Left = 120
    Top = 272
    Width = 205
    Height = 21
    TabOrder = 11
    OnChange = editKeyboardIDChange
  end
  object cmdBrowse: TButton
    Left = 340
    Top = 245
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 10
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 245
    Width = 205
    Height = 21
    TabOrder = 9
    OnChange = editPathChange
  end
  object editKeyboardName: TEdit
    Left = 120
    Top = 8
    Width = 205
    Height = 21
    TabOrder = 0
    OnChange = editKeyboardNameChange
  end
  object editCopyright: TEdit
    Left = 120
    Top = 35
    Width = 205
    Height = 21
    TabOrder = 1
    Text = #169
    OnChange = editCopyrightChange
  end
  object editVersion: TEdit
    Left = 120
    Top = 62
    Width = 205
    Height = 21
    TabOrder = 2
    Text = '1.0'
    OnChange = editVersionChange
  end
  object editAuthor: TEdit
    Left = 120
    Top = 89
    Width = 205
    Height = 21
    TabOrder = 3
    OnChange = editAuthorChange
  end
  object cmdOK: TButton
    Left = 463
    Top = 297
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 13
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 542
    Top = 297
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 14
  end
  object clbTargets: TCheckListBox
    Left = 120
    Top = 116
    Width = 205
    Height = 97
    OnClickCheck = clbTargetsClickCheck
    ItemHeight = 13
    TabOrder = 4
  end
  object gridKeyboardLanguages: TStringGrid
    Left = 339
    Top = 32
    Width = 278
    Height = 153
    ColCount = 2
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 9
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 5
    OnClick = gridKeyboardLanguagesClick
    OnDblClick = gridKeyboardLanguagesDblClick
    ColWidths = (
      78
      64)
  end
  object cmdKeyboardAddLanguage: TButton
    Left = 340
    Top = 191
    Width = 73
    Height = 25
    Caption = '&Add...'
    TabOrder = 6
    OnClick = cmdKeyboardAddLanguageClick
  end
  object cmdKeyboardEditLanguage: TButton
    Left = 419
    Top = 191
    Width = 73
    Height = 25
    Caption = 'Ed&it...'
    TabOrder = 7
    OnClick = cmdKeyboardEditLanguageClick
  end
  object cmdKeyboardRemoveLanguage: TButton
    Left = 498
    Top = 191
    Width = 72
    Height = 25
    Caption = '&Remove'
    TabOrder = 8
    OnClick = cmdKeyboardRemoveLanguageClick
  end
  object editProjectFilename: TEdit
    Left = 120
    Top = 299
    Width = 293
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 12
    OnChange = editKeyboardIDChange
  end
  object dlgBrowse: TBrowse4Folder
    Options = [OnlySelectFileSysDir, ShowEditBox, UseNewDialogStyle]
    Root = Desktop
    Title = 'Select folder to save project to'
    Left = 56
    Top = 168
  end
end
