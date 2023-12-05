inherited frmNewProjectParameters: TfrmNewProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New Basic Keyboard Project'
  ClientHeight = 444
  ClientWidth = 625
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 631
  ExplicitHeight = 473
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileName: TLabel
    Left = 12
    Top = 390
    Width = 64
    Height = 13
    Caption = '&Keyboard ID:'
    FocusControl = editKeyboardID
  end
  object lblPath: TLabel
    Left = 12
    Top = 363
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
    Top = 180
    Width = 51
    Height = 13
    Caption = '&Copyright:'
    FocusControl = editCopyright
  end
  object lblVersion: TLabel
    Left = 12
    Top = 233
    Width = 39
    Height = 13
    Caption = '&Version:'
    FocusControl = editVersion
  end
  object lblAuthor: TLabel
    Left = 12
    Top = 153
    Width = 37
    Height = 13
    Caption = 'A&uthor:'
    FocusControl = editAuthor
  end
  object lblTargets: TLabel
    Left = 12
    Top = 260
    Width = 41
    Height = 13
    Caption = '&Targets:'
    FocusControl = clbTargets
  end
  object lblKeyboardLanguages: TLabel
    Left = 339
    Top = 152
    Width = 52
    Height = 13
    Caption = '&Languages'
    FocusControl = gridKeyboardLanguages
  end
  object lblProjectFilename: TLabel
    Left = 12
    Top = 417
    Width = 77
    Height = 13
    Caption = 'Project &filename'
    FocusControl = editProjectFilename
  end
  object Label1: TLabel
    Left = 12
    Top = 206
    Width = 68
    Height = 13
    Caption = 'Fu&ll copyright:'
    FocusControl = editFullCopyright
  end
  object Label2: TLabel
    Left = 12
    Top = 38
    Width = 53
    Height = 13
    Caption = '&Description'
    FocusControl = memoDescription
  end
  object editKeyboardID: TEdit
    Left = 120
    Top = 387
    Width = 205
    Height = 21
    TabOrder = 13
    OnChange = editKeyboardIDChange
  end
  object cmdBrowse: TButton
    Left = 340
    Top = 360
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 12
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 360
    Width = 205
    Height = 21
    TabOrder = 11
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
    Top = 176
    Width = 205
    Height = 21
    TabOrder = 3
    Text = 'Copyright '#169
    OnChange = editCopyrightChange
  end
  object editVersion: TEdit
    Left = 120
    Top = 230
    Width = 205
    Height = 21
    TabOrder = 5
    Text = '1.0'
    OnChange = editVersionChange
  end
  object editAuthor: TEdit
    Left = 120
    Top = 150
    Width = 205
    Height = 21
    TabOrder = 2
    OnChange = editAuthorChange
  end
  object cmdOK: TButton
    Left = 463
    Top = 412
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 15
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 542
    Top = 412
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 16
  end
  object clbTargets: TCheckListBox
    Left = 120
    Top = 257
    Width = 205
    Height = 97
    OnClickCheck = clbTargetsClickCheck
    ItemHeight = 13
    TabOrder = 6
  end
  object gridKeyboardLanguages: TStringGrid
    Left = 339
    Top = 173
    Width = 278
    Height = 120
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
    Left = 340
    Top = 299
    Width = 73
    Height = 25
    Caption = '&Add...'
    TabOrder = 8
    OnClick = cmdKeyboardAddLanguageClick
  end
  object cmdKeyboardEditLanguage: TButton
    Left = 419
    Top = 299
    Width = 73
    Height = 25
    Caption = 'Ed&it...'
    TabOrder = 9
    OnClick = cmdKeyboardEditLanguageClick
  end
  object cmdKeyboardRemoveLanguage: TButton
    Left = 498
    Top = 299
    Width = 72
    Height = 25
    Caption = '&Remove'
    TabOrder = 10
    OnClick = cmdKeyboardRemoveLanguageClick
  end
  object editProjectFilename: TEdit
    Left = 120
    Top = 414
    Width = 293
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 14
    OnChange = editKeyboardIDChange
  end
  object editFullCopyright: TEdit
    Left = 120
    Top = 203
    Width = 205
    Height = 21
    TabOrder = 4
    Text = 'Copyright '#169' YYYY'
    OnChange = editFullCopyrightChange
  end
  object memoDescription: TMemo
    Left = 120
    Top = 35
    Width = 497
    Height = 109
    TabOrder = 1
    OnChange = memoDescriptionChange
  end
end
