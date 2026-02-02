inherited frmNewLDMLKeyboardProjectParameters: TfrmNewLDMLKeyboardProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New LDML Keyboard Project'
  ClientHeight = 520
  ClientWidth = 721
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 727
  ExplicitHeight = 549
  PixelsPerInch = 96
  TextHeight = 13
  object lblKeyboardID: TLabel
    Left = 12
    Top = 430
    Width = 64
    Height = 13
    Caption = '&Keyboard ID:'
    FocusControl = editKeyboardID
  end
  object lblPath: TLabel
    Left = 12
    Top = 403
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
  object lblKeyboardLanguages: TLabel
    Left = 120
    Top = 381
    Width = 265
    Height = 26
    Caption = '(one BCP 47 tag per line, first tag is primary)'
    WordWrap = True
  end
  object lblProjectFilename: TLabel
    Left = 12
    Top = 457
    Width = 77
    Height = 13
    Caption = 'Project &filename'
    FocusControl = editProjectFilename
  end
  object lblFullCopyright: TLabel
    Left = 12
    Top = 206
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
  object lblLanguages: TLabel
    Left = 12
    Top = 284
    Width = 102
    Height = 13
    Caption = '&Supported languages'
    FocusControl = memoLanguages
  end
  object Label4: TLabel
    Left = 120
    Top = 256
    Width = 232
    Height = 13
    Caption = 'Semantic version: major.minor.patch, e.g. 1.0.1'
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
  object lblCopyrightValidation: TLabel
    Left = 424
    Top = 180
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editCopyright
    WordWrap = True
  end
  object lblVersionValidation: TLabel
    Left = 424
    Top = 233
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editVersion
    WordWrap = True
  end
  object lblAuthorValidation: TLabel
    Left = 424
    Top = 153
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editAuthor
    WordWrap = True
  end
  object lblFullCopyrightValidation: TLabel
    Left = 424
    Top = 206
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editFullCopyright
    WordWrap = True
  end
  object lblLanguagesValidation: TLabel
    Left = 424
    Top = 284
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    WordWrap = True
  end
  object lblPathValidation: TLabel
    Left = 424
    Top = 403
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editPath
    WordWrap = True
  end
  object lblKeyboardIDValidation: TLabel
    Left = 424
    Top = 430
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editKeyboardID
    WordWrap = True
  end
  object lblProjectFilenameValidation: TLabel
    Left = 424
    Top = 457
    Width = 250
    Height = 26
    AutoSize = False
    Caption = '(validation)'
    FocusControl = editProjectFilename
    WordWrap = True
  end
  object lblDescriptionMarkdown: TLabel
    Left = 120
    Top = 123
    Width = 197
    Height = 13
    Caption = 'Markdown accepted, no embedded HTML'
    Transparent = True
  end
  object editKeyboardID: TEdit
    Left = 120
    Top = 427
    Width = 293
    Height = 21
    TabOrder = 9
    OnChange = editKeyboardIDChange
    OnExit = ControlExit
  end
  object cmdBrowse: TButton
    Left = 340
    Top = 400
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 8
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 400
    Width = 205
    Height = 21
    TabOrder = 7
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
    Top = 176
    Width = 293
    Height = 21
    TabOrder = 3
    Text = 'Copyright '#169
    OnChange = editCopyrightChange
    OnExit = ControlExit
  end
  object editVersion: TEdit
    Left = 120
    Top = 230
    Width = 293
    Height = 21
    TabOrder = 5
    Text = '1.0.0'
    OnChange = editVersionChange
    OnExit = ControlExit
  end
  object editAuthor: TEdit
    Left = 120
    Top = 150
    Width = 293
    Height = 21
    TabOrder = 2
    OnChange = editAuthorChange
    OnExit = ControlExit
  end
  object cmdOK: TButton
    Left = 120
    Top = 487
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 11
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 199
    Top = 487
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 12
  end
  object editProjectFilename: TEdit
    Left = 120
    Top = 454
    Width = 293
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 10
  end
  object editFullCopyright: TEdit
    Left = 120
    Top = 203
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
    Height = 82
    TabOrder = 1
    OnChange = memoDescriptionChange
    OnExit = ControlExit
  end
  object memoLanguages: TMemo
    Left = 120
    Top = 279
    Width = 293
    Height = 96
    TabOrder = 6
    OnChange = memoLanguagesChange
    OnExit = ControlExit
  end
end
