inherited frmNewLDMLKeyboardProjectParameters: TfrmNewLDMLKeyboardProjectParameters
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New LDML Keyboard Project'
  ClientHeight = 465
  ClientWidth = 625
  Position = poScreenCenter
  ExplicitWidth = 631
  ExplicitHeight = 494
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileName: TLabel
    Left = 12
    Top = 406
    Width = 64
    Height = 13
    Caption = '&Keyboard ID:'
    FocusControl = editKeyboardID
  end
  object lblPath: TLabel
    Left = 12
    Top = 379
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
    Left = 419
    Top = 260
    Width = 145
    Height = 26
    Caption = '(one BCP 47 tag per line, first tag is primary)'
    WordWrap = True
  end
  object lblProjectFilename: TLabel
    Left = 12
    Top = 433
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
  object Label3: TLabel
    Left = 12
    Top = 260
    Width = 102
    Height = 13
    Caption = 'Supported &languages'
    FocusControl = memoLanguages
  end
  object Label4: TLabel
    Left = 331
    Top = 233
    Width = 232
    Height = 13
    Caption = 'Semantic version: major.minor.patch, e.g. 1.0.1'
  end
  object editKeyboardID: TEdit
    Left = 120
    Top = 403
    Width = 205
    Height = 21
    TabOrder = 9
    OnChange = editKeyboardIDChange
  end
  object cmdBrowse: TButton
    Left = 340
    Top = 376
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 8
    OnClick = cmdBrowseClick
  end
  object editPath: TEdit
    Left = 120
    Top = 376
    Width = 205
    Height = 21
    TabOrder = 7
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
    Text = '1.0.0'
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
    Top = 428
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 11
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 542
    Top = 428
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 12
  end
  object editProjectFilename: TEdit
    Left = 120
    Top = 430
    Width = 293
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 10
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
  object memoLanguages: TMemo
    Left = 120
    Top = 257
    Width = 293
    Height = 113
    TabOrder = 6
    OnChange = memoLanguagesChange
  end
end
