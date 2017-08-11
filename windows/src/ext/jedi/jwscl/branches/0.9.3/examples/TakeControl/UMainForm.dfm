object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'TakeControl over your files'
  ClientHeight = 458
  ClientWidth = 475
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    475
    458)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelStatus: TLabel
    Left = 24
    Top = 176
    Width = 56
    Height = 13
    Caption = 'LabelStatus'
  end
  object LabelErrors: TLabel
    Left = 352
    Top = 134
    Width = 33
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Errors:'
  end
  object LabelFileCount: TLabel
    Left = 352
    Top = 153
    Width = 25
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Files:'
  end
  object Label1: TLabel
    Left = 26
    Top = 15
    Width = 164
    Height = 13
    AutoSize = False
    Caption = 'Add one extension separated by ;'
  end
  object JvWaitingGradient1: TJvWaitingGradient
    Left = 26
    Top = 160
    Width = 73
    GradientWidth = 50
  end
  object Label2: TLabel
    Left = 24
    Top = 199
    Width = 91
    Height = 13
    Caption = 'File copy progress:'
  end
  object EditSource: TEdit
    Left = 23
    Top = 42
    Width = 349
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'EditSource'
    ExplicitWidth = 393
  end
  object ButtonBrowseSource: TButton
    Left = 387
    Top = 40
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 1
    OnClick = ButtonBrowseSourceClick
    ExplicitLeft = 422
  end
  object ButtonBrowseTarget: TButton
    Left = 387
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Browse'
    TabOrder = 2
    OnClick = ButtonBrowseTargetClick
    ExplicitLeft = 422
  end
  object EditTarget: TEdit
    Left = 23
    Top = 98
    Width = 349
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'Edit1'
    ExplicitWidth = 393
  end
  object MemoStatus: TMemo
    Left = 24
    Top = 222
    Width = 440
    Height = 187
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WordWrap = False
  end
  object ButtonRun: TButton
    Left = 24
    Top = 125
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 5
    OnClick = ButtonRunClick
  end
  object EditMask: TEdit
    Left = 196
    Top = 8
    Width = 266
    Height = 21
    Hint = 
      'Set some file extentions separated by semicolon ";" like .mpg;.w' +
      'mv'
    Anchors = [akLeft, akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    Text = '.mpg;.wmv'
  end
  object ProgressBar1: TProgressBar
    Left = 121
    Top = 199
    Width = 346
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
  end
  object CheckBoxCopy: TCheckBox
    Left = 24
    Top = 69
    Width = 97
    Height = 17
    Hint = 
      'If checked, the files won'#39't be moved but just copied to the targ' +
      'et. Security Descriptors of source files won'#39't be changed.'
    Caption = 'Copy files only'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
  end
  object CheckBoxListOnly: TCheckBox
    Left = 105
    Top = 129
    Width = 176
    Height = 17
    Hint = 
      'If checked, t|If checked, the Run won'#39't change anything! It just' +
      ' counts the number of files found.'
    Caption = 'List Run Only (no modification)'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 9
    OnClick = CheckBoxListOnlyClick
  end
  object Button1: TButton
    Left = 387
    Top = 425
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Info'
    TabOrder = 10
    OnClick = Button1Click
  end
  object JvBrowseForFolderDialog1: TJvBrowseForFolderDialog
    Options = [odOnlyDirectory, odStatusAvailable, odEditBox, odNewDialogStyle, odValidate]
    Left = 312
    Top = 16
  end
end
