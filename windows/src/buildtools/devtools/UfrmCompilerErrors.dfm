object CompilerErrorsForm: TCompilerErrorsForm
  Left = 216
  Top = 229
  ActiveControl = gridErrors
  Caption = 'Compile Errors'
  ClientHeight = 387
  ClientWidth = 740
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    740
    387)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileOpenRetry: TLabel
    Left = 378
    Top = 365
    Width = 3
    Height = 13
    Alignment = taRightJustify
  end
  object gridErrors: TStringGrid
    Left = 7
    Top = 7
    Width = 733
    Height = 343
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 4
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 2
    Options = [goColSizing, goRowSelect]
    TabOrder = 0
    OnClick = gridErrorsClick
    OnDblClick = gridErrorsDblClick
    OnDrawCell = gridErrorsDrawCell
    OnKeyDown = gridErrorsKeyDown
    ColWidths = (
      64
      64
      64
      64)
    RowHeights = (
      16
      16)
  end
  object chkContinueBuild: TCheckBox
    Left = 7
    Top = 364
    Width = 105
    Height = 16
    Alignment = taLeftJustify
    Anchors = [akLeft, akBottom]
    Caption = '&Continue Build'
    TabOrder = 1
  end
  object cmdClose: TButton
    Left = 665
    Top = 360
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = cmdCloseClick
  end
  object cmdRebuild: TButton
    Left = 579
    Top = 360
    Width = 76
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = '&Rebuild'
    TabOrder = 2
    OnClick = cmdRebuildClick
  end
  object cmdViewErrorSource: TButton
    Left = 468
    Top = 360
    Width = 101
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = '&View error source'
    Default = True
    TabOrder = 4
    OnClick = gridErrorsDblClick
  end
  object cmdCancelFileOpenRetry: TButton
    Left = 387
    Top = 360
    Width = 75
    Height = 24
    Caption = 'Cancel'
    TabOrder = 5
    Visible = False
    OnClick = cmdCancelFileOpenRetryClick
  end
  object tmrOpenFileRetry: TTimer
    Enabled = False
    OnTimer = tmrOpenFileRetryTimer
    Left = 392
    Top = 216
  end
end
