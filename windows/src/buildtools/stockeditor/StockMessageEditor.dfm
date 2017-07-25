object frmStockMessageEditor: TfrmStockMessageEditor
  Left = 131
  Top = 146
  Caption = 'Stock Message Editor'
  ClientHeight = 453
  ClientWidth = 850
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    850
    453)
  PixelsPerInch = 96
  TextHeight = 13
  object gridMessages: TStringGridEditControlled
    Left = 8
    Top = 8
    Width = 834
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 16
    DefaultDrawing = False
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing]
    ScrollBars = ssVertical
    TabOrder = 0
    OnClick = gridMessagesClick
    OnDrawCell = gridMessagesDrawCell
    OnSelectCell = gridMessagesSelectCell
    OnSetEditText = gridMessagesSetEditText
    OnCanEditShow = gridMessagesCanEditShow
    ColWidths = (
      200
      469)
    RowHeights = (
      16
      16
      16
      16
      16)
  end
  object panMessageDetails: TPanel
    Left = 8
    Top = 200
    Width = 835
    Height = 213
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    DesignSize = (
      835
      213)
    object lblIdentifier: TLabel
      Left = 8
      Top = 8
      Width = 40
      Height = 13
      Caption = 'Identifier'
      FocusControl = editIdentifier
    end
    object lblText: TLabel
      Left = 8
      Top = 32
      Width = 58
      Height = 13
      Caption = 'Default Text'
      FocusControl = memoText
    end
    object lblParams: TLabel
      Left = 8
      Top = 79
      Width = 53
      Height = 13
      Caption = 'Parameters'
    end
    object lblComment: TLabel
      Left = 8
      Top = 148
      Width = 53
      Height = 13
      Caption = 'Description'
    end
    object editIdentifier: TEdit
      Left = 68
      Top = 4
      Width = 309
      Height = 21
      TabStop = False
      TabOrder = 0
      OnChange = editIdentifierChange
    end
    object memoText: TMemo
      Left = 68
      Top = 28
      Width = 759
      Height = 45
      Cursor = crIBeam
      Anchors = [akLeft, akTop, akRight]
      ScrollBars = ssVertical
      TabOrder = 1
      OnChange = memoTextChange
    end
    object memoComment: TMemo
      Left = 68
      Top = 144
      Width = 759
      Height = 45
      Cursor = crIBeam
      Anchors = [akLeft, akTop, akRight]
      ScrollBars = ssVertical
      TabOrder = 2
      OnChange = memoCommentChange
    end
    object gridParams: TStringGrid
      Left = 68
      Top = 76
      Width = 710
      Height = 65
      Anchors = [akLeft, akTop, akRight]
      ColCount = 2
      DefaultRowHeight = 16
      DefaultDrawing = False
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing]
      TabOrder = 3
      OnDrawCell = gridParamsDrawCell
      OnSetEditText = gridParamsSetEditText
      ColWidths = (
        70
        492)
      RowHeights = (
        16
        16)
    end
    object cmdAddParam: TButton
      Left = 781
      Top = 76
      Width = 45
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Add...'
      TabOrder = 4
      OnClick = cmdAddParamClick
    end
    object cmdDeleteParam: TButton
      Left = 781
      Top = 100
      Width = 45
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Delete'
      TabOrder = 5
      OnClick = cmdDeleteParamClick
    end
  end
  object cmdExit: TButton
    Left = 765
    Top = 420
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'E&xit'
    ModalResult = 2
    TabOrder = 3
    OnClick = cmdExitClick
  end
  object cmdOK: TButton
    Left = 681
    Top = 420
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    Visible = False
    OnClick = cmdOKClick
  end
  object cmdAdd: TButton
    Left = 8
    Top = 420
    Width = 77
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add...'
    TabOrder = 4
    OnClick = cmdAddClick
  end
  object cmdDelete: TButton
    Left = 92
    Top = 420
    Width = 77
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete'
    TabOrder = 5
    OnClick = cmdDeleteClick
  end
  object cmdMoveToSection: TButton
    Left = 176
    Top = 420
    Width = 109
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Move to Section...'
    TabOrder = 6
    OnClick = cmdMoveToSectionClick
  end
  object cmdSave: TButton
    Left = 564
    Top = 420
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Save...'
    TabOrder = 7
    OnClick = cmdSaveClick
  end
  object cmdNewSection: TButton
    Left = 296
    Top = 420
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&New Section...'
    TabOrder = 8
    OnClick = cmdNewSectionClick
  end
  object cmdDeleteSection: TButton
    Left = 376
    Top = 420
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete &Section'
    TabOrder = 9
    OnClick = cmdDeleteSectionClick
  end
  object cmdRenameSection: TButton
    Left = 456
    Top = 420
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Rename Section'
    TabOrder = 10
    OnClick = cmdRenameSectionClick
  end
end
