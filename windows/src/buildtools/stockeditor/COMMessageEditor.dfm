object frmCOMMessages: TfrmCOMMessages
  Left = 234
  Top = 123
  Caption = 'frmCOMMessages'
  ClientHeight = 445
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    685
    445)
  PixelsPerInch = 96
  TextHeight = 13
  object gridMessages: TStringGrid
    Left = 4
    Top = 4
    Width = 677
    Height = 405
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 6
    DefaultRowHeight = 16
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    OnSelectCell = gridMessagesSelectCell
    OnSetEditText = gridMessagesSetEditText
    ColWidths = (
      47
      252
      354
      89
      64
      185)
    RowHeights = (
      16
      16
      16
      16
      16)
  end
  object cmdLoad: TButton
    Left = 4
    Top = 416
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Load'
    TabOrder = 1
    OnClick = cmdLoadClick
  end
  object cmdSave: TButton
    Left = 84
    Top = 416
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Save'
    TabOrder = 2
    OnClick = cmdSaveClick
  end
  object cmdAdd: TButton
    Left = 204
    Top = 416
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Add'
    TabOrder = 3
    OnClick = cmdAddClick
  end
  object cmdDelete: TButton
    Left = 284
    Top = 416
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Delete'
    TabOrder = 4
    OnClick = cmdDeleteClick
  end
end
