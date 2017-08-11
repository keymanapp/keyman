object frameAttachedFiles: TframeAttachedFiles
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Attached Files'
  ClientHeight = 244
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnResize = FormResize
  DesignSize = (
    534
    244)
  PixelsPerInch = 96
  TextHeight = 13
  object gridFiles: TStringGrid
    Left = 4
    Top = 4
    Width = 525
    Height = 235
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 4
    DefaultRowHeight = 16
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 0
    OnMouseDown = gridFilesMouseDown
    ExplicitWidth = 509
    ExplicitHeight = 197
    ColWidths = (
      244
      105
      64
      64)
    RowHeights = (
      16
      16
      16
      16
      16)
  end
  object PopupMenu1: TPopupMenu
    Left = 312
    Top = 116
    object cmdOpen: TMenuItem
      Caption = '&Open...'
      OnClick = cmdOpenClick
    end
    object cmdOpenContainingFolder: TMenuItem
      Caption = 'Open Containing &Folder...'
      OnClick = cmdOpenContainingFolderClick
    end
  end
end
