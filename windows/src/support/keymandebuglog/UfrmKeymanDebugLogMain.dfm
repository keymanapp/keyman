object frmDebugLog: TfrmDebugLog
  Left = 0
  Top = 0
  Caption = 'Keyman Debug Log'
  ClientHeight = 424
  ClientWidth = 714
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object panCommand: TPanel
    Left = 0
    Top = 0
    Width = 714
    Height = 41
    Align = alTop
    TabOrder = 0
    object cmdClearLog: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Clear Log'
      TabOrder = 0
      OnClick = cmdClearLogClick
    end
    object editFilterProcess: TEdit
      Left = 89
      Top = 12
      Width = 121
      Height = 21
      TabOrder = 1
      TextHint = 'Process filter'
    end
  end
  object gridLog: TStringGrid
    Left = 0
    Top = 41
    Width = 714
    Height = 383
    Align = alClient
    DefaultRowHeight = 16
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    TabOrder = 1
    ColWidths = (
      64
      64
      64
      64
      64)
    RowHeights = (
      16
      16
      16
      16
      16)
  end
end
