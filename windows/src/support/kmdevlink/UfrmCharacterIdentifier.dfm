object frmCharacterIdentifier: TfrmCharacterIdentifier
  Left = 0
  Top = 0
  Caption = 'Keyman Character Identifier'
  ClientHeight = 611
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 101
    Width = 625
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 240
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 209
    Width = 625
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 107
    ExplicitWidth = 629
  end
  object sgChars: TStringGrid
    Left = 0
    Top = 104
    Width = 625
    Height = 105
    Align = alClient
    ColCount = 1
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 2
    FixedRows = 0
    TabOrder = 1
    OnDrawCell = sgCharsDrawCell
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 625
    Height = 101
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    DesignSize = (
      625
      101)
    object pmChars: TRichEdit
      Left = 8
      Top = 8
      Width = 546
      Height = 93
      Cursor = crIBeam
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Code2000'
      Font.Style = [fsBold]
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
      Zoom = 100
      OnChange = pmCharsChange
    end
    object cmdFont: TButton
      Left = 560
      Top = 8
      Width = 61
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Font...'
      TabOrder = 1
      OnClick = cmdFontClick
    end
    object SpTBXButton2: TButton
      Left = 560
      Top = 43
      Width = 61
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'SpTBXButton2'
      TabOrder = 2
      Visible = False
    end
  end
  object gridFonts: TStringGrid
    Left = 0
    Top = 212
    Width = 625
    Height = 399
    Align = alBottom
    ColCount = 2
    DefaultColWidth = 200
    DefaultRowHeight = 16
    FixedCols = 0
    TabOrder = 2
    OnClick = gridFontsClick
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 516
    Top = 16
  end
end
