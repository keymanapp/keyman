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
  OnCloseQuery = FormCloseQuery
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
    object pmChars: TPlusMemoU
      Left = 8
      Top = 8
      Width = 525
      Height = 93
      Cursor = crIBeam
      Alignment = taLeftJustify
      ScrollBars = ssVertical
      OnChange = pmCharsChange
      AltFont.Charset = DEFAULT_CHARSET
      AltFont.Color = clWindowText
      AltFont.Height = -11
      AltFont.Name = 'Tahoma'
      AltFont.Style = []
      CaretWidth = 1
      DisplayOnly = False
      EndOfTextMark.Color = clRed
      Justified = False
      LineHeight = 0
      NullReplacement = #0
      Options = [pmoWrapCaret, pmoInsertKeyActive, pmoWideOverwriteCaret, pmoAutoScrollBars, pmoBlockSelection]
      Overwrite = False
      SelBackColor = clHighlight
      SelTextColor = clHighlightText
      Separators = #9' $%&,./:;<=>'
      SpecUnderline.Color = clRed
      UndoMaxSpace = 65536
      Version = 'v5.3a Professional ed.'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Code2000'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      TabStop = True
      Anchors = [akLeft, akTop, akRight]
    end
    object cmdFont: TButton
      Left = 539
      Top = 8
      Width = 78
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Font...'
      TabOrder = 1
      OnClick = cmdFontClick
    end
    object cmdPaste: TButton
      Left = 539
      Top = 39
      Width = 78
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Paste'
      TabOrder = 2
      OnClick = cmdPasteClick
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
    Left = 496
    Top = 16
  end
  object mnuPopup: TPopupMenu
    Left = 308
    Top = 308
    object mnuOpen: TMenuItem
      Caption = '&Open Character Identifier...'
      Default = True
      OnClick = mnuOpenClick
    end
    object cmdOpenRenderingTestCases: TMenuItem
      Caption = 'Open &Rendering Test Cases...'
      OnClick = cmdOpenRenderingTestCasesClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mnuAbout: TMenuItem
      Caption = '&About...'
      OnClick = mnuAboutClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuExit: TMenuItem
      Caption = 'E&xit'
      OnClick = mnuExitClick
    end
  end
end
