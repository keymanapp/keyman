inherited frmCharacterIdentifier: TfrmCharacterIdentifier
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Character Identifier'
  ClientHeight = 138
  ClientWidth = 406
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Segoe UI'
  FormStyle = fsStayOnTop
  Position = poDefault
  OnActivate = FormActivate
  OnConstrainedResize = FormConstrainedResize
  OnDestroy = FormDestroy
  OnResize = FormResize
  ExplicitWidth = 422
  ExplicitHeight = 177
  PixelsPerInch = 96
  TextHeight = 13
  object splitterBottom: TSplitter [0]
    Left = 0
    Top = 68
    Width = 406
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 107
    ExplicitWidth = 629
  end
  object sgChars: TStringGrid [1]
    Left = 0
    Top = 0
    Width = 406
    Height = 68
    Align = alClient
    ColCount = 1
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 2
    FixedRows = 0
    ScrollBars = ssHorizontal
    TabOrder = 0
    OnDblClick = sgCharsDblClick
    OnDrawCell = sgCharsDrawCell
    OnEnter = sgCharsEnter
    OnKeyDown = sgCharsKeyDown
    ColWidths = (
      64)
    RowHeights = (
      24
      24)
  end
  object gridFonts: TStringGrid [2]
    Left = 0
    Top = 71
    Width = 406
    Height = 67
    Align = alBottom
    ColCount = 2
    DefaultColWidth = 200
    DefaultRowHeight = 16
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 1
    OnClick = gridFontsClick
    OnEnter = sgCharsEnter
    OnKeyDown = sgCharsKeyDown
    ColWidths = (
      335
      200)
    RowHeights = (
      16
      16
      16
      16
      16)
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
