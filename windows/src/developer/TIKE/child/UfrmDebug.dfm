inherited frmDebug: TfrmDebug
  Left = 85
  Top = 229
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Debug'
  ClientHeight = 132
  ClientWidth = 755
  Font.Name = 'MS Sans Serif'
  FormStyle = fsStayOnTop
  KeyPreview = True
  Position = poDefault
  OnClose = FormClose
  OnDestroy = FormDestroy
  ExplicitWidth = 771
  ExplicitHeight = 171
  PixelsPerInch = 96
  TextHeight = 13
  object memo: TKeymanDeveloperDebuggerMemo
    Left = 0
    Top = 0
    Width = 755
    Height = 73
    Cursor = crIBeam
    Alignment = taLeftJustify
    BorderStyle = bsNone
    ScrollBars = ssVertical
    OnChange = memoChange
    AltFont.Charset = DEFAULT_CHARSET
    AltFont.Color = clWindowText
    AltFont.Height = -11
    AltFont.Name = 'MS Sans Serif'
    AltFont.Style = []
    ApplyStartStopKeys = False
    CaretWidth = 1
    ColumnWrap = 0
    DisplayOnly = False
    EnableHotKeys = False
    EndOfTextMark.Color = clRed
    EndOfTextMark.Style = psClear
    Justified = False
    LineHeight = 0
    Options = [pmoWrapCaret, pmoInsertKeyActive, pmoWideOverwriteCaret, pmoAutoScrollBars, pmoBlockSelection]
    Overwrite = False
    RightLinePen.Color = clGray
    RightLinePen.Style = psDot
    SelBackColor = clHighlight
    SelTextColor = clHighlightText
    Separators = #9#10#13' $%&,./:;<=>'
    ShowEndParSelected = False
    SpecUnderline.Color = clRed
    TabStops = 4
    UndoMaxSpace = 65536
    UpdateMode = umImmediate
    Version = 'v7.1  Professional ed.'
    OnSelMove = memoSelMove
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    PopupMenu = mnuPopup
    TabOrder = 0
    TabStop = True
    OnClick = memoClick
    OnEnter = memoGotFocus
    OnExit = memoLostFocus
    OnMessage = memoMessage
  end
  object sgChars: TStringGrid
    Left = 0
    Top = 73
    Width = 755
    Height = 59
    Align = alBottom
    ColCount = 1
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 2
    FixedRows = 0
    ScrollBars = ssHorizontal
    TabOrder = 1
    OnDrawCell = sgCharsDrawCell
    ColWidths = (
      64)
    RowHeights = (
      24
      24)
  end
  object mnuPopup: TPopupMenu
    Images = frmKeymanDeveloper.lstImages
    Left = 228
    Top = 73
    object mnuPopupFont: TMenuItem
      Action = modActionsKeyboardEditor.actDebugViewFont
    end
    object mnuPopupResetToQuotedFont: TMenuItem
      Action = modActionsKeyboardEditor.actDebugViewDefaultFont
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuCut: TMenuItem
      Action = modActionsMain.actEditCut
    end
    object mnuCopy: TMenuItem
      Action = modActionsMain.actEditCopy
    end
    object mnuPaste: TMenuItem
      Action = modActionsMain.actEditPaste
    end
  end
  object appEvents: TApplicationEvents
    Left = 368
    Top = 72
  end
end
