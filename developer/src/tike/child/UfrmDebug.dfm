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
  OnResize = FormResize
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
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    PopupMenu = mnuPopup
    ScrollBars = ssVertical
    TabOrder = 0
    OnChange = memoChange
    OnClick = memoClick
    OnEnter = memoGotFocus
    OnExit = memoLostFocus
    OnKeyUp = memoKeyUp
    OnMessage = memoMessage
    IsDebugging = False
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
    ScrollBars = ssNone
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
      Action = modActionsDebugger.actDebugViewFont
    end
    object mnuPopupResetToQuotedFont: TMenuItem
      Action = modActionsDebugger.actDebugViewDefaultFont
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
