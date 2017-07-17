inherited frmTestKeyboard: TfrmTestKeyboard
  Left = 123
  Top = 143
  HelpContext = 1003
  Caption = 'frmTestKeyboard'
  ClientHeight = 470
  ClientWidth = 638
  KeyPreview = True
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnKeyDown = FormKeyDown
  ExplicitWidth = 638
  ExplicitHeight = 470
  PixelsPerInch = 96
  TextHeight = 13
  object HSplitter: TSplitter
    Left = 0
    Top = 370
    Width = 638
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 296
    ExplicitWidth = 630
  end
  object panFontInfo: TPanel
    Left = 0
    Top = 0
    Width = 638
    Height = 29
    Align = alTop
    TabOrder = 0
    object chkDebug: TCheckBox
      Left = 346
      Top = 6
      Width = 77
      Height = 17
      TabStop = False
      Caption = 'De&bugging'
      TabOrder = 0
      Visible = False
      OnClick = chkDebugClick
    end
    object chkTestKeyboard: TCheckBox
      Left = 6
      Top = 6
      Width = 97
      Height = 17
      Caption = '&Keyboard active'
      TabOrder = 1
      OnClick = chkTestKeyboardClick
    end
    object rbANSI: TRadioButton
      Left = 116
      Top = 6
      Width = 49
      Height = 17
      Caption = '&ANSI'
      TabOrder = 2
      OnClick = rbANSIClick
    end
    object rbUnicode: TRadioButton
      Left = 172
      Top = 6
      Width = 65
      Height = 17
      Caption = '&Unicode'
      TabOrder = 3
      OnClick = rbUnicodeClick
    end
  end
  object lbDebug: TListBox
    Left = 0
    Top = 373
    Width = 638
    Height = 97
    Align = alBottom
    ItemHeight = 13
    PopupMenu = mnuDebugPopup
    TabOrder = 1
  end
  object memo: TKeymanDeveloperDebuggerMemo
    Left = 0
    Top = 29
    Width = 638
    Height = 341
    Cursor = crIBeam
    Alignment = taLeftJustify
    ScrollBars = ssVertical
    AltFont.Charset = DEFAULT_CHARSET
    AltFont.Color = clWindowText
    AltFont.Height = -11
    AltFont.Name = 'MS Sans Serif'
    AltFont.Style = []
    CaretWidth = 1
    ColumnWrap = 0
    DisplayOnly = False
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
    SpecUnderline.Color = clRed
    UndoMaxSpace = 65536
    Version = 'v7.1  Professional ed.'
    OnSelMove = memoSelMove
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial Unicode MS'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    TabStop = True
    OnEnter = memoGotFocus
    OnExit = memoLostFocus
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 108
    Top = 40
  end
  object mnuDebugPopup: TPopupMenu
    Left = 44
    Top = 40
    object mnuDebugSaveToFile: TMenuItem
      Caption = '&Save to file...'
      OnClick = mnuDebugSaveToFileClick
    end
    object mnuDebugClear: TMenuItem
      Caption = '&Clear'
      OnClick = mnuDebugClearClick
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Left = 140
    Top = 40
  end
end
