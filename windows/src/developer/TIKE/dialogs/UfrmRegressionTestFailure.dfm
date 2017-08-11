inherited frmRegressionTestFailure: TfrmRegressionTestFailure
  Left = 145
  Top = 241
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsDialog
  Caption = 'Regression Test Failure'
  ClientHeight = 261
  ClientWidth = 385
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  ExplicitWidth = 391
  ExplicitHeight = 290
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 227
    Height = 13
    Caption = 'Regression test failure at current event.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 28
    Height = 13
    Caption = 'Event'
  end
  object lblEvent: TLabel
    Left = 100
    Top = 48
    Width = 3
    Height = 13
  end
  object Label4: TLabel
    Left = 8
    Top = 72
    Width = 78
    Height = 13
    Caption = 'Expected output'
  end
  object Label5: TLabel
    Left = 8
    Top = 152
    Width = 63
    Height = 13
    Caption = 'Actual output'
  end
  object Label3: TLabel
    Left = 8
    Top = 28
    Width = 50
    Height = 13
    Caption = 'Test name'
  end
  object lblTestName: TLabel
    Left = 100
    Top = 28
    Width = 3
    Height = 13
  end
  object memoExpected: TKeymanDeveloperMemo
    Left = 100
    Top = 68
    Width = 277
    Height = 73
    Cursor = crIBeam
    Alignment = taLeftJustify
    ScrollBars = ssNone
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = True
  end
  object memoActual: TKeymanDeveloperMemo
    Left = 100
    Top = 148
    Width = 277
    Height = 73
    Cursor = crIBeam
    Alignment = taLeftJustify
    ScrollBars = ssNone
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    TabStop = True
  end
  object cmdOk: TButton
    Left = 304
    Top = 228
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
