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
  object memoExpected: TMemo
    Left = 100
    Top = 68
    Width = 277
    Height = 73
    Cursor = crIBeam
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object memoActual: TMemo
    Left = 100
    Top = 148
    Width = 277
    Height = 73
    Cursor = crIBeam
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
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
