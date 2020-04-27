object frmDiagnosticTests: TfrmDiagnosticTests
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Diagnostic Tests'
  ClientHeight = 166
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object cmdSendTestException: TButton
    Left = 8
    Top = 8
    Width = 185
    Height = 25
    Caption = '&Send Test Exception'
    TabOrder = 0
    OnClick = cmdSendTestExceptionClick
  end
  object cmdSendTestCOMAPIException: TButton
    Left = 8
    Top = 70
    Width = 185
    Height = 25
    Caption = 'Send Test COM &API Exception'
    TabOrder = 2
    OnClick = cmdSendTestCOMAPIEventClick
  end
  object cmdClose: TButton
    Left = 279
    Top = 133
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Close'
    ModalResult = 1
    TabOrder = 5
  end
  object cmdSendTestEvent: TButton
    Left = 8
    Top = 39
    Width = 185
    Height = 25
    Caption = '&Send Test Event'
    TabOrder = 1
    OnClick = cmdSendTestEventClick
  end
  object cmdSendTestKeyman32Exception: TButton
    Left = 8
    Top = 101
    Width = 185
    Height = 25
    Caption = 'Send Test &keyman32 Exception'
    TabOrder = 3
    OnClick = cmdSendTestKeyman32ExceptionClick
  end
  object cmdSendTestKMTipException: TButton
    Left = 8
    Top = 132
    Width = 185
    Height = 25
    Caption = 'Send Test km&tip Exception'
    TabOrder = 4
    OnClick = cmdSendTestKMTipExceptionClick
  end
end
