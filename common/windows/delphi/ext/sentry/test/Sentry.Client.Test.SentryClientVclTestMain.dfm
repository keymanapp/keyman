object frmSentryClientVclTestMain: TfrmSentryClientVclTestMain
  Left = 0
  Top = 0
  Caption = 'Sentry Delphi Client - VCL Test App'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object cmdSendEvent: TButton
    Left = 8
    Top = 8
    Width = 145
    Height = 25
    Caption = 'Send an event'
    TabOrder = 0
    OnClick = cmdSendEventClick
  end
  object cmdCrashApp: TButton
    Left = 8
    Top = 39
    Width = 145
    Height = 25
    Caption = 'Crash the app'
    TabOrder = 1
    OnClick = cmdCrashAppClick
  end
end
