object frmI4793: TfrmI4793
  Left = 0
  Top = 0
  Caption = 'I4793'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDblClick = FormDblClick
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 16
    Top = 35
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 16
    Top = 58
    Width = 3
    Height = 13
  end
  object Label4: TLabel
    Left = 16
    Top = 248
    Width = 611
    Height = 43
    AutoSize = False
    Caption = 
      'Reproduces the timing issue in I4793. Select a Keyman keyboard a' +
      'nd ensure it is functioning. Then double-click on the form to co' +
      'unt down for the demo.  After 5 seconds, shift+abc-shift will be' +
      ' sent, and Keyman will process it.  Keyman will fail to release ' +
      'the shift key, so all subsequent input will also be shifted.'
    WordWrap = True
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 584
    Top = 16
  end
end
