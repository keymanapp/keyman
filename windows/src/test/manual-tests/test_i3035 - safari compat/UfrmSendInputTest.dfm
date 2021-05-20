object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'SendInput Test Harness'
  ClientHeight = 176
  ClientWidth = 547
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TLabel
    Left = 8
    Top = 11
    Width = 61
    Height = 13
    Caption = 'Text to send'
  end
  object TntLabel2: TLabel
    Left = 8
    Top = 44
    Width = 37
    Height = 13
    Caption = 'Options'
  end
  object lblCountdown: TLabel
    Left = 441
    Top = 17
    Width = 8
    Height = 33
    Alignment = taRightJustify
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object editText: TEdit
    Left = 75
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object chkTimer: TCheckBox
    Left = 75
    Top = 43
    Width = 146
    Height = 17
    Caption = 'char-by-char with timer'
    TabOrder = 1
  end
  object memoLog: TMemo
    Left = 75
    Top = 66
    Width = 463
    Height = 99
    ScrollBars = ssVertical
    TabOrder = 2
    WordWrap = False
  end
  object cmdGo: TButton
    Left = 463
    Top = 8
    Width = 75
    Height = 49
    Caption = 'Send Input'
    TabOrder = 3
    OnClick = cmdGoClick
  end
  object timerSend: TTimer
    Enabled = False
    Interval = 500
    OnTimer = timerSendTimer
    Left = 220
    Top = 88
  end
  object timerCountdown: TTimer
    Enabled = False
    OnTimer = timerCountdownTimer
    Left = 272
    Top = 88
  end
end
