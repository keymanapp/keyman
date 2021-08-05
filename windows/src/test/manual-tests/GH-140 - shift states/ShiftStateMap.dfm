object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Shift State Map'
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
  object Memo1: TMemo
    Left = 0
    Top = 73
    Width = 635
    Height = 226
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 73
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 11
      Top = 14
      Width = 30
      Height = 13
      Caption = 'Press:'
    end
    object Label2: TLabel
      Left = 12
      Top = 47
      Width = 42
      Height = 13
      Caption = 'Release:'
    end
    object cmdPressLShift: TButton
      Left = 60
      Top = 9
      Width = 49
      Height = 25
      Caption = 'LShift'
      TabOrder = 0
      OnClick = cmdPressClick
    end
    object cmdPressRShift: TButton
      Tag = 1
      Left = 115
      Top = 9
      Width = 48
      Height = 25
      Caption = 'RShift'
      TabOrder = 1
      OnClick = cmdPressClick
    end
    object cmdPressRCtrl: TButton
      Tag = 3
      Left = 224
      Top = 9
      Width = 48
      Height = 25
      Caption = 'RCtrl'
      TabOrder = 2
      OnClick = cmdPressClick
    end
    object cmdPressLCtrl: TButton
      Tag = 2
      Left = 169
      Top = 9
      Width = 49
      Height = 25
      Caption = 'LCtrl'
      TabOrder = 3
      OnClick = cmdPressClick
    end
    object cmdPressLAlt: TButton
      Tag = 4
      Left = 277
      Top = 9
      Width = 49
      Height = 25
      Caption = 'LAlt'
      TabOrder = 4
      OnClick = cmdPressClick
    end
    object cmdPressRAlt: TButton
      Tag = 5
      Left = 332
      Top = 9
      Width = 48
      Height = 25
      Caption = 'RAlt'
      TabOrder = 5
      OnClick = cmdPressClick
    end
    object cmdReleaseLShift: TButton
      Left = 60
      Top = 42
      Width = 49
      Height = 25
      Caption = 'LShift'
      TabOrder = 6
      OnClick = cmdReleaseClick
    end
    object cmdReleaseRShift: TButton
      Tag = 1
      Left = 115
      Top = 42
      Width = 48
      Height = 25
      Caption = 'RShift'
      TabOrder = 7
      OnClick = cmdReleaseClick
    end
    object cmdReleaseRCtrl: TButton
      Tag = 3
      Left = 224
      Top = 42
      Width = 48
      Height = 25
      Caption = 'RCtrl'
      TabOrder = 8
      OnClick = cmdReleaseClick
    end
    object cmdReleaseLCtrl: TButton
      Tag = 2
      Left = 169
      Top = 42
      Width = 49
      Height = 25
      Caption = 'LCtrl'
      TabOrder = 9
      OnClick = cmdReleaseClick
    end
    object cmdReleaseLAlt: TButton
      Tag = 4
      Left = 277
      Top = 42
      Width = 49
      Height = 25
      Caption = 'LAlt'
      TabOrder = 10
      OnClick = cmdReleaseClick
    end
    object cmdReleaseRAlt: TButton
      Tag = 5
      Left = 332
      Top = 42
      Width = 48
      Height = 25
      Caption = 'RAlt'
      TabOrder = 11
      OnClick = cmdReleaseClick
    end
    object cmdMapVirtualKey: TButton
      Tag = 5
      Left = 396
      Top = 9
      Width = 93
      Height = 25
      Caption = 'MapVirtualKey'
      TabOrder = 12
      OnClick = cmdMapVirtualKeyClick
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 72
    Top = 176
  end
  object ApplicationEvents1: TApplicationEvents
    OnMessage = ApplicationEvents1Message
    Left = 312
    Top = 152
  end
end
