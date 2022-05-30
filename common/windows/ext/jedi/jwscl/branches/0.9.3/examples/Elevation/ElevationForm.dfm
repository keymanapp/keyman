object Form1: TForm1
  Left = 2509
  Top = 191
  Width = 376
  Height = 322
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 48
    Width = 42
    Height = 13
    Caption = 'Filename'
  end
  object lblCurrentUser: TLabel
    Left = 16
    Top = 16
    Width = 212
    Height = 13
    Caption = 'Process runs as user "bla" (with admin privs)'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lblSurun: TLabel
    Left = 128
    Top = 192
    Width = 83
    Height = 13
    Caption = 'Surun is available'
  end
  object lblPID: TLabel
    Left = 152
    Top = 256
    Width = 3
    Height = 13
  end
  object JvFilenameEdit1: TJvFilenameEdit
    Left = 64
    Top = 48
    Width = 281
    Height = 21
    TabOrder = 0
    Text = 'JvFilenameEdit1'
  end
  object RadioButton2000: TRadioButton
    Tag = 5
    Left = 24
    Top = 88
    Width = 113
    Height = 17
    Caption = 'Windows 2000'
    TabOrder = 1
    OnClick = RadioButton2000Click
  end
  object RadioButtonXP: TRadioButton
    Left = 24
    Top = 112
    Width = 113
    Height = 17
    Caption = 'XP'
    TabOrder = 4
    OnClick = RadioButton2000Click
  end
  object RadioButtonVista: TRadioButton
    Left = 24
    Top = 136
    Width = 113
    Height = 17
    Caption = 'Vista'
    TabOrder = 5
    OnClick = RadioButton2000Click
  end
  object RadioButtonWin7: TRadioButton
    Left = 24
    Top = 160
    Width = 113
    Height = 17
    Caption = 'Win7'
    TabOrder = 6
    OnClick = RadioButton2000Click
  end
  object chkSuRun: TCheckBox
    Left = 24
    Top = 192
    Width = 97
    Height = 17
    Caption = 'Use Surun'
    TabOrder = 2
  end
  object btnRun: TButton
    Left = 24
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 3
    OnClick = btnRunClick
  end
  object chkAllowUI: TCheckBox
    Left = 24
    Top = 216
    Width = 97
    Height = 17
    Caption = 'Allow UI'
    TabOrder = 7
  end
  object dlgLogin: TJvLoginDialog
    Caption = 'Elevate'
    Active = False
    AppTitleLabelCaption = 'Supply Administrator credentials.'
    PasswordLabelCaption = 'Password'
    UserNameLabelCaption = 'Username'
    OnCheckUser = dlgLoginCheckUser
    OnGetPassword = dlgLoginGetPassword
    Left = 184
    Top = 88
  end
end
