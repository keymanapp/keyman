object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Test Keyman Proxy Support'
  ClientHeight = 467
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    428
    467)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 415
    Width = 32
    Height = 13
    Caption = 'Server'
  end
  object Label2: TLabel
    Left = 8
    Top = 442
    Width = 39
    Height = 13
    Caption = 'Protocol'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Test Proxy'
    TabOrder = 0
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 96
    Top = 12
    Width = 141
    Height = 17
    Caption = '&Send a test file to server'
    TabOrder = 1
  end
  object memoLog: TMemo
    Left = 8
    Top = 39
    Width = 412
    Height = 362
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object Button2: TButton
    Left = 304
    Top = 8
    Width = 117
    Height = 25
    Caption = '&Proxy Settings...'
    TabOrder = 3
    OnClick = Button2Click
  end
  object editServer: TEdit
    Left = 52
    Top = 412
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'n'
  end
  object editProtocol: TEdit
    Left = 52
    Top = 439
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'editProtocol'
  end
end
