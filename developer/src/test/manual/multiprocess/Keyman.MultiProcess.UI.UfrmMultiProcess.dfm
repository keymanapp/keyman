object frmMultiProcess: TfrmMultiProcess
  Left = 0
  Top = 0
  Caption = 'frmMultiProcess'
  ClientHeight = 333
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 240
    Top = 11
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 296
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Button1: TButton
    Left = 512
    Top = 8
    Width = 75
    Height = 25
    Caption = 'New Process'
    TabOrder = 1
    OnClick = Button1Click
  end
  object lbProcess: TListBox
    Left = 8
    Top = 8
    Width = 209
    Height = 289
    ItemHeight = 13
    TabOrder = 2
    OnDblClick = lbProcessDblClick
  end
  object cmdFocus: TButton
    Left = 8
    Top = 300
    Width = 75
    Height = 25
    Caption = 'Focus process'
    TabOrder = 3
    OnClick = cmdFocusClick
  end
  object tmrEnumerate: TTimer
    Interval = 100
    OnTimer = tmrEnumerateTimer
    Left = 400
    Top = 112
  end
end
