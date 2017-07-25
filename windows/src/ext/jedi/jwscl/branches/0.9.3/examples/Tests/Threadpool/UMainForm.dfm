object Form1: TForm1
  Left = 248
  Top = 292
  Width = 826
  Height = 546
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 280
    Width = 169
    Height = 25
    Caption = 'Run cmd in session'
    TabOrder = 0
    OnClick = Button1Click
  end
  object SessionEdit: TEdit
    Left = 16
    Top = 248
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '1'
  end
  object ListView1: TListView
    Left = 16
    Top = 24
    Width = 265
    Height = 209
    Columns = <
      item
        Caption = 'Session'
        Width = 75
      end
      item
        Caption = 'Process ID'
        Width = 75
      end
      item
        Caption = 'ProcessHandle'
        Width = 100
      end>
    ReadOnly = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object Button2: TButton
    Left = 296
    Top = 24
    Width = 113
    Height = 25
    Caption = 'Terminate'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 296
    Top = 64
    Width = 113
    Height = 25
    Caption = 'Send WM_QUIT'
    TabOrder = 4
  end
end
