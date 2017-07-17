object Form1: TForm1
  Left = 853
  Top = 434
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 284
  ClientWidth = 418
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
  object ListBox1: TListBox
    Left = 24
    Top = 32
    Width = 241
    Height = 193
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 272
    Top = 32
    Width = 137
    Height = 41
    Caption = 'List'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 272
    Top = 80
    Width = 137
    Height = 41
    Caption = 'Add'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 272
    Top = 128
    Width = 137
    Height = 41
    Caption = 'Delete'
    TabOrder = 3
    OnClick = Button3Click
  end
end
