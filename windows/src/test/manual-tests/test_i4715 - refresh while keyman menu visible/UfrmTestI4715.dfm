object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 211
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
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 225
    Height = 25
    Caption = 'Trigger Refresh after 5 seconds'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 39
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 1
    Visible = False
  end
  object Memo1: TMemo
    Left = 135
    Top = 39
    Width = 185
    Height = 89
    Lines.Strings = (
      'This program triggers a profile '
      'change notification in Keyman '
      'Engine. It '
      'is not a valid profile change '
      'because it has no atom data '
      'associated with'
      'it but it works to trigger the issue in '
      'I4715.'
      ''
      'Note that I4715 is inconsistently '
      'triggered as the objects are used '
      'after'
      'free and are usually still consistent, '
      'or in many cases, reallocated with '
      'the'
      'same object data due to the '
      'immediate reallocation of the data '
      'in Refresh'
      'procedure in TLangSwitchManager.')
    TabOrder = 2
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 200
    Top = 112
  end
end
