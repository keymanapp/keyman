object frmProgress: TfrmProgress
  Left = 295
  Top = 281
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'frmProgress'
  ClientHeight = 69
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 12
    Top = 44
    Width = 309
    Height = 13
    AutoSize = False
  end
  object prog: TProgressBar
    Left = 12
    Top = 20
    Width = 309
    Height = 17
    Min = 0
    Max = 100
    TabOrder = 0
  end
end
