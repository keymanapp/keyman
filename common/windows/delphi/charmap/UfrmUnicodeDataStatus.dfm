object frmUnicodeDataStatus: TfrmUnicodeDataStatus
  Left = 297
  Top = 261
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Updating Character Database'
  ClientHeight = 105
  ClientWidth = 282
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
  object lblPleaseWait: TLabel
    Left = 8
    Top = 8
    Width = 220
    Height = 13
    Caption = 'Please wait while updating character database'
  end
  object lblStatus: TLabel
    Left = 4
    Top = 32
    Width = 273
    Height = 13
    Alignment = taCenter
    AutoSize = False
  end
  object pbar: TProgressBar
    Left = 4
    Top = 64
    Width = 273
    Height = 17
    Max = 1000
    Smooth = True
    TabOrder = 0
  end
end
