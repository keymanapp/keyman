inherited frmDownloadProgress: TfrmDownloadProgress
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Download Progress'
  ClientHeight = 148
  ClientWidth = 305
  OldCreateOrder = True
  Position = poScreenCenter
  OnShow = TntFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 12
    Top = 68
    Width = 281
    Height = 33
    Alignment = taCenter
    AutoSize = False
    WordWrap = True
  end
  object progress: TProgressBar
    Left = 12
    Top = 44
    Width = 281
    Height = 17
    TabOrder = 0
  end
  object cmdCancel: TButton
    Left = 116
    Top = 112
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = cmdCancelClick
  end
end
