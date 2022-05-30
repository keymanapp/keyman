inherited frmProgress: TfrmProgress
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 148
  ClientWidth = 305
  Font.Name = 'Tahoma'
  Position = poScreenCenter
  ExplicitWidth = 311
  ExplicitHeight = 177
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
