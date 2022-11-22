inherited frmMessageDlgWithSave: TfrmMessageDlgWithSave
  Left = 192
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Keyman Developer'
  ClientHeight = 133
  ClientWidth = 393
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  ExplicitWidth = 399
  ExplicitHeight = 162
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 8
    Top = 8
    Width = 377
    Height = 58
    AutoSize = False
    WordWrap = True
  end
  object cmdYes: TButton
    Left = 65
    Top = 100
    Width = 73
    Height = 25
    Caption = '&Yes'
    Default = True
    ModalResult = 6
    TabOrder = 1
  end
  object cmdNo: TButton
    Left = 160
    Top = 100
    Width = 73
    Height = 25
    Caption = '&No'
    ModalResult = 7
    TabOrder = 2
  end
  object cmdCancel: TButton
    Left = 255
    Top = 100
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object chkSave: TCheckBox
    Left = 8
    Top = 72
    Width = 377
    Height = 17
    Caption = '&Remember this choice for next time'
    TabOrder = 0
  end
end
