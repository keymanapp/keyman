object DownloadRequest: TDownloadRequest
  Left = 557
  Top = 186
  BorderStyle = bsDialog
  Caption = 'File Download'
  ClientHeight = 161
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbQuestion: TLabel
    Left = 16
    Top = 16
    Width = 217
    Height = 13
    Caption = 'Do you want to open or save this file?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbName: TLabel
    Left = 81
    Top = 40
    Width = 31
    Height = 13
    Alignment = taRightJustify
    Caption = 'Name:'
  end
  object lbType: TLabel
    Left = 85
    Top = 56
    Width = 27
    Height = 13
    Alignment = taRightJustify
    Caption = 'Type:'
  end
  object lbFrom: TLabel
    Left = 86
    Top = 88
    Width = 26
    Height = 13
    Alignment = taRightJustify
    Caption = 'From:'
  end
  object imgIcon: TImage
    Left = 16
    Top = 40
    Width = 32
    Height = 32
  end
  object lbNameTxt: TLabel
    Left = 120
    Top = 40
    Width = 241
    Height = 13
    AutoSize = False
  end
  object lbTypeTxt: TLabel
    Left = 120
    Top = 56
    Width = 241
    Height = 13
    AutoSize = False
  end
  object lbFromTxt: TLabel
    Left = 120
    Top = 88
    Width = 241
    Height = 13
    AutoSize = False
  end
  object lbSize: TLabel
    Left = 89
    Top = 72
    Width = 23
    Height = 13
    Alignment = taRightJustify
    Caption = 'Size:'
  end
  object lbSizeTxt: TLabel
    Left = 120
    Top = 72
    Width = 241
    Height = 13
    AutoSize = False
  end
  object btnOpen: TButton
    Left = 112
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Open'
    Enabled = False
    TabOrder = 0
    OnClick = btnOpenClick
  end
  object btnSave: TButton
    Left = 200
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 1
    OnClick = btnSaveClick
  end
  object btnCancel: TButton
    Left = 288
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object dlgSave: TSaveDialog
    Left = 24
    Top = 104
  end
end
