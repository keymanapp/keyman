object frmOpenCRMRecord: TfrmOpenCRMRecord
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Open Keyman Issue/PR'
  ClientHeight = 233
  ClientWidth = 471
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TLabel
    Left = 20
    Top = 20
    Width = 115
    Height = 19
    Caption = '&Issue/PR/Search'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblRepo: TLabel
    Left = 20
    Top = 72
    Width = 75
    Height = 19
    Caption = '&Repository'
    FocusControl = cbRepository
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object editSearchFor: TEdit
    Left = 156
    Top = 17
    Width = 293
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = editSearchForChange
  end
  object cmdOK: TButton
    Left = 156
    Top = 188
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object cmdCancel: TButton
    Left = 240
    Top = 188
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object cbRepository: TComboBox
    Left = 156
    Top = 69
    Width = 293
    Height = 27
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = cbRepositoryClick
  end
  object cmdCopyHTML: TButton
    Left = 156
    Top = 110
    Width = 159
    Height = 25
    Caption = 'Copy short form as &HTML link'
    TabOrder = 2
    OnClick = cmdCopyHTMLClick
  end
end
