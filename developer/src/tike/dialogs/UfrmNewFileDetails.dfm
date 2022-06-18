inherited frmNewFileDetails: TfrmNewFileDetails
  Left = 282
  Top = 282
  ActiveControl = editFileName
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'New File'
  ClientHeight = 138
  ClientWidth = 393
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  ExplicitWidth = 399
  ExplicitHeight = 167
  PixelsPerInch = 96
  TextHeight = 13
  object lblFileTypeLabel: TLabel
    Left = 12
    Top = 12
    Width = 46
    Height = 13
    Caption = 'File Type:'
  end
  object lblFileName: TLabel
    Left = 12
    Top = 67
    Width = 50
    Height = 13
    Caption = 'File &Name:'
    FocusControl = editFileName
  end
  object lblFileType: TLabel
    Left = 72
    Top = 12
    Width = 50
    Height = 13
    Caption = 'lblFileType'
  end
  object Label1: TLabel
    Left = 12
    Top = 40
    Width = 44
    Height = 13
    Caption = 'File &Path:'
    FocusControl = editFilePath
  end
  object editFileName: TEdit
    Left = 72
    Top = 64
    Width = 229
    Height = 21
    AutoSelect = False
    TabOrder = 1
    OnChange = editFileNameChange
  end
  object cmdOK: TButton
    Left = 120
    Top = 100
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = cmdOKClick
  end
  object cmdBrowse: TButton
    Left = 308
    Top = 64
    Width = 73
    Height = 21
    Caption = '&Browse...'
    TabOrder = 2
    OnClick = cmdBrowseClick
  end
  object cmdCancel: TButton
    Left = 200
    Top = 100
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object editFilePath: TEdit
    Left = 72
    Top = 37
    Width = 229
    Height = 21
    AutoSelect = False
    TabOrder = 0
    OnChange = editFileNameChange
  end
  object dlgSave: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'New File'
    Left = 132
    Top = 20
  end
end
