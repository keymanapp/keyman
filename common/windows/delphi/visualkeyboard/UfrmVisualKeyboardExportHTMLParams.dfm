inherited frmVisualKeyboardExportHTMLParams: TfrmVisualKeyboardExportHTMLParams
  Left = 600
  Top = 284
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Output HTML Parameters'
  ClientHeight = 274
  ClientWidth = 337
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object cmdOK: TButton
    Left = 256
    Top = 8
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 256
    Top = 40
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbOutputType: TGroupBox
    Left = 8
    Top = 8
    Width = 241
    Height = 257
    Caption = 'Output Type'
    TabOrder = 2
    object Label1: TLabel
      Left = 26
      Top = 35
      Width = 206
      Height = 26
      Caption = 
        'Use this option to organise files more neatly. The subdirectory ' +
        'will be called :'
      WordWrap = True
    end
    object lblFileName: TLabel
      Left = 48
      Top = 67
      Width = 79
      Height = 13
      Caption = 'myfilename_Files'
    end
    object Label5: TLabel
      Left = 26
      Top = 111
      Width = 186
      Height = 65
      AutoSize = False
      Caption = 
        'Use this option when you want to include the On Screen Keyboard ' +
        'in a HTML file in a package, as packages don'#39't support subdirect' +
        'ories'
      WordWrap = True
    end
    object Label2: TLabel
      Left = 26
      Top = 202
      Width = 186
      Height = 47
      AutoSize = False
      Caption = 
        'Background graphics make the key images look nicer, but may be h' +
        'arder to print in some web browsers'
      WordWrap = True
    end
    object rbFolders: TRadioButton
      Left = 8
      Top = 16
      Width = 204
      Height = 17
      Caption = 'Images and CSS in subdirectory'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbNoFolders: TRadioButton
      Left = 8
      Top = 88
      Width = 189
      Height = 17
      Caption = 'Images and CSS in same directory'
      TabOrder = 1
    end
    object chkGraphical: TCheckBox
      Left = 8
      Top = 179
      Width = 213
      Height = 17
      Caption = 'Use background graphic for key images '
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
end
