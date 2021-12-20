object frmNgrokOptions: TfrmNgrokOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Keyman Developer Server Options'
  ClientHeight = 436
  ClientWidth = 433
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
  object Label1: TLabel
    Left = 8
    Top = 96
    Width = 403
    Height = 39
    Caption = 
      'ngrok is a service that allows you to temporarily share your loc' +
      'al web debugger site with a public URL, so that you can access i' +
      't from any device or share it with other users for testing your ' +
      'keyboards, models and packages.'
    WordWrap = True
  end
  object lblWebHostDefaultPort: TLabel
    Left = 8
    Top = 19
    Width = 58
    Height = 13
    Caption = '&Default port'
  end
  object cmdOK: TButton
    Left = 272
    Top = 403
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 352
    Top = 403
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object gbSetup: TGroupBox
    Left = 8
    Top = 144
    Width = 417
    Height = 150
    Caption = 'Setup'
    TabOrder = 3
    object lblAuthToken: TLabel
      Left = 16
      Top = 93
      Width = 100
      Height = 13
      Caption = '&Authentication token'
      FocusControl = editAuthToken
    end
    object lblRegion: TLabel
      Left = 16
      Top = 120
      Width = 33
      Height = 13
      Caption = '&Region'
      FocusControl = cbRegion
    end
    object lblVersion: TLabel
      Left = 183
      Top = 29
      Width = 43
      Height = 13
      Caption = '(version)'
    end
    object lblGetToken: TLabel
      Left = 360
      Top = 93
      Width = 47
      Height = 13
      Cursor = crHandPoint
      Caption = 'Get token'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblGetTokenClick
    end
    object editAuthToken: TEdit
      Left = 122
      Top = 90
      Width = 231
      Height = 21
      TabOrder = 2
    end
    object cbRegion: TComboBox
      Left = 122
      Top = 117
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'us (North America)'
        'eu (Europe)'
        'au (Australia)'
        'ap (Asia/Pacific)'
        'sa (South Africa)'
        'jp (Japan)'
        'in (India)')
    end
    object cmdDownload: TButton
      Left = 16
      Top = 24
      Width = 161
      Height = 25
      Caption = 'Download or &update ngrok'
      TabOrder = 0
      OnClick = cmdDownloadClick
    end
    object cmdCreateAccount: TButton
      Left = 16
      Top = 55
      Width = 161
      Height = 25
      Caption = '&Create free ngrok account...'
      TabOrder = 1
      OnClick = cmdCreateAccountClick
    end
  end
  object gbAdvanced: TGroupBox
    Left = 8
    Top = 300
    Width = 417
    Height = 90
    Caption = 'Advanced Options'
    TabOrder = 4
    object lblControlPort: TLabel
      Left = 16
      Top = 57
      Width = 58
      Height = 13
      Caption = 'Control &port'
      FocusControl = editControlPort
    end
    object chkKeepNGrokControlWindowVisible: TCheckBox
      Left = 16
      Top = 27
      Width = 257
      Height = 17
      Caption = '&Keep ngrok control window visible'
      TabOrder = 0
    end
    object editControlPort: TEdit
      Left = 122
      Top = 54
      Width = 79
      Height = 21
      NumbersOnly = True
      TabOrder = 1
    end
  end
  object editWebHostDefaultPort: TEdit
    Left = 84
    Top = 16
    Width = 61
    Height = 21
    NumbersOnly = True
    TabOrder = 0
  end
  object chkUseNgrok: TCheckBox
    Left = 8
    Top = 73
    Width = 257
    Height = 17
    Caption = 'Use &ngrok to provide public url for web debugger'
    TabOrder = 2
  end
  object chkLeaveServerRunning: TCheckBox
    Left = 8
    Top = 43
    Width = 257
    Height = 17
    Caption = '&Leave server running after closing IDE'
    TabOrder = 1
  end
end
