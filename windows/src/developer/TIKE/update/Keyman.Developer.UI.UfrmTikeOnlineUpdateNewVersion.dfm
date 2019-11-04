object frmTikeOnlineUpdateNewVersion: TfrmTikeOnlineUpdateNewVersion
  Left = 385
  Top = 410
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Keyman Developer'
  ClientHeight = 173
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblNewVersion: TLabel
    Left = 16
    Top = 16
    Width = 316
    Height = 13
    Caption = 'A newer version of Keyman Developer is now available.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblVersionInfo: TLabel
    Left = 16
    Top = 40
    Width = 338
    Height = 13
    Caption = 
      'The new version is 1.2.3.4 (you are currently running version 1.' +
      '2.3.4)'
  end
  object panDownload: TPanel
    Left = 0
    Top = 64
    Width = 469
    Height = 97
    BevelOuter = bvNone
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 48
      Width = 440
      Height = 13
      Caption = 
        'Do you want to visit the website and learn more about the new ve' +
        'rsion or download it now?'
    end
    object Label4: TLabel
      Left = 16
      Top = 8
      Width = 199
      Height = 13
      Caption = 'You can download the new version from: '
    end
    object lblURL: TLabel
      Left = 40
      Top = 24
      Width = 267
      Height = 13
      Cursor = crHandPoint
      Caption = 'https://secure.tavultesoft.com/keymandev/downloads/'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblURLClick
    end
    object cmdVisitNow: TButton
      Left = 128
      Top = 72
      Width = 105
      Height = 25
      Caption = '&Visit website now'
      Default = True
      TabOrder = 0
      OnClick = cmdVisitNowClick
    end
    object cmdDownloadLater: TButton
      Left = 240
      Top = 72
      Width = 105
      Height = 25
      Cancel = True
      Caption = '&Later'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object panDownloadDetail: TPanel
    Left = 0
    Top = 60
    Width = 469
    Height = 101
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 44
      Width = 252
      Height = 13
      Caption = 'Do you want to download and install this patch now?'
    end
    object lblInstallDetail: TLabel
      Left = 16
      Top = 16
      Width = 305
      Height = 13
      Caption = 'A ###KB patch can be automatically downloaded and installed.'
    end
    object cmdInstallLater: TButton
      Left = 240
      Top = 76
      Width = 105
      Height = 25
      Cancel = True
      Caption = '&Later'
      ModalResult = 2
      TabOrder = 0
    end
    object cmdInstallNow: TButton
      Left = 128
      Top = 76
      Width = 105
      Height = 25
      Caption = '&Install Now'
      Default = True
      ModalResult = 6
      TabOrder = 1
    end
  end
end
