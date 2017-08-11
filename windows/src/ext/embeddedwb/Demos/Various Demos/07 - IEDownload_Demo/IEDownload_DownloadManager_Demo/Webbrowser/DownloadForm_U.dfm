object DownloadForm: TDownloadForm
  Left = 322
  Top = 111
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Downloads'
  ClientHeight = 322
  ClientWidth = 776
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 242
    Width = 776
    Height = 80
    Align = alBottom
    TabOrder = 0
    object btnCancel: TButton
      Left = 680
      Top = 14
      Width = 75
      Height = 19
      Caption = 'Cancel All'
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object chkAutoClose: TCheckBox
      Left = 8
      Top = 6
      Width = 369
      Height = 17
      Caption = 'Close this dialog box when downloads are completed.'
      TabOrder = 1
    end
    object cbRemoveComp: TCheckBox
      Left = 8
      Top = 29
      Width = 369
      Height = 17
      Caption = 'Remove complete downloads'
      TabOrder = 2
      OnClick = cbRemoveCompClick
    end
    object cbExecute: TCheckBox
      Left = 8
      Top = 52
      Width = 145
      Height = 17
      Caption = 'Execute On Complete'
      TabOrder = 3
      OnClick = cbExecuteClick
    end
    object Memo1: TMemo
      Left = 296
      Top = 6
      Width = 273
      Height = 67
      Lines.Strings = (
        'Memo1')
      ScrollBars = ssBoth
      TabOrder = 4
    end
  end
  object ListView: TListView
    Left = 0
    Top = 0
    Width = 776
    Height = 242
    Align = alClient
    Columns = <
      item
        Caption = 'File'
        Width = 120
      end
      item
        Caption = 'Status'
        Width = 120
      end
      item
        Alignment = taRightJustify
        Caption = 'Size'
        Width = 55
      end
      item
        Alignment = taRightJustify
        Caption = 'Progress'
        Width = 55
      end
      item
        Alignment = taRightJustify
        Caption = 'Speed'
        Width = 60
      end
      item
        Caption = 'Remaining'
        Width = 70
      end
      item
        Caption = 'Thread ID'
        Width = 60
      end
      item
        Caption = 'ActiveConnections'
        Width = 90
      end
      item
        Caption = 'Progress bar'
        Width = 150
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = pmDownloadItem
    TabOrder = 1
    ViewStyle = vsReport
    OnContextPopup = ListViewContextPopup
  end
  object IEDownload: TIEDownload
    About = ' IEDownload http://bsalsa.com/'
    AdditionalHeader.Strings = (
      'Content-Type: application/x-www-form-urlencoded')
    BindF = [Asynchronous, AsyncStorage, GetNewestVersion, NoWriteCache, NeedFile]
    DefaultProtocol = 'http://'
    DefaultUrlFileName = 'index.html'
    OnBeforeDownload = IEDownloadBeforeDownload
    OnBeginningTransaction = IEDownloadBeginningTransaction
    OnDataAvailable = IEDownloadDataAvailable
    OnComplete = IEDownloadComplete
    OnError = IEDownloadError
    OnProgress = IEDownloadProgress
    OnStartBinding = IEDownloadStartBinding
    TimeOut = 2147483647
    Left = 16
    Top = 80
  end
  object pmDownloadItem: TPopupMenu
    Left = 48
    Top = 80
    object miCancel: TMenuItem
      Caption = 'Cancel'
      OnClick = miCancelClick
    end
    object miOpen: TMenuItem
      Caption = 'Open'
      Enabled = False
      OnClick = miOpenClick
    end
  end
end
