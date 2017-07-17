object DownloadForm: TDownloadForm
  Left = 322
  Top = 111
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Downloads'
  ClientHeight = 178
  ClientWidth = 663
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lvDownloads: TListView
    Left = 0
    Top = 0
    Width = 663
    Height = 121
    Align = alClient
    Columns = <
      item
        Caption = 'File'
        Width = 140
      end
      item
        Caption = 'Status'
        Width = 150
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
        Width = 70
      end
      item
        Caption = 'Remaining'
        Width = 80
      end
      item
        Caption = 'Progressbar'
        Width = 80
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnContextPopup = lvDownloadsContextPopup
  end
  object Panel1: TPanel
    Left = 0
    Top = 121
    Width = 663
    Height = 57
    Align = alBottom
    TabOrder = 1
    object btnCancel: TButton
      Left = 464
      Top = 29
      Width = 75
      Height = 19
      Caption = 'Cancel All'
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object chkAutoClose: TCheckBox
      Left = 8
      Top = 32
      Width = 369
      Height = 17
      Caption = 'Close this dialog box when downloads are completed'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 6
      Width = 531
      Height = 17
      TabOrder = 2
    end
  end
  object IEDownload: TIEDownload
    AdditionalHeader.Strings = (
      'Content-Type: application/x-www-form-urlencoded')
    DefaultProtocol = 'http://'
    DefaultUrlFileName = 'index.html'
    DownloadMethod = dlFile
    OnBeginningTransaction = IEDownloadBeginningTransaction
    OnBinding = IEDownloadBinding
    OnDownloadComplete = IEDownloadComplete
    OnProgress = IEDownloadProgress
    Options = [Asynchronous, AsyncStorage, GetNewestVersion, NoWriteCache, PullData, Pragma_No_Cache]
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
