object frmMultiDownloader: TfrmMultiDownloader
  Left = 299
  Top = 296
  Caption = 'Multi Downloader Demo'
  ClientHeight = 443
  ClientWidth = 709
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 424
    Width = 709
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Style = psOwnerDraw
        Text = 'D/L Progress:'
        Width = 350
      end>
    OnDrawPanel = StatusBar1DrawPanel
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 709
    Height = 41
    Align = alTop
    TabOrder = 1
    object btnGo: TButton
      Left = 597
      Top = 13
      Width = 42
      Height = 22
      Caption = 'Go'
      TabOrder = 0
      OnClick = btnGoClick
    end
    object edtAddress: TEdit
      Left = 5
      Top = 14
      Width = 580
      Height = 21
      TabOrder = 1
      Text = 'http://bsalsa.com'
    end
    object btnCancel: TButton
      Left = 645
      Top = 13
      Width = 49
      Height = 22
      Caption = 'Cancel'
      Enabled = False
      TabOrder = 2
      OnClick = btnCancelClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 709
    Height = 383
    Align = alClient
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 707
      Height = 381
      ActivePage = tsDownloads
      Align = alClient
      TabOrder = 0
      object tsEvents: TTabSheet
        Caption = 'Events'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 699
          Height = 353
          Align = alClient
          Caption = 'Panel5'
          TabOrder = 0
          object memEvents: TMemo
            Left = 1
            Top = 1
            Width = 697
            Height = 351
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Server Response'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memResponse: TMemo
          Left = 0
          Top = 0
          Width = 699
          Height = 353
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'Progress Text'
        ImageIndex = 5
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memProgress: TMemo
          Left = 0
          Top = 0
          Width = 699
          Height = 353
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'Errors'
        ImageIndex = 7
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memErrors: TMemo
          Left = 0
          Top = 0
          Width = 699
          Height = 353
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet10: TTabSheet
        Caption = 'Details'
        ImageIndex = 9
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memDetails: TMemo
          Left = 0
          Top = 0
          Width = 699
          Height = 353
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabImages: TTabSheet
        Caption = 'Image List'
        ImageIndex = 7
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memImages: TMemo
          Left = 0
          Top = 0
          Width = 699
          Height = 353
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabLinks: TTabSheet
        Caption = 'Link List'
        ImageIndex = 8
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memLinks: TMemo
          Left = 0
          Top = 0
          Width = 699
          Height = 353
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsDownloads: TTabSheet
        Caption = 'Downloads'
        ImageIndex = 7
        object memDownloads: TMemo
          Left = 0
          Top = 0
          Width = 699
          Height = 353
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object tsItems: TTabSheet
        Caption = 'Items'
        ImageIndex = 8
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object memItems: TMemo
          Left = 0
          Top = 0
          Width = 699
          Height = 353
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
    end
  end
  object ProgressBar1: TProgressBar
    Left = 296
    Top = 392
    Width = 350
    Height = 17
    TabOrder = 3
  end
  object IEMultiDownload1: TIEMultiDownload
    About = 'TIEMultiDownload from: http://www.bsalsa.com'
    AdditionalHeader.Strings = (
      'Content-Type: application/x-www-form-BindInfoFd ')
    DefaultProtocol = 'http://'
    DefaultUrlFileName = 'index.html'
    OnBeforeDownload = IEMultiDownload1BeforeDownload
    OnDataAvailableInfo = IEMultiDownload1DataAvailableInfo
    OnError = IEMultiDownload1Error
    OnProgress = IEMultiDownload1Progress
    OnResponse = IEMultiDownload1Response
    OnStateChange = IEMultiDownload1StateChange
    TimeOut = 2147483647
    DownloadOptions = doPages
    GetCompleteBaseSite = True
    Items = <>
    OnMultiBeforeDownload = IEMultiDownload1MultiBeforeDownload
    OnMultiComplete = IEMultiDownload1MultiComplete
    OnMultiGetDocInfo = IEMultiDownload1MultiGetDocInfo
    OnMultiGetImage = IEMultiDownload1MultiGetImage
    OnMultiGetLink = IEMultiDownload1MultiGetLink
    OnMultiGetQueryInfo = IEMultiDownload1MultiGetQueryInfo
    OnMultiItemAdded = IEMultiDownload1MultiItemAdded
    OnMultiParseComplete = IEMultiDownload1MultiParseComplete
    OnMultiParseDocument = IEMultiDownload1MultiParseDocument
    OnMultiParseError = IEMultiDownload1MultiParseError
    OnMultiParseProgress = IEMultiDownload1MultiParseProgress
    OnMultiStateChange = IEMultiDownload1MultiStateChange
    OnMultiStartParsing = IEMultiDownload1MultiStartParsing
    Left = 32
    Top = 80
  end
end
