object Form1: TForm1
  Left = 88
  Top = 73
  Width = 915
  Height = 639
  Caption = 'IEDownload Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 899
    Height = 193
    Align = alTop
    Color = clWhite
    TabOrder = 0
    object btnStart: TButton
      Left = 303
      Top = 7
      Width = 98
      Height = 21
      Caption = 'Download A File'
      TabOrder = 0
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 407
      Top = 7
      Width = 131
      Height = 21
      Caption = 'Stop Current '
      Enabled = False
      TabOrder = 1
      OnClick = btnStopClick
    end
    object rgBind: TRadioGroup
      Left = 711
      Top = 68
      Width = 182
      Height = 41
      Caption = 'DownloadMethod'
      Items.Strings = (
        'Download To File'
        'Download To Stream')
      TabOrder = 2
      OnClick = rgBindClick
    end
    object rgBindMethod: TRadioGroup
      Left = 807
      Top = 3
      Width = 87
      Height = 66
      Caption = 'BindVerbOption'
      Items.Strings = (
        'Get'
        'Post'
        'Put'
        'Custom')
      TabOrder = 3
    end
    object GroupBox1: TGroupBox
      Left = 1
      Top = 113
      Width = 897
      Height = 79
      Align = alBottom
      Caption = 'Download Path:'
      TabOrder = 4
      DesignSize = (
        897
        79)
      object lblProgress: TLabel
        Left = 3
        Top = 39
        Width = 98
        Height = 13
        Caption = ' Download Progress:'
      end
      object ProgressBar1: TProgressBar
        Left = 3
        Top = 55
        Width = 889
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Min = 0
        Max = 100
        TabOrder = 0
      end
      object edtFile: TEdit
        Left = 3
        Top = 12
        Width = 814
        Height = 21
        Color = clBtnFace
        TabOrder = 1
      end
    end
    object GroupBox2: TGroupBox
      Left = 5
      Top = 7
      Width = 292
      Height = 102
      Caption = 'Enter the addresses of the files to download::'
      TabOrder = 5
      object memAddress: TMemo
        Left = 2
        Top = 15
        Width = 288
        Height = 85
        Align = alClient
        Lines.Strings = (
          'http://www.microsoft.com/en/us/default.aspx')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object GroupBox3: TGroupBox
      Left = 404
      Top = 80
      Width = 134
      Height = 29
      Caption = 'State:'
      TabOrder = 6
      object lblState: TLabel
        Left = 48
        Top = 11
        Width = 31
        Height = 13
        Caption = 'Ready'
      end
    end
    object GroupBox4: TGroupBox
      Left = 544
      Top = 7
      Width = 161
      Height = 102
      Caption = 'Options:'
      TabOrder = 7
      object cbOverWrite: TCheckBox
        Left = 3
        Top = 15
        Width = 138
        Height = 17
        Caption = 'OverWrite If File Exists'
        TabOrder = 0
      end
      object cbOpenFolder: TCheckBox
        Left = 3
        Top = 38
        Width = 165
        Height = 17
        Caption = 'Open Folder After Download'
        TabOrder = 1
      end
      object cbAsyn: TCheckBox
        Left = 3
        Top = 61
        Width = 150
        Height = 10
        Caption = 'Asynchronous mode'
        TabOrder = 2
      end
    end
    object btnStopAll: TButton
      Left = 407
      Top = 34
      Width = 131
      Height = 21
      Caption = 'Stop All'
      Enabled = False
      TabOrder = 8
      OnClick = btnStopAllClick
    end
    object btnGoList: TButton
      Left = 301
      Top = 34
      Width = 100
      Height = 21
      Caption = 'Download a List'
      TabOrder = 9
      OnClick = btnGoListClick
    end
    object rgFileExists: TRadioGroup
      Left = 711
      Top = 7
      Width = 90
      Height = 62
      Caption = 'File Exist Options:'
      Items.Strings = (
        'OverWrite'
        'Rename'
        'Skip')
      TabOrder = 10
    end
    object btnToCache: TButton
      Left = 303
      Top = 61
      Width = 98
      Height = 21
      Caption = 'DownloadToCache'
      TabOrder = 11
      OnClick = btnToCacheClick
    end
    object btnToFile: TButton
      Left = 303
      Top = 88
      Width = 98
      Height = 21
      Caption = 'DownloadToFile'
      TabOrder = 12
      OnClick = btnToFileClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 463
    Width = 899
    Height = 140
    Align = alBottom
    Caption = 'Panel3'
    TabOrder = 1
    object ListView: TListView
      Left = 1
      Top = 1
      Width = 897
      Height = 138
      Align = alClient
      Columns = <
        item
          Caption = 'File Name'
          Width = 160
        end
        item
          Caption = 'Speed'
          Width = 75
        end
        item
          Caption = 'Downloaded'
          Width = 75
        end
        item
          Caption = 'Remaining Time'
          Width = 90
        end
        item
          Caption = 'Elapsed Time'
          Width = 90
        end
        item
          Caption = 'Status'
          Width = 100
        end
        item
          Caption = 'Data Recived'
          Width = 160
        end
        item
          Caption = 'Percents'
          Width = 70
        end>
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 193
    Width = 899
    Height = 270
    Align = alClient
    Caption = 'Panel4'
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 897
      Height = 268
      ActivePage = TabSheet1
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Web Browser'
        object EmbeddedWB1: TEmbeddedWB
          Left = 0
          Top = 41
          Width = 889
          Height = 199
          Align = alClient
          TabOrder = 0
          OnBeforeNavigate2 = EmbeddedWB1BeforeNavigate2
          OnNavigateComplete2 = EmbeddedWB1NavigateComplete2
          OnFileDownload = EmbeddedWB1FileDownload
          DisableCtrlShortcuts = 'N'
          UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
          About = ' EmbeddedWB http://bsalsa.com/'
          PrintOptions.Margins.Left = 19.05
          PrintOptions.Margins.Right = 19.05
          PrintOptions.Margins.Top = 19.05
          PrintOptions.Margins.Bottom = 19.05
          PrintOptions.Header = '&w&bPage &p of &P'
          PrintOptions.HTMLHeader.Strings = (
            '<HTML></HTML>')
          PrintOptions.Footer = '&u&b&d'
          PrintOptions.Orientation = poPortrait
          ControlData = {
            4C00000048430000E81B00000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 889
          Height = 41
          Align = alTop
          TabOrder = 1
          object btnGo: TButton
            Left = 808
            Top = 8
            Width = 75
            Height = 25
            Caption = 'Go'
            TabOrder = 0
            OnClick = btnGoClick
          end
          object IEAddress1: TIEAddress
            Left = 2
            Top = 8
            Width = 800
            Height = 22
            About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
            IconLeft = 4
            IconTop = 3
            ItemHeight = 16
            ParentBiDiMode = True
            TabOrder = 1
            TextOnShow = 'http://bsalsa.com/downloads.html'
          end
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Preview Data'
        ImageIndex = 3
        object memPreviewData: TMemo
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet9: TTabSheet
        Caption = 'Preview Stream'
        ImageIndex = 8
        object memPreviewStream: TMemo
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          Lines.Strings = (
            '')
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Events'
        ImageIndex = 2
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          Caption = 'Panel5'
          TabOrder = 0
          object memEvents: TMemo
            Left = 1
            Top = 1
            Width = 887
            Height = 238
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Server Response'
        ImageIndex = 4
        object memResponse: TMemo
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet6: TTabSheet
        Caption = 'Progress Text'
        ImageIndex = 5
        object memProgress: TMemo
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet7: TTabSheet
        Caption = 'Additional Headers'
        ImageIndex = 6
        object memHeaders: TMemo
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'Errors'
        ImageIndex = 7
        object memErrors: TMemo
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          Lines.Strings = (
            '')
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet10: TTabSheet
        Caption = 'Details'
        ImageIndex = 9
        object memDetails: TMemo
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
      object TabSheet11: TTabSheet
        Caption = 'Thread info'
        ImageIndex = 10
        object lvThread: TListView
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          Columns = <
            item
              Caption = 'FileName'
              Width = 150
            end
            item
              Caption = 'Thread ID'
              Width = 100
            end
            item
              Caption = 'Thread Handle'
              Width = 100
            end
            item
              Caption = 'Status'
              Width = 100
            end
            item
              Caption = 'ActiveConnecctions'
              Width = 150
            end>
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Help'
        ImageIndex = 9
        object memSession: TMemo
          Left = 0
          Top = 0
          Width = 889
          Height = 240
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
    end
  end
  object IEDownload1: TIEDownload
    About = ' IEDownload http://bsalsa.com/'
    AdditionalHeader.Strings = (
      'Content-Type: application/x-www-form-urlencoded')
    DefaultProtocol = 'http://'
    DefaultUrlFileName = 'index.html'
    OnAuthenticate = IEDownload1Authenticate
    OnAuthenticateEx = IEDownload1AuthenticateEx
    OnPutProperty = IEDownload1PutProperty
    OnBeforeDownload = IEDownload1BeforeDownload
    OnBeginningTransaction = IEDownload1BeginningTransaction
    OnCodeInstallProblem = IEDownload1CodeInstallProblem
    OnDataAvailable = IEDownload1DataAvailable
    OnDataAvailableInfo = IEDownload1DataAvailableInfo
    OnConnect = IEDownload1Connect
    OnComplete = IEDownload1Complete
    OnStreamComplete = IEDownload1StreamComplete
    OnError = IEDownload1Error
    OnGetBindResults = IEDownload1GetBindResults
    OnGetBindInfo = IEDownload1GetBindInfo
    OnGetBindInfoEx = IEDownload1GetBindInfoEx
    OnGetSerializedClientCertContext = IEDownload1GetSerializedClientCertContext
    OnGetRootSecurityId = IEDownload1GetRootSecurityId
    OnGetWindow = IEDownload1GetWindow
    OnProgress = IEDownload1Progress
    OnRedirect = IEDownload1Redirect
    OnResponse = IEDownload1Response
    OnResume = IEDownload1Resume
    OnSecurityProblem = IEDownload1SecurityProblem
    OnStartBinding = IEDownload1StartBinding
    OnStateChange = IEDownload1StateChange
    OnStopBinding = IEDownload1StopBinding
    TimeOut = 2147483647
    Left = 120
    Top = 72
  end
end
