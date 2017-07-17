object Form1: TForm1
  Left = 269
  Top = 107
  Width = 671
  Height = 428
  Caption = 'IEDownload Simple Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 62
    Width = 655
    Height = 177
    TabStop = False
    Align = alClient
    TabOrder = 0
    OnDownloadComplete = EmbeddedWB1DownloadComplete
    OnBeforeNavigate2 = EmbeddedWB1BeforeNavigate2
    OnFileDownload = EmbeddedWB1FileDownload
    DisableCtrlShortcuts = 'N'
    DownloadOptions = [DownloadImages, DownloadVideos, DownloadBGSounds, DownloadAndIgnoreCache]
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
      4C0000005E330000901A00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 655
    Height = 42
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 520
      Top = 13
      Width = 58
      Height = 22
      Caption = 'Go'
      TabOrder = 0
      OnClick = Button1Click
    end
    object IEAddress1: TEdit
      Left = 0
      Top = 14
      Width = 514
      Height = 21
      TabOrder = 1
      Text = 'http://bsalsa.com/downloads.html'
    end
    object Button2: TButton
      Left = 584
      Top = 13
      Width = 58
      Height = 22
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 239
    Width = 655
    Height = 153
    Align = alBottom
    Caption = 'Panel4'
    TabOrder = 2
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 653
      Height = 151
      ActivePage = TabSheet3
      Align = alClient
      TabOrder = 0
      object TabSheet3: TTabSheet
        Caption = 'Events'
        ImageIndex = 2
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 645
          Height = 123
          Align = alClient
          Caption = 'Panel5'
          TabOrder = 0
          object memEvents: TMemo
            Left = 1
            Top = 1
            Width = 643
            Height = 121
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 0
          end
        end
      end
      object TabSheet8: TTabSheet
        Caption = 'Errors'
        ImageIndex = 7
        object memErrors: TMemo
          Left = 0
          Top = 0
          Width = 645
          Height = 123
          Align = alClient
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
          Width = 645
          Height = 123
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
    end
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 42
    Width = 655
    Height = 20
    Align = alTop
    Min = 0
    Max = 100
    TabOrder = 3
  end
  object IEDownload1: TIEDownload
    About = ' IEDownload http://bsalsa.com/'
    AdditionalHeader.Strings = (
      'Content-Type: application/x-www-form-BindInfoFd ')
    DefaultProtocol = 'http://'
    DefaultUrlFileName = 'index.html'
    OnBeforeDownload = IEDownload1BeforeDownload
    OnComplete = IEDownload1Complete
    OnError = IEDownload1Error
    OnProgress = IEDownload1Progress
    OnStartBinding = IEDownload1StartBinding
    TimeOut = 2147483647
    Left = 16
    Top = 64
  end
end
