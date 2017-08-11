object Form1: TForm1
  Left = 353
  Top = 207
  Width = 684
  Height = 586
  Caption = 'bsalsa - Guid Creator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 676
    Height = 193
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 121
      Top = 44
      Width = 73
      Height = 13
      Caption = 'Headers Folder'
    end
    object Label2: TLabel
      Left = 134
      Top = 64
      Width = 60
      Height = 13
      Caption = 'SaveFileAs..'
    end
    object Label3: TLabel
      Left = 134
      Top = 83
      Width = 60
      Height = 13
      Caption = 'SaveFileAs..'
    end
    object LinkLabel1: TLinkLabel
      Left = 1
      Top = 1
      Width = 674
      Height = 29
      AutoSize = False
      TextRun.Enable = True
      Launch.AsHttp.Address = 'bsalsa.com/ie_guid.html'
      TextEffects = teRunning
      Layout = tlTop
      Align = alTop
      Caption = 'Instructions'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
    end
    object LinkLabel3: TLinkLabel
      Left = 17
      Top = 44
      Width = 87
      Height = 29
      AutoSize = False
      Launch.AsHttp.Address = 
        'msdn.microsoft.com/archive/default.asp?url=/archive/en-us/sample' +
        's/internet/libraries/ie6_lib/default.asp'
      Caption = 'DownloadHeaders'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
    end
    object btnIEGuid: TButton
      Left = 615
      Top = 53
      Width = 48
      Height = 21
      Caption = 'Browse'
      TabOrder = 0
      OnClick = btnIEGuidClick
    end
    object btnHPath: TButton
      Left = 615
      Top = 25
      Width = 48
      Height = 22
      Caption = 'Browse'
      TabOrder = 1
      OnClick = btnHPathClick
    end
    object btnCreateIEGuid: TButton
      Left = 17
      Top = 63
      Width = 97
      Height = 21
      Caption = 'CreateIEGuid'
      TabOrder = 2
      OnClick = btnCreateIEGuidClick
    end
    object btnCreateIEList: TButton
      Left = 17
      Top = 82
      Width = 97
      Height = 21
      Caption = 'CreateIEList'
      TabOrder = 3
      OnClick = btnCreateIEListClick
    end
    object edtHPath: TEdit
      Left = 200
      Top = 37
      Width = 409
      Height = 21
      TabOrder = 4
    end
    object edtIEGuid: TEdit
      Left = 200
      Top = 61
      Width = 409
      Height = 21
      TabOrder = 5
    end
    object btnIEList: TButton
      Left = 615
      Top = 80
      Width = 48
      Height = 21
      Caption = 'Browse'
      TabOrder = 6
      OnClick = btnIEListClick
    end
    object edtIEList: TEdit
      Left = 200
      Top = 83
      Width = 409
      Height = 21
      TabOrder = 7
    end
    object cbOpenNotepad: TCheckBox
      Left = 17
      Top = 110
      Width = 184
      Height = 17
      Caption = 'Auto open in notepad when done'
      TabOrder = 8
    end
    object GroupBox1: TGroupBox
      Left = 4
      Top = 133
      Width = 659
      Height = 54
      Caption = 'Use after file created'
      Enabled = False
      TabOrder = 9
      object btnGetInterfaces: TButton
        Left = 281
        Top = 24
        Width = 82
        Height = 17
        Caption = 'GetInterfaces'
        TabOrder = 0
        OnClick = btnGetInterfacesClick
      end
      object btnConnectionP: TButton
        Left = 186
        Top = 22
        Width = 89
        Height = 19
        Caption = 'ConnectionPoints'
        TabOrder = 1
        OnClick = btnConnectionPClick
      end
      object btnGetServices: TButton
        Left = 369
        Top = 24
        Width = 75
        Height = 17
        Caption = 'GetServices'
        TabOrder = 2
        OnClick = btnGetServicesClick
      end
      object Button1: TButton
        Left = 13
        Top = 23
        Width = 167
        Height = 18
        Caption = 'Open Memo content In Notepad'
        TabOrder = 3
        OnClick = Button1Click
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 193
    Width = 676
    Height = 359
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Memo1'
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 668
        Height = 331
        Align = alClient
        Lines.Strings = (
          '')
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'EmbeddedWB'
      ImageIndex = 1
      object EmbeddedWB1: TEmbeddedWB
        Left = 0
        Top = 29
        Width = 668
        Height = 302
        Align = alClient
        TabOrder = 0
        DisableCtrlShortcuts = 'N'
        UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
        OnQueryService = EmbeddedWB1QueryService
        About = ' EmbeddedWB http://bsalsa.com/'
        PrintOptions.Margins.Left = 19.05
        PrintOptions.Margins.Right = 19.05
        PrintOptions.Margins.Top = 19.05
        PrintOptions.Margins.Bottom = 19.05
        PrintOptions.Header = '&w&bSeite &p von &P'
        PrintOptions.HTMLHeader.Strings = (
          '<HTML></HTML>')
        PrintOptions.Footer = '&u&b&d'
        PrintOptions.Orientation = poPortrait
        UserAgent = 'Mozilla/4.0(Compatible-EmbeddedWB 14.58 http://bsalsa.com/ '
        ControlData = {
          4C0000000A450000F91D00000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 668
        Height = 29
        Align = alTop
        Caption = 'Panel2'
        TabOrder = 1
        object IEAddress1: TIEAddress
          Left = 0
          Top = 5
          Width = 578
          Height = 22
          AutoNavigateOnLoad = False
          About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
          EmbeddedWB = EmbeddedWB1
          IconLeft = 4
          IconTop = 3
          ItemHeight = 16
          ParentBiDiMode = True
          TabOrder = 0
        end
        object btnGo: TButton
          Left = 584
          Top = 8
          Width = 75
          Height = 17
          Caption = 'Go'
          TabOrder = 1
          OnClick = btnGoClick
        end
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 624
    Top = 288
  end
end
