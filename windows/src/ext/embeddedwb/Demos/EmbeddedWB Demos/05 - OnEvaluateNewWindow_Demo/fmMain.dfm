object Form1: TForm1
  Left = 162
  Top = 186
  Width = 831
  Height = 521
  Caption = 'TEmbeddedWB - OnEvaluateNewWindow Event Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 815
    Height = 105
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 75
      Width = 774
      Height = 13
      Caption = 
        'Accepts data about a new window that is attempting to display an' +
        'd determines whether that window should be allowed to open based' +
        ' on the user'#39's preferences.'
      Transparent = True
    end
    object Label2: TLabel
      Left = 8
      Top = 56
      Width = 166
      Height = 13
      Caption = 'OnEvaluateNewWindow Event'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      Transparent = True
    end
    object btnGo: TButton
      Left = 423
      Top = 7
      Width = 43
      Height = 21
      Caption = 'Go'
      TabOrder = 0
    end
    object IEAddress1: TIEAddress
      Left = 8
      Top = 7
      Width = 409
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      EmbeddedWB = EmbeddedWB1
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      TabOrder = 1
      TextOnLoad = tlUserDefine
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 466
    Width = 815
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 105
    Width = 815
    Height = 361
    Align = alClient
    TabOrder = 2
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    OnEvaluateNewWindow = EmbeddedWB1EvaluateNewWindow
    About = ' EmbeddedWB http://bsalsa.com/'
    HTMLCode.Strings = (
      'http://www.popupcheck.com/freescan/popup/popup_test_standard.asp')
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
      4C0000000F550000842500000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
