object Form1: TForm1
  Left = 386
  Top = 355
  Width = 690
  Height = 514
  Caption = 'Drag And Drop Demo'
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
    Width = 682
    Height = 97
    Align = alTop
    TabOrder = 0
    DesignSize = (
      682
      97)
    object Label1: TLabel
      Left = 10
      Top = 35
      Width = 618
      Height = 24
      Caption = 
        'Drag a file over the EmbeddedWB and you will see the events orde' +
        'r. '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -20
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 10
      Top = 65
      Width = 653
      Height = 18
      Caption = 
        'Note: You must place the EmbeddedWB on a panel or another contro' +
        'l and not directly on the form.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Button1: TButton
      Left = 600
      Top = 8
      Width = 57
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'GO'
      TabOrder = 0
      OnClick = Button1Click
    end
    object IEAddress1: TIEAddress
      Left = 10
      Top = 7
      Width = 569
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      DragMode = dmAutomatic
      EmbeddedWB = EmbeddedWB1
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      ParentBiDiMode = True
      ShowFavicon = True
      TabOrder = 1
      UseAppIcon = True
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 280
    Width = 682
    Height = 200
    Align = alBottom
    TabOrder = 1
    object Memo1: TMemo
      Left = 1
      Top = 12
      Width = 680
      Height = 187
      Align = alBottom
      Lines.Strings = (
        '')
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 97
    Width = 682
    Height = 183
    Align = alClient
    Caption = 'Panel3'
    TabOrder = 2
    object EmbeddedWB1: TEmbeddedWB
      Left = 1
      Top = 1
      Width = 680
      Height = 181
      Align = alClient
      DragCursor = crHandPoint
      TabOrder = 0
      Silent = False
      DisableCtrlShortcuts = 'N'
      UserInterfaceOptions = [EnableThemes]
      DropHandlingType = ddtCustom
      OnGetDropTarget = EmbeddedWB1GetDropTarget
      OnDragEnter = EmbeddedWB1DragEnter
      OnDragLeave = EmbeddedWB1DragLeave
      OnDragOver2 = EmbeddedWB1DragOver2
      OnDropEvent = EmbeddedWB1DropEvent
      About = ' EmbeddedWB http://bsalsa.com/'
      PrintOptions.Margins.Left = 19.050000000000000000
      PrintOptions.Margins.Right = 19.050000000000000000
      PrintOptions.Margins.Top = 19.050000000000000000
      PrintOptions.Margins.Bottom = 19.050000000000000000
      PrintOptions.Header = '&w&bSeite &p von &P'
      PrintOptions.HTMLHeader.Strings = (
        '<HTML></HTML>')
      PrintOptions.Footer = '&u&b&d'
      PrintOptions.Orientation = poPortrait
      UserAgent = 'EmbeddedWB 14.52 from: http://www.bsalsa.com/'
      ControlData = {
        4C000000712C0000582600000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126208000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
end
