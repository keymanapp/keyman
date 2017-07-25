object Form1: TForm1
  Left = 388
  Top = 235
  Width = 574
  Height = 524
  Caption = 'TEmbeddedWB - 11 - moveto, moveby, resizeto, resizeby Demo'
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 30
    Width = 566
    Height = 441
    Align = alClient
    TabOrder = 0
    OnNewWindow2 = EmbeddedWB1NewWindow2
    OnVisible = EmbeddedWB1Visible
    OnAddressBar = EmbeddedWB1AddressBar
    OnWindowSetResizable = EmbeddedWB1WindowSetResizable
    OnWindowSetLeft = EmbeddedWB1WindowSetLeft
    OnWindowSetTop = EmbeddedWB1WindowSetTop
    OnWindowSetWidth = EmbeddedWB1WindowSetWidth
    OnWindowSetHeight = EmbeddedWB1WindowSetHeight
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnableThemes]
    OnMove = EmbeddedWB1Move
    OnMoveBy = EmbeddedWB1MoveBy
    OnResize = EmbeddedWB1Resize
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
    VisualEffects.TextSize = 0
    UserAgent = 'EmbeddedWB 14,52 from: http://www.bsalsa.com/'
    ControlData = {
      4C00000032310000722900000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object pnlAddressBar: TPanel
    Left = 0
    Top = 0
    Width = 566
    Height = 30
    Align = alTop
    TabOrder = 1
    DesignSize = (
      566
      30)
    object edUrl: TEdit
      Left = 8
      Top = 4
      Width = 507
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'http://devitco.de/coint/navtest'
      OnKeyPress = edUrlKeyPress
    end
    object btnGo: TButton
      Left = 522
      Top = 4
      Width = 33
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Go'
      TabOrder = 1
      OnClick = btnGoClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 471
    Width = 566
    Height = 19
    Panels = <>
    SimplePanel = False
  end
end
