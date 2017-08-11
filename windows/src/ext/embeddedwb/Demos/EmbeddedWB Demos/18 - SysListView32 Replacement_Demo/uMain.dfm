object Form1: TForm1
  Left = 305
  Top = 362
  Width = 696
  Height = 480
  Caption = 'EmbeddedWB SysListView Replacement Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Go "c:\"'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object BitBtn2: TBitBtn
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Go Google'
      TabOrder = 1
      OnClick = BitBtn2Click
    end
  end
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 41
    Width = 688
    Height = 405
    Align = alClient
    TabOrder = 1
    Silent = False
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    OnPreRefresh = EmbeddedWB1PreRefresh
    About = ' EmbeddedWB http://bsalsa.com/'
    HTMLCode.Strings = (
      'www.google.com')
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.Header = '&w&bSeite &p von &P'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    ControlData = {
      4C0000001B470000DC2900000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object ListView1: TListView
    Left = 14
    Top = 272
    Width = 163
    Height = 153
    Columns = <
      item
        Width = 300
      end
      item
        Width = 150
      end>
    SmallImages = ImageList1
    TabOrder = 2
    ViewStyle = vsReport
    Visible = False
  end
  object ImageList1: TImageList
    Left = 24
    Top = 216
  end
end
