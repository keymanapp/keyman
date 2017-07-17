object Form2: TForm2
  Left = 376
  Top = 366
  BorderStyle = bsSingle
  Caption = 'Export IE Favorites'
  ClientHeight = 511
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 451
    Width = 569
    Height = 41
    Align = alBottom
    TabOrder = 0
    object Button1: TButton
      Left = 6
      Top = 8
      Width = 121
      Height = 25
      Caption = 'Export Favorites!'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 492
    Width = 569
    Height = 19
    Panels = <>
  end
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 108
    Width = 569
    Height = 223
    Align = alClient
    TabOrder = 2
    Silent = False
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
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
    UserAgent = 'EmbeddedWB 14.55 from: http://www.bsalsa.com/'
    ControlData = {
      4C000000D1310000112A00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 569
    Height = 108
    Align = alTop
    TabOrder = 3
    object Label1: TLabel
      Left = 13
      Top = 12
      Width = 58
      Height = 13
      Caption = 'TargetPath:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 13
      Top = 43
      Width = 79
      Height = 13
      Caption = 'TargetFileName:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 13
      Top = 75
      Width = 53
      Height = 13
      Caption = 'HTML Title:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object ChkNavigateOnComplete: TCheckBox
      Left = 323
      Top = 8
      Width = 153
      Height = 17
      Caption = 'NavigateOnComplete'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 0
    end
    object edTargetPath: TEdit
      Left = 104
      Top = 8
      Width = 170
      Height = 21
      TabOrder = 1
      Text = 'C:\'
    end
    object edHTMLTitle: TEdit
      Left = 104
      Top = 72
      Width = 170
      Height = 21
      TabOrder = 2
      Text = '* Exported Favorites *'
    end
  end
  object ChkExploreFavFileOnComplete: TCheckBox
    Left = 323
    Top = 32
    Width = 185
    Height = 17
    Caption = 'ExploreFavFileOnComplete'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    State = cbChecked
    TabOrder = 4
  end
  object edTargetFileName: TEdit
    Left = 104
    Top = 40
    Width = 170
    Height = 21
    TabOrder = 5
    Text = 'newbook.htm'
  end
  object Log: TListBox
    Left = 0
    Top = 354
    Width = 569
    Height = 97
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 6
  end
  object Panel3: TPanel
    Left = 0
    Top = 331
    Width = 569
    Height = 23
    Align = alBottom
    TabOrder = 7
    object Label4: TLabel
      Left = 13
      Top = 5
      Width = 48
      Height = 13
      Caption = 'Error Log:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object ExportFavorite1: TExportFavorite
    About = 
      'TExportFavorites by bsalsa. Help & Support: http://www.bsalsa.co' +
      'm/'
    ExploreFavFileOnComplete = True
    FavoritesPath = 'Auto'
    NavigateOnComplete = True
    StatusBar = StatusBar1
    SuccessMessage.Strings = (
      #39'Your favorites have been successfully exported to %s'#39)
    TargetFileName = 'newbook.htm'
    TargetPath = 'f:'
    EmbeddedWB = EmbeddedWB1
    Localization.TargetPathInvalid = 'The target path is invalid.'
    Localization.TargetFileNameInvalid = 'The target file name is invalid.'
    Localization.TargetFileNameExtInvalid = 'The target file name extension is invalid. It must be "*.htm".'
    Localization.FavoritesPathInvalid = 'The Favorites Path is invalid.'
    Localization.NoSuccessMessage = 'You must enter a SuccessMessage or turn off messages.'
    Localization.ChangeItMessage = 'Please change it.'
    Localization.HTMLTitle = 'Exported Favorites'
    OnError = ExportFavorite1Error
    OnSuccess = ExportFavorite1Success
    Left = 32
    Top = 120
  end
end
