object Form1: TForm1
  Left = 210
  Top = 156
  Caption = 'IEAddress Demo'
  ClientHeight = 480
  ClientWidth = 635
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
    Width = 635
    Height = 41
    Align = alTop
    TabOrder = 0
    DesignSize = (
      635
      41)
    object Button1: TButton
      Left = 585
      Top = 15
      Width = 41
      Height = 20
      Anchors = [akTop, akRight]
      Caption = 'GO'
      TabOrder = 0
      OnClick = Button1Click
    end
    object IEAddress1: TIEAddress
      Left = 10
      Top = 13
      Width = 569
      Height = 22
      About = 'TIEAddress. Help & Support: http://www.bsalsa.com/'
      IconLeft = 4
      IconTop = 3
      ItemHeight = 16
      OnGetAppIcon = IEAddress1GetAppIcon
      OnGetFavicon = IEAddress1GetFavicon
      ParentBiDiMode = True
      ShowFavicon = True
      TabOrder = 1
      TextOnShow = 'sssssss'
      Themes = tmXP
    end
  end
  object GroupBox1: TGroupBox
    Left = 450
    Top = 41
    Width = 185
    Height = 439
    Align = alRight
    Caption = 'Property Settings'
    TabOrder = 1
    object Label1: TLabel
      Left = 14
      Top = 205
      Width = 89
      Height = 13
      Caption = 'Drop Down Count:'
    end
    object Label3: TLabel
      Left = 6
      Top = 252
      Width = 59
      Height = 13
      Caption = 'DLL Version:'
    end
    object Image1: TImage
      Left = 22
      Top = 329
      Width = 35
      Height = 33
    end
    object Image2: TImage
      Left = 126
      Top = 330
      Width = 35
      Height = 32
    end
    object Label2: TLabel
      Left = 22
      Top = 295
      Width = 3
      Height = 13
    end
    object Label4: TLabel
      Left = 118
      Top = 295
      Width = 3
      Height = 13
    end
    object chkHasDropDown: TCheckBox
      Left = 16
      Top = 44
      Width = 97
      Height = 17
      Caption = 'HasDropDown'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkHasDropDownClick
    end
    object chkRegistryUpdate: TCheckBox
      Left = 16
      Top = 67
      Width = 97
      Height = 17
      Caption = 'RegistryUpdate'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkRegistryUpdateClick
    end
    object chkHasBorder: TCheckBox
      Left = 16
      Top = 21
      Width = 97
      Height = 17
      Caption = 'HasBorder'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = chkHasBorderClick
    end
    object cbShowFavicons: TCheckBox
      Left = 16
      Top = 113
      Width = 95
      Height = 17
      Caption = 'ShowFavicons'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = cbShowFaviconsClick
    end
    object cbShowAppIconToHtml: TCheckBox
      Left = 16
      Top = 90
      Width = 166
      Height = 17
      Caption = 'ShowAppIconOnHtml'
      Checked = True
      State = cbChecked
      TabOrder = 7
      OnClick = cbShowAppIconToHtmlClick
    end
    object chkAutoNavOnSelectLV: TCheckBox
      Left = 16
      Top = 159
      Width = 145
      Height = 17
      Caption = 'AutoNavOnSelectLV'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = chkAutoNavOnSelectLVClick
    end
    object edtDLL: TEdit
      Left = 6
      Top = 268
      Width = 171
      Height = 21
      ReadOnly = True
      TabOrder = 4
    end
    object Button2: TButton
      Left = 71
      Top = 224
      Width = 83
      Height = 22
      Caption = 'Drop Down List'
      TabOrder = 6
      OnClick = Button2Click
    end
    object chkAutoNavigateOnDblClk: TCheckBox
      Left = 16
      Top = 182
      Width = 153
      Height = 17
      Caption = 'AutoNavigateOnDblClk'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object chkAutoNavigateOnEnterKey: TCheckBox
      Left = 16
      Top = 136
      Width = 145
      Height = 17
      Caption = 'AutoNavigateOnEnterKey'
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = chkAutoNavigateOnEnterKeyClick
    end
    object SpinDropDownCount: TSpinEdit
      Left = 14
      Top = 224
      Width = 49
      Height = 22
      EditorEnabled = False
      MaxLength = 3
      MaxValue = 100
      MinValue = 1
      TabOrder = 10
      Value = 15
    end
  end
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 41
    Width = 450
    Height = 439
    Align = alClient
    TabOrder = 2
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.Header = '&w&bPage &p of &P'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    UserAgent = 'MyBrowser'
    ControlData = {
      4C000000822E00005F2D00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object ImageList1: TImageList
    Left = 88
    Top = 328
  end
end
