inherited frmKeyboardFonts: TfrmKeyboardFonts
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Keyboard Fonts'
  ClientHeight = 322
  ClientWidth = 512
  Position = poScreenCenter
  ExplicitWidth = 518
  ExplicitHeight = 351
  PixelsPerInch = 96
  TextHeight = 13
  object lblDevEnv: TLabel
    Left = 16
    Top = 19
    Width = 151
    Height = 13
    Caption = 'Development Environment'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblCodeFont: TLabel
    Left = 32
    Top = 44
    Width = 81
    Height = 13
    Caption = 'Text Editor Code'
  end
  object lblDevEnvName: TLabel
    Left = 176
    Top = 19
    Width = 51
    Height = 13
    Caption = 'Font name'
  end
  object lblDevEnvSize: TLabel
    Left = 391
    Top = 19
    Width = 43
    Height = 13
    Caption = 'Font size'
  end
  object lblCharFont: TLabel
    Left = 32
    Top = 71
    Width = 138
    Height = 13
    Caption = 'Debugger, Editor Characters'
  end
  object lblOSKFont: TLabel
    Left = 32
    Top = 127
    Width = 99
    Height = 13
    Caption = 'On Screen Keyboard'
  end
  object lblTouchLayoutPhoneFont: TLabel
    Left = 32
    Top = 154
    Width = 105
    Height = 13
    Caption = 'Touch Layout - Phone'
  end
  object lblTouchLayoutTabletFont: TLabel
    Left = 32
    Top = 181
    Width = 105
    Height = 13
    Caption = 'Touch Layout - Tablet'
  end
  object lblTouchLayoutDesktopFont: TLabel
    Left = 32
    Top = 208
    Width = 114
    Height = 13
    Caption = 'Touch Layout - Desktop'
  end
  object lblProp: TLabel
    Left = 16
    Top = 102
    Width = 116
    Height = 13
    Caption = 'Keyboard Properties'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblPropName: TLabel
    Left = 176
    Top = 102
    Width = 51
    Height = 13
    Caption = 'Font name'
  end
  object lblPropSize: TLabel
    Left = 391
    Top = 102
    Width = 43
    Height = 13
    Caption = 'Font size'
  end
  object lblOSKFontSize: TLabel
    Left = 463
    Top = 127
    Width = 10
    Height = 13
    Caption = 'pt'
  end
  object lblTextEditorFontSize: TLabel
    Left = 463
    Top = 44
    Width = 10
    Height = 13
    Caption = 'pt'
  end
  object lblDebuggerFontSize: TLabel
    Left = 463
    Top = 71
    Width = 10
    Height = 13
    Caption = 'pt'
  end
  object lblTouchLayoutPhoneFontSize: TLabel
    Left = 463
    Top = 154
    Width = 14
    Height = 13
    Caption = 'em'
  end
  object Label1: TLabel
    Left = 463
    Top = 181
    Width = 14
    Height = 13
    Caption = 'em'
  end
  object Label2: TLabel
    Left = 463
    Top = 208
    Width = 14
    Height = 13
    Caption = 'em'
  end
  object lblDisplayMap: TLabel
    Left = 32
    Top = 248
    Width = 109
    Height = 13
    Caption = '&displayMap target font'
  end
  object Label4: TLabel
    Left = 463
    Top = 248
    Width = 14
    Height = 13
    Caption = 'em'
  end
  object fcbCode: TscFontComboBox
    Left = 176
    Top = 41
    Width = 209
    Height = 24
    DropDownCount = 12
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    TabOrder = 0
    MoveUsedToTop = False
    ColorUsed = clWindow
    PreviewWidth = 240
    PreviewHeight = 60
    FontTypes = [ftTrueTypeAnsi, ftTrueTypeSymbol]
    ShowPreview = False
    ShowPreviewFontName = False
    ShowPreviewInList = False
  end
  object editCodeSize: TEdit
    Left = 391
    Top = 41
    Width = 66
    Height = 21
    TabOrder = 1
  end
  object fcbChar: TscFontComboBox
    Left = 176
    Top = 68
    Width = 209
    Height = 24
    DropDownCount = 12
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    TabOrder = 2
    MoveUsedToTop = False
    ColorUsed = clWindow
    PreviewWidth = 240
    PreviewHeight = 60
    FontTypes = [ftTrueTypeAnsi, ftTrueTypeSymbol]
    ShowPreview = False
    ShowPreviewFontName = False
    ShowPreviewInList = False
  end
  object editCharSize: TEdit
    Left = 391
    Top = 68
    Width = 66
    Height = 21
    TabOrder = 3
  end
  object fcbOSK: TscFontComboBox
    Left = 176
    Top = 124
    Width = 209
    Height = 24
    DropDownCount = 12
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    TabOrder = 4
    MoveUsedToTop = False
    ColorUsed = clWindow
    PreviewWidth = 240
    PreviewHeight = 60
    FontTypes = [ftTrueTypeAnsi, ftTrueTypeSymbol]
    ShowPreview = False
    ShowPreviewFontName = False
    ShowPreviewInList = False
  end
  object editOSKSize: TEdit
    Left = 391
    Top = 124
    Width = 66
    Height = 21
    TabOrder = 5
  end
  object editTouchLayoutPhoneSize: TEdit
    Left = 391
    Top = 151
    Width = 66
    Height = 21
    TabOrder = 7
  end
  object editTouchLayoutTabletSize: TEdit
    Left = 391
    Top = 178
    Width = 66
    Height = 21
    TabOrder = 9
  end
  object editTouchLayoutDesktopSize: TEdit
    Left = 391
    Top = 205
    Width = 66
    Height = 21
    TabOrder = 11
  end
  object cmdOK: TButton
    Left = 162
    Top = 280
    Width = 91
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 14
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 259
    Top = 280
    Width = 90
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 15
  end
  object fcbTouchLayoutPhone: TscFontComboBox
    Left = 176
    Top = 151
    Width = 209
    Height = 24
    DropDownCount = 12
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    TabOrder = 6
    MoveUsedToTop = False
    ColorUsed = clWindow
    PreviewWidth = 240
    PreviewHeight = 60
    FontTypes = [ftTrueTypeAnsi, ftTrueTypeSymbol]
    ShowPreview = False
    ShowPreviewFontName = False
    ShowPreviewInList = False
  end
  object fcbTouchLayoutTablet: TscFontComboBox
    Left = 176
    Top = 178
    Width = 209
    Height = 24
    DropDownCount = 12
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    TabOrder = 8
    MoveUsedToTop = False
    ColorUsed = clWindow
    PreviewWidth = 240
    PreviewHeight = 60
    FontTypes = [ftTrueTypeAnsi, ftTrueTypeSymbol]
    ShowPreview = False
    ShowPreviewFontName = False
    ShowPreviewInList = False
  end
  object fcbTouchLayoutDesktop: TscFontComboBox
    Left = 176
    Top = 205
    Width = 209
    Height = 24
    DropDownCount = 12
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    TabOrder = 10
    MoveUsedToTop = False
    ColorUsed = clWindow
    PreviewWidth = 240
    PreviewHeight = 60
    FontTypes = [ftTrueTypeAnsi, ftTrueTypeSymbol]
    ShowPreview = False
    ShowPreviewFontName = False
    ShowPreviewInList = False
  end
  object editDisplayMapSize: TEdit
    Left = 391
    Top = 245
    Width = 66
    Height = 21
    TabOrder = 13
  end
  object fcbDisplayMap: TscFontComboBox
    Left = 176
    Top = 245
    Width = 209
    Height = 24
    DropDownCount = 12
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 18
    ParentFont = False
    TabOrder = 12
    MoveUsedToTop = False
    ColorUsed = clWindow
    PreviewWidth = 240
    PreviewHeight = 60
    FontTypes = [ftTrueTypeAnsi, ftTrueTypeSymbol]
    ShowPreview = False
    ShowPreviewFontName = False
    ShowPreviewInList = False
  end
end
