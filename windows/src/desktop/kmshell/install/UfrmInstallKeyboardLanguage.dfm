inherited frmInstallKeyboardLanguage: TfrmInstallKeyboardLanguage
  ActiveControl = editSearch
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add Language to Keyboard'
  ClientHeight = 432
  ClientWidth = 600
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Tahoma'
  Position = poScreenCenter
  ExplicitWidth = 606
  ExplicitHeight = 461
  PixelsPerInch = 96
  TextHeight = 13
  object lblLanguage: TLabel
    Left = 8
    Top = 11
    Width = 47
    Height = 13
    Caption = 'Language'
  end
  object cmdCancel: TButton
    Left = 517
    Top = 399
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object cmdOK: TButton
    Left = 436
    Top = 399
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = cmdOKClick
  end
  object editSearch: TEdit
    Left = 72
    Top = 8
    Width = 370
    Height = 21
    TabOrder = 0
    TextHint = 'Search for language'
    OnChange = editSearchChange
    OnKeyDown = editSearchKeyDown
  end
  object gridLanguages: TStringGrid
    Left = 73
    Top = 32
    Width = 519
    Height = 170
    ColCount = 4
    DefaultRowHeight = 20
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
    TabOrder = 1
    OnClick = gridLanguagesClick
    OnDrawCell = gridLanguagesDrawCell
    ColWidths = (
      64
      64
      64
      64)
    RowHeights = (
      20
      20
      20
      20
      20)
  end
  object gridLanguageVariants: TStringGrid
    Left = 73
    Top = 208
    Width = 519
    Height = 185
    ColCount = 4
    DefaultRowHeight = 20
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 2
    ColWidths = (
      64
      64
      64
      64)
    RowHeights = (
      20
      20
      20
      20
      20)
  end
end
