object frameJSONEditor: TframeJSONEditor
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'frameJSONEditor'
  ClientHeight = 501
  ClientWidth = 679
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TPageControl
    Left = 0
    Top = 0
    Width = 679
    Height = 501
    ActivePage = pageDesign
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    OnChanging = pagesChanging
    object pageDesign: TTabSheet
      Caption = 'Design'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object sbJSON: TScrollBox
        Left = 0
        Top = 0
        Width = 671
        Height = 475
        Align = alClient
        BorderStyle = bsNone
        TabOrder = 0
        object Label1: TLabel
          Left = 24
          Top = 35
          Width = 93
          Height = 13
          Caption = 'Keyboard base URI'
        end
        object Label2: TLabel
          Left = 24
          Top = 62
          Width = 69
          Height = 13
          Caption = 'Font base URI'
        end
        object Label3: TLabel
          Left = 24
          Top = 102
          Width = 72
          Height = 13
          Caption = 'Font resources'
        end
        object Label4: TLabel
          Left = 24
          Top = 230
          Width = 93
          Height = 13
          Caption = 'OSK font resources'
        end
        object lblLanguages: TLabel
          Left = 24
          Top = 353
          Width = 52
          Height = 13
          Caption = 'Languages'
        end
        object lblLanguagesHint: TLabel
          Left = 24
          Top = 372
          Width = 98
          Height = 13
          Caption = '(Recommend 1 only)'
        end
        object editKeyboardBaseURI: TEdit
          Left = 152
          Top = 32
          Width = 225
          Height = 21
          TabOrder = 0
          OnChange = editKeyboardBaseURIChange
        end
        object editFontBaseURI: TEdit
          Left = 152
          Top = 59
          Width = 225
          Height = 21
          TabOrder = 1
          OnChange = editKeyboardBaseURIChange
        end
        object gridFonts: TStringGrid
          Left = 152
          Top = 96
          Width = 320
          Height = 120
          DefaultRowHeight = 16
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goAlwaysShowEditor]
          TabOrder = 2
          OnSetEditText = gridFontsSetEditText
          ColWidths = (
            64
            64
            64
            64
            64)
          RowHeights = (
            16
            16
            16
            16
            16)
        end
        object gridOSKFonts: TStringGrid
          Left = 152
          Top = 222
          Width = 320
          Height = 120
          DefaultRowHeight = 16
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goAlwaysShowEditor]
          TabOrder = 3
          OnSetEditText = gridFontsSetEditText
          ColWidths = (
            64
            64
            64
            64
            64)
          RowHeights = (
            16
            16
            16
            16
            16)
        end
        object cmdAddFont: TButton
          Left = 478
          Top = 96
          Width = 75
          Height = 25
          Caption = 'Add...'
          TabOrder = 4
          OnClick = cmdAddFontClick
        end
        object cmdRemoveFont: TButton
          Left = 478
          Top = 127
          Width = 75
          Height = 25
          Caption = 'Remove'
          TabOrder = 5
          OnClick = cmdRemoveFontClick
        end
        object cmdAddOSKFont: TButton
          Left = 478
          Top = 222
          Width = 75
          Height = 25
          Caption = 'Add...'
          TabOrder = 6
          OnClick = cmdAddOSKFontClick
        end
        object cmdRemoveOSKFont: TButton
          Left = 478
          Top = 253
          Width = 75
          Height = 25
          Caption = 'Remove'
          TabOrder = 7
          OnClick = cmdRemoveOSKFontClick
        end
        object gridLanguages: TStringGrid
          Left = 152
          Top = 348
          Width = 320
          Height = 75
          DefaultRowHeight = 16
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goAlwaysShowEditor]
          TabOrder = 8
          OnSetEditText = gridFontsSetEditText
          ColWidths = (
            64
            64
            64
            64
            64)
          RowHeights = (
            16
            16
            16
            16
            16)
        end
        object cmdAddLanguage: TButton
          Left = 478
          Top = 348
          Width = 75
          Height = 25
          Caption = 'Add...'
          TabOrder = 9
          OnClick = cmdAddLanguageClick
        end
        object cmdRemoveLanguage: TButton
          Left = 478
          Top = 379
          Width = 75
          Height = 25
          Caption = 'Remove'
          TabOrder = 10
          OnClick = cmdRemoveLanguageClick
        end
      end
    end
    object pageCode: TTabSheet
      Caption = 'Code'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
