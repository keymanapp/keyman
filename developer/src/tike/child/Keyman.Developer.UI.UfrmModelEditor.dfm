inherited frmModelEditor: TfrmModelEditor
  Left = 0
  Top = 0
  Caption = 'frmModelEditor'
  ClientHeight = 708
  ClientWidth = 712
  Font.Name = 'Tahoma'
  Position = poDesigned
  ExplicitWidth = 712
  ExplicitHeight = 708
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TLeftTabbedPageControl
    Left = 0
    Top = 0
    Width = 712
    Height = 708
    ActivePage = pageDetails
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    Images = modActionsMain.ilEditorPages
    MultiLine = True
    OwnerDraw = True
    ParentFont = False
    TabHeight = 90
    TabOrder = 0
    TabPosition = tpLeft
    TabWidth = 60
    OnChanging = pagesChanging
    object pageDetails: TTabSheet
      Caption = 'Details'
      ImageIndex = 2
      object sbDetails: TScrollBox
        Left = 0
        Top = 0
        Width = 619
        Height = 708
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = 14211288
        ParentColor = False
        TabOrder = 0
        OnMouseWheel = sbDetailsMouseWheel
        object panWordlists: TPanel
          AlignWithMargins = True
          Left = 4
          Top = 279
          Width = 611
          Height = 232
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitTop = 261
          DesignSize = (
            611
            232)
          object lblWordlists: TLabel
            AlignWithMargins = True
            Left = 12
            Top = 4
            Width = 64
            Height = 17
            Caption = 'Wordlists'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblReadOnly: TLabel
            Left = 15
            Top = 200
            Width = 415
            Height = 13
            Caption =
              'The editor was unable to parse the source file. Details shown he' +
              're are read-only.'
          end
          object gridWordlists: TStringGrid
            Left = 12
            Top = 27
            Width = 485
            Height = 160
            Anchors = [akLeft, akTop, akRight]
            ColCount = 1
            FixedCols = 0
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
            TabOrder = 0
            OnDblClick = gridWordlistsDblClick
            ColWidths = (
              271)
            RowHeights = (
              24
              24
              24
              24
              24)
          end
          object cmdAddWordlist: TButton
            Left = 503
            Top = 27
            Width = 101
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '&Add...'
            TabOrder = 1
            OnClick = cmdAddWordlistClick
          end
          object cmdRemoveWordlist: TButton
            Left = 503
            Top = 58
            Width = 100
            Height = 25
            Anchors = [akTop, akRight]
            Caption = 'Remove'
            TabOrder = 2
            OnClick = cmdRemoveWordlistClick
          end
        end
        object panBasicInformation: TPanel
          AlignWithMargins = True
          Left = 4
          Top = 6
          Width = 611
          Height = 265
          Margins.Left = 4
          Margins.Top = 6
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 0
          DesignSize = (
            611
            265)
          object lblFormat: TLabel
            Left = 12
            Top = 30
            Width = 36
            Height = 13
            Caption = '&Format'
            FocusControl = cbFormat
          end
          object lblBasicInformation: TLabel
            AlignWithMargins = True
            Left = 12
            Top = 3
            Width = 123
            Height = 17
            Caption = 'Basic Information'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lblWordBreaker: TLabel
            Left = 12
            Top = 56
            Width = 71
            Height = 13
            Caption = '&Word breaker'
          end
          object lblComments: TLabel
            Left = 12
            Top = 181
            Width = 50
            Height = 13
            Caption = 'Comme&nts'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object lblInsertAfterWord: TLabel
            Left = 12
            Top = 83
            Width = 86
            Height = 13
            Caption = '&Insert after word'
            FocusControl = cbInsertAfterWord
          end
          object lblQuotationMarks: TLabel
            Left = 12
            Top = 110
            Width = 86
            Height = 13
            Caption = '&Quotation marks'
            FocusControl = cbOpenQuote
          end
          object lblOpenQuote: TLabel
            Left = 106
            Top = 110
            Width = 27
            Height = 13
            Caption = 'open'
            FocusControl = cbOpenQuote
          end
          object lblCloseQuote: TLabel
            Left = 220
            Top = 110
            Width = 26
            Height = 13
            Caption = 'close'
            FocusControl = cbCloseQuote
          end
          object cbFormat: TComboBox
            Left = 106
            Top = 26
            Width = 145
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnClick = cbFormatClick
            Items.Strings = (
              'Wordlist (trie-1.0)'
              'Custom (custom-1.0)')
          end
          object cbWordBreaker: TComboBox
            Left = 106
            Top = 53
            Width = 145
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            OnClick = cbWordBreakerClick
            Items.Strings = (
              'default'
              'ascii'
              'custom')
          end
          object memoComments: TMemo
            Left = 106
            Top = 178
            Width = 498
            Height = 87
            Anchors = [akLeft, akTop, akRight]
            BevelOuter = bvNone
            TabOrder = 7
            OnChange = memoCommentsChange
          end
          object cbInsertAfterWord: TComboBox
            Left = 106
            Top = 80
            Width = 145
            Height = 21
            TabOrder = 2
            OnChange = cbInsertAfterWordClick
            OnClick = cbInsertAfterWordClick
            OnKeyUp = cbInsertAfterWordKeyUp
          end
          object cbOpenQuote: TComboBox
            Left = 139
            Top = 107
            Width = 63
            Height = 21
            TabOrder = 3
            OnChange = cbOpenQuoteClick
            OnClick = cbOpenQuoteClick
            OnKeyUp = cbOpenQuoteKeyUp
          end
          object chkIsRTL: TCheckBox
            Left = 106
            Top = 134
            Width = 111
            Height = 17
            Caption = 'Right-to-left script'
            TabOrder = 5
            OnClick = chkIsRTLClick
          end
          object cbCloseQuote: TComboBox
            Left = 252
            Top = 107
            Width = 63
            Height = 21
            TabOrder = 4
            OnChange = cbCloseQuoteClick
            OnClick = cbCloseQuoteClick
            OnKeyUp = cbCloseQuoteKeyUp
          end
          object chkLanguageUsesCasing: TCheckBox
            Left = 106
            Top = 155
            Width = 145
            Height = 17
            Caption = 'Language uses casing'
            TabOrder = 6
            OnClick = chkLanguageUsesCasingClick
          end
        end
      end
    end
    object pageSource: TTabSheet
      Caption = 'Source'
      ImageIndex = 9
    end
    object pageCompile: TTabSheet
      Caption = 'Build'
      ImageIndex = 1
      object sbCompile: TScrollBox
        Left = 0
        Top = 0
        Width = 619
        Height = 708
        Align = alClient
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = 14211288
        ParentColor = False
        TabOrder = 0
        OnMouseWheel = sbDetailsMouseWheel
        object lblCongrats: TLabel
          Left = 10
          Top = 13
          Width = 333
          Height = 13
          Caption =
            'The model must be compiled in order to distribute or install ' +
            'it'
        end
        object panBuildLexicalModel: TPanel
          Left = 10
          Top = 235
          Width = 471
          Height = 353
          BevelOuter = bvNone
          Color = 15921906
          ParentBackground = False
          TabOrder = 2
          object lblDebugHostCaption: TLabel
            Left = 12
            Top = 158
            Width = 222
            Height = 13
            Caption = 'Server is active at following web addresses:'
          end
          object lblCrossPlatform: TLabel
            Left = 12
            Top = 14
            Width = 237
            Height = 17
            Caption = 'Test Lexical Model in Web Browser'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label1: TLabel
            Left = 12
            Top = 48
            Width = 105
            Height = 13
            Caption = 'Keyboard for testing'
          end
          object Label2: TLabel
            Left = 12
            Top = 72
            Width = 389
            Height = 26
            Caption =
              'Optionally, select a compiled keyboard with which to test this l' +
              'exical model. Any keyboards already loaded in the web debugger w' +
              'ill also be available.'
            WordWrap = True
          end
          object imgQRCode: TImage
            Left = 309
            Top = 152
            Width = 128
            Height = 128
            Proportional = True
            Stretch = True
          end
          object cmdTestLexicalModel: TButton
            Left = 12
            Top = 122
            Width = 150
            Height = 25
            Action = modActionsModelEditor.actModelTest
            TabOrder = 2
          end
          object cmdOpenDebugHost: TButton
            Left = 12
            Top = 286
            Width = 109
            Height = 25
            Caption = 'Open in browser'
            TabOrder = 5
            OnClick = cmdOpenDebugHostClick
          end
          object lbDebugHosts: TListBox
            Left = 12
            Top = 183
            Width = 289
            Height = 97
            ItemHeight = 13
            TabOrder = 4
            OnClick = lbDebugHostsClick
          end
          object cmdSendURLsToEmail: TButton
            Left = 127
            Top = 286
            Width = 109
            Height = 25
            Caption = 'Send to &email...'
            TabOrder = 6
            OnClick = cmdSendURLsToEmailClick
          end
          object editTestKeyboard: TEdit
            Left = 123
            Top = 45
            Width = 158
            Height = 21
            TabOrder = 0
            OnChange = editTestKeyboardChange
          end
          object cmdBrowseTestKeyboard: TButton
            Left = 287
            Top = 43
            Width = 75
            Height = 25
            Caption = '&Browse...'
            TabOrder = 1
            OnClick = cmdBrowseTestKeyboardClick
          end
          object cmdCopyDebuggerLink: TButton
            Left = 242
            Top = 286
            Width = 109
            Height = 25
            Caption = 'Copy &link'
            TabOrder = 7
            OnClick = cmdCopyDebuggerLinkClick
          end
          object cmdConfigureWebDebugger: TButton
            Left = 168
            Top = 122
            Width = 153
            Height = 25
            Action = modActionsMain.actToolsWebConfigure
            TabOrder = 3
          end
        end
        object panOpenInExplorer: TPanel
          Left = 10
          Top = 148
          Width = 471
          Height = 67
          BevelOuter = bvNone
          Color = 15921906
          ParentBackground = False
          TabOrder = 1
          object lblOpenInExplorer: TLabel
            Left = 9
            Top = 6
            Width = 115
            Height = 17
            Caption = 'Open in Explorer'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object cmdOpenSourceFolder: TButton
            Left = 9
            Top = 33
            Width = 138
            Height = 25
            Caption = '&Open source folder'
            TabOrder = 0
            OnClick = cmdOpenSourceFolderClick
          end
          object cmdOpenBuildFolder: TButton
            Left = 153
            Top = 33
            Width = 138
            Height = 25
            Caption = 'Open &build folder'
            TabOrder = 1
            OnClick = cmdOpenBuildFolderClick
          end
          object cmdOpenProjectFolder: TButton
            Left = 297
            Top = 33
            Width = 138
            Height = 25
            Caption = 'Open pro&ject folder'
            TabOrder = 2
            OnClick = cmdOpenProjectFolderClick
          end
        end
        object panFileActions: TPanel
          Left = 10
          Top = 32
          Width = 471
          Height = 97
          BevelOuter = bvNone
          Color = 15921906
          ParentBackground = False
          TabOrder = 0
          object lblFileActions: TLabel
            Left = 9
            Top = 6
            Width = 75
            Height = 17
            Caption = 'File actions'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -14
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Label5: TLabel
            Left = 10
            Top = 71
            Width = 82
            Height = 13
            Caption = 'Target filename:'
          end
          object cmdAddToProject: TButton
            Left = 153
            Top = 32
            Width = 137
            Height = 25
            Action = modActionsMain.actProjectAddCurrentEditorFile
            TabOrder = 1
          end
          object cmdCompile: TButton
            Left = 10
            Top = 32
            Width = 137
            Height = 25
            Action = modActionsModelEditor.actModelCompile
            TabOrder = 0
          end
          object editOutPath: TEdit
            Left = 98
            Top = 68
            Width = 359
            Height = 21
            TabStop = False
            ParentColor = True
            ReadOnly = True
            TabOrder = 2
          end
        end
      end
    end
  end
  object dlgAddWordlist: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Create or Add Wordlist'
    Left = 32
    Top = 216
  end
  object dlgBrowseTestKeyboard: TOpenDialog
    DefaultExt = 'js'
    Filter = 'Compiled keyboard files (*.js)|*.js|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select compiled keyboard to test with'
    Left = 32
    Top = 280
  end
end
