inherited frmOptions: TfrmOptions
  Left = 336
  Top = 191
  HelpContext = 1241
  ActiveControl = lbColours
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 439
  ClientWidth = 441
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  OnDestroy = FormDestroy
  ExplicitWidth = 447
  ExplicitHeight = 468
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TPageControl
    Left = 8
    Top = 8
    Width = 425
    Height = 392
    ActivePage = tabColours
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbStartup: TGroupBox
        Left = 8
        Top = 8
        Width = 237
        Height = 73
        Caption = 'Startup'
        TabOrder = 0
        object chkShowStartupDialog: TCheckBox
          Left = 8
          Top = 20
          Width = 125
          Height = 17
          Caption = '&Show splash screen'
          TabOrder = 0
        end
        object chkAllowMultipleInstances: TCheckBox
          Left = 8
          Top = 44
          Width = 221
          Height = 17
          Caption = '&Allow multiple instances of TIKE'
          TabOrder = 1
        end
      end
      object cmdProxySettings: TButton
        Left = 3
        Top = 232
        Width = 125
        Height = 25
        Caption = '&Proxy Settings...'
        TabOrder = 5
        OnClick = cmdProxySettingsClick
      end
      object gbExternalEditor: TGroupBox
        Left = 3
        Top = 143
        Width = 237
        Height = 45
        Caption = '&External Editor Path'
        TabOrder = 2
        object editExternalEditorPath: TEdit
          Left = 8
          Top = 16
          Width = 141
          Height = 21
          TabOrder = 0
        end
        object cmdBrowseExternalEditor: TButton
          Left = 156
          Top = 16
          Width = 73
          Height = 21
          Caption = '&Browse...'
          TabOrder = 1
        end
      end
      object cmdSMTPSettings: TButton
        Left = 134
        Top = 232
        Width = 125
        Height = 25
        Caption = 'S&MTP Settings...'
        TabOrder = 4
        OnClick = cmdSMTPSettingsClick
      end
      object chkOpenKeyboardFilesInSourceView: TCheckBox
        Left = 8
        Top = 88
        Width = 221
        Height = 17
        Caption = 'Open &keyboard files in source view'
        TabOrder = 1
      end
      object cmdResetToolWindows: TButton
        Left = 3
        Top = 201
        Width = 125
        Height = 25
        Caption = '&Reset tool windows'
        TabOrder = 3
        OnClick = cmdResetToolWindowsClick
      end
    end
    object tabEditor: TTabSheet
      Caption = 'Editor'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblIndentSize: TLabel
        Left = 8
        Top = 60
        Width = 51
        Height = 13
        Caption = '&Indent size'
        FocusControl = editIndentSize
      end
      object chkUseTab: TCheckBox
        Left = 8
        Top = 12
        Width = 129
        Height = 17
        Caption = '&Use tab character (x9)'
        TabOrder = 0
      end
      object editIndentSize: TEdit
        Left = 68
        Top = 56
        Width = 45
        Height = 21
        TabOrder = 1
      end
      object cmdDefaultFont: TButton
        Left = 8
        Top = 96
        Width = 101
        Height = 25
        Caption = 'Default &font...'
        TabOrder = 2
        OnClick = cmdDefaultFontClick
      end
      object panFontSample: TPanel
        Left = 120
        Top = 96
        Width = 125
        Height = 25
        BevelOuter = bvLowered
        Caption = '<font name>'
        TabOrder = 3
      end
      object cmdQuotedFont: TButton
        Left = 8
        Top = 128
        Width = 101
        Height = 25
        Caption = '&Quoted font...'
        TabOrder = 4
        OnClick = cmdQuotedFontClick
      end
      object panQuotedFontSample: TPanel
        Left = 120
        Top = 128
        Width = 125
        Height = 25
        BevelOuter = bvLowered
        Caption = '<font name>'
        TabOrder = 5
      end
      object chkLinkFontSizes: TCheckBox
        Left = 8
        Top = 32
        Width = 213
        Height = 17
        Caption = '&Link quoted font size to primary font size'
        TabOrder = 6
      end
    end
    object tabColours: TTabSheet
      Caption = 'Colours'
      ImageIndex = 1
      object panCol1: TPaintPanel
        Left = 176
        Top = 8
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clBlack
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 0
      end
      object lbColours: TListBox
        Left = 4
        Top = 24
        Width = 121
        Height = 145
        ItemHeight = 13
        TabOrder = 1
      end
      object panCol2: TPaintPanel
        Tag = 1
        Left = 204
        Top = 8
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clMaroon
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 2
      end
      object panCol3: TPaintPanel
        Tag = 2
        Left = 232
        Top = 8
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clGreen
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 3
      end
      object panCol4: TPaintPanel
        Tag = 3
        Left = 260
        Top = 8
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clOlive
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 4
      end
      object panCol5: TPaintPanel
        Tag = 4
        Left = 176
        Top = 36
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clNavy
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 5
      end
      object panCol6: TPaintPanel
        Tag = 5
        Left = 204
        Top = 36
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clPurple
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 6
      end
      object panCol7: TPaintPanel
        Tag = 6
        Left = 232
        Top = 36
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clTeal
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 7
      end
      object panCol8: TPaintPanel
        Tag = 7
        Left = 260
        Top = 36
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clGray
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 8
      end
      object panCol9: TPaintPanel
        Tag = 8
        Left = 176
        Top = 64
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clSilver
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 9
      end
      object panCol10: TPaintPanel
        Tag = 9
        Left = 204
        Top = 64
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clRed
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 10
      end
      object panCol11: TPaintPanel
        Tag = 10
        Left = 232
        Top = 64
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clLime
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 11
      end
      object panCol12: TPaintPanel
        Tag = 11
        Left = 260
        Top = 64
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clYellow
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 12
      end
      object panCol13: TPaintPanel
        Tag = 12
        Left = 176
        Top = 92
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clBlue
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 13
      end
      object panCol14: TPaintPanel
        Tag = 13
        Left = 204
        Top = 92
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clFuchsia
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 14
      end
      object panCol15: TPaintPanel
        Tag = 14
        Left = 232
        Top = 92
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clAqua
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 15
      end
      object panCol16: TPaintPanel
        Tag = 15
        Left = 260
        Top = 92
        Width = 25
        Height = 25
        BevelOuter = bvLowered
        Caption = 'FG'
        Color = clWhite
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 16
      end
      object cmdResetSelected: TButton
        Left = 4
        Top = 176
        Width = 85
        Height = 25
        Caption = '&Reset selected'
        TabOrder = 17
      end
      object cmdResetAll: TButton
        Left = 4
        Top = 208
        Width = 85
        Height = 25
        Caption = 'Reset &all'
        TabOrder = 18
      end
      object gbColourStyle: TGroupBox
        Left = 296
        Top = 4
        Width = 113
        Height = 77
        Caption = 'Text attributes'
        TabOrder = 19
        object chkColoursBold: TCheckBox
          Left = 8
          Top = 16
          Width = 41
          Height = 17
          Caption = '&Bold'
          TabOrder = 0
        end
        object chkColoursItalic: TCheckBox
          Left = 8
          Top = 34
          Width = 45
          Height = 17
          Caption = '&Italic'
          TabOrder = 1
        end
        object chkColoursUnderline: TCheckBox
          Left = 8
          Top = 52
          Width = 69
          Height = 17
          Caption = '&Underline'
          TabOrder = 2
        end
      end
      object pmSyntaxExample: TMemo
        Left = 132
        Top = 124
        Width = 277
        Height = 113
        Cursor = crIBeam
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'c Syntax Highlighting'
          'NAME "Syntax Highlight"'
          ''
          'begin Unicode > use(main)'
          ''
          'U+00FC + '#39'A'#39' > U+1124'
          'd225 + '#39'B'#39' > beep')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 20
      end
      object chkUseSyntaxHighlighting: TCheckBox
        Left = 4
        Top = 4
        Width = 137
        Height = 17
        Caption = 'Use &Syntax Highlighting'
        Checked = True
        State = cbChecked
        TabOrder = 21
      end
      object chkPlainBG: TCheckBox
        Left = 296
        Top = 84
        Width = 117
        Height = 17
        Caption = '&Plain background'
        TabOrder = 22
      end
      object chkPlainFG: TCheckBox
        Left = 296
        Top = 103
        Width = 113
        Height = 17
        Caption = 'Plain &foreground'
        TabOrder = 23
      end
    end
    object tabDebugger: TTabSheet
      Caption = 'Debugger'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbDebuggerSettings: TGroupBox
        Left = 8
        Top = 8
        Width = 313
        Height = 129
        Caption = 'Settings'
        TabOrder = 0
        object chkUseOldDebugger: TCheckBox
          Left = 8
          Top = 20
          Width = 217
          Height = 17
          Caption = '&Enable test window (from version 5.0)'
          TabOrder = 0
        end
        object chkDebuggerBreakWhenExitingLine: TCheckBox
          Left = 8
          Top = 40
          Width = 205
          Height = 17
          Caption = '&Breakpoints fire also when exiting line'
          TabOrder = 1
        end
        object chkDebuggerSingleStepAfterBreak: TCheckBox
          Left = 8
          Top = 60
          Width = 197
          Height = 17
          Caption = '&Turn on single step after breakpoint'
          TabOrder = 2
        end
        object chkDebuggerShowStoreOffset: TCheckBox
          Left = 8
          Top = 80
          Width = 217
          Height = 17
          Caption = '&Show matched character offsets in stores'
          TabOrder = 3
        end
        object chkDebuggerAutoRecompile: TCheckBox
          Left = 8
          Top = 100
          Width = 289
          Height = 17
          Caption = '&Automatically recompile if no debug information available'
          TabOrder = 4
        end
      end
      object gbWebHost: TGroupBox
        Left = 8
        Top = 143
        Width = 313
        Height = 89
        Caption = 'Web Host'
        TabOrder = 1
        object lblWebHostDefaultPort: TLabel
          Left = 8
          Top = 31
          Width = 55
          Height = 13
          Caption = 'Default port'
        end
        object editWebHostDefaultPort: TEdit
          Left = 84
          Top = 28
          Width = 61
          Height = 21
          TabOrder = 0
          Text = 'editWebHostDefaultPort'
          OnKeyPress = editWebHostDefaultPortKeyPress
        end
      end
    end
    object tabCharMap: TTabSheet
      Caption = 'Character Map'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbCharMapCharacterLookups: TGroupBox
        Left = 8
        Top = 8
        Width = 261
        Height = 65
        Caption = 'Character Lookups'
        TabOrder = 0
        object chkCharMapAutoLookup: TCheckBox
          Left = 8
          Top = 20
          Width = 221
          Height = 17
          Caption = '&Find character under cursor automatically'
          TabOrder = 0
        end
        object chkCharMapDisableDatabaseLookups: TCheckBox
          Left = 8
          Top = 40
          Width = 221
          Height = 17
          Caption = '&Disable database lookups'
          TabOrder = 1
        end
      end
      object gbCharMapCharacterDatabase: TGroupBox
        Left = 8
        Top = 80
        Width = 261
        Height = 125
        Caption = 'Character Database'
        TabOrder = 1
        object TntLabel1: TLabel
          Left = 8
          Top = 21
          Width = 74
          Height = 13
          Caption = 'Database Path:'
        end
        object cmdCharMapRebuildDatabase: TButton
          Left = 8
          Top = 72
          Width = 113
          Height = 25
          Caption = '&Update database...'
          TabOrder = 0
          OnClick = cmdCharMapRebuildDatabaseClick
        end
        object editDatabasePath: TEdit
          Left = 96
          Top = 18
          Width = 157
          Height = 21
          ParentColor = True
          ReadOnly = True
          TabOrder = 1
        end
      end
    end
  end
  object cmdCancel: TButton
    Left = 356
    Top = 406
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cmdOK: TButton
    Left = 276
    Top = 406
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = cmdOKClick
  end
  object dlgFonts: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = []
    Left = 48
    Top = 404
  end
  object dlgBrowse: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'Programs (*.exe)|*.exe|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Browse for External Editor'
    Left = 84
    Top = 404
  end
  object dlgBrowseUnicodeData: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Unicode Data Files (*.txt)|*.txt|All files (*.*)|*.*'
    Title = 'Browse for Unicode Data Files'
    Left = 132
    Top = 404
  end
end
