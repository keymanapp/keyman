inherited frmOptions: TfrmOptions
  Left = 336
  Top = 191
  HelpContext = 1241
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
    ActivePage = tabGeneral
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 2
      object cmdProxySettings: TButton
        Left = 7
        Top = 185
        Width = 125
        Height = 25
        Caption = 'Prox&y Settings...'
        TabOrder = 4
        OnClick = cmdProxySettingsClick
      end
      object gbExternalEditor: TGroupBox
        Left = 7
        Top = 47
        Width = 401
        Height = 45
        Caption = 'E&xternal Editor Path'
        TabOrder = 1
        object editExternalEditorPath: TEdit
          Left = 8
          Top = 16
          Width = 311
          Height = 21
          TabOrder = 0
        end
        object cmdBrowseExternalEditor: TButton
          Left = 325
          Top = 16
          Width = 69
          Height = 21
          Caption = '&Browse...'
          TabOrder = 1
        end
      end
      object cmdSMTPSettings: TButton
        Left = 7
        Top = 216
        Width = 125
        Height = 25
        Caption = '&SMTP Settings...'
        TabOrder = 5
        OnClick = cmdSMTPSettingsClick
      end
      object chkOpenKeyboardFilesInSourceView: TCheckBox
        Left = 7
        Top = 12
        Width = 221
        Height = 17
        Caption = 'Open &keyboard files in source view'
        TabOrder = 0
      end
      object cmdResetToolWindows: TButton
        Left = 7
        Top = 154
        Width = 125
        Height = 25
        Caption = '&Reset tool windows'
        TabOrder = 3
        OnClick = cmdResetToolWindowsClick
      end
      object gbDefaultProjectPath: TGroupBox
        Left = 7
        Top = 98
        Width = 401
        Height = 45
        Caption = '&Default Project Folder'
        TabOrder = 2
        object editDefaultProjectPath: TEdit
          Left = 8
          Top = 16
          Width = 311
          Height = 21
          TabOrder = 0
        end
        object cmdBrowseDefaultProjectPath: TButton
          Left = 325
          Top = 16
          Width = 69
          Height = 21
          Caption = 'Bro&wse...'
          TabOrder = 1
          OnClick = cmdBrowseDefaultProjectPathClick
        end
      end
      object chkAutoSaveBeforeCompiling: TCheckBox
        Left = 151
        Top = 158
        Width = 218
        Height = 17
        Caption = '&Automatically save before compiling'
        TabOrder = 6
      end
      object chkOSKAutoSaveBeforeImporting: TCheckBox
        Left = 151
        Top = 181
        Width = 218
        Height = 17
        Caption = 'Au&tomatically save before importing OSK'
        TabOrder = 7
      end
      object gbPrivacy: TGroupBox
        Left = 7
        Top = 256
        Width = 401
        Height = 73
        Caption = 'Privacy'
        TabOrder = 9
        object chkReportUsage: TCheckBox
          Left = 8
          Top = 44
          Width = 311
          Height = 17
          Caption = 'Share anonymous &usage statistics with keyman.com'
          TabOrder = 1
        end
        object chkReportErrors: TCheckBox
          Left = 8
          Top = 21
          Width = 311
          Height = 17
          Caption = 'Automatically report &errors to keyman.com'
          TabOrder = 0
        end
      end
      object chkPromptToUpgradeProjects: TCheckBox
        Left = 151
        Top = 204
        Width = 218
        Height = 17
        Caption = '&Prompt to upgrade projects to 17.0 format'
        TabOrder = 8
      end
    end
    object tabEditor: TTabSheet
      Caption = 'Editor'
      object gbEditorSpaces: TGroupBox
        Left = 11
        Top = 16
        Width = 353
        Height = 89
        Caption = 'Tabs/Spaces'
        TabOrder = 0
        object lblIndentSize: TLabel
          Left = 13
          Top = 55
          Width = 51
          Height = 13
          Caption = '&Indent size'
          FocusControl = editIndentSize
        end
        object chkUseTab: TCheckBox
          Left = 13
          Top = 28
          Width = 129
          Height = 17
          Caption = '&Use tab character (x9)'
          TabOrder = 0
        end
        object editIndentSize: TEdit
          Left = 72
          Top = 51
          Width = 45
          Height = 21
          TabOrder = 1
        end
      end
      object gbEditorFonts: TGroupBox
        Left = 11
        Top = 111
        Width = 353
        Height = 121
        Caption = 'Fonts'
        TabOrder = 1
        object chkLinkFontSizes: TCheckBox
          Left = 13
          Top = 20
          Width = 213
          Height = 17
          Caption = '&Link quoted font size to primary font size'
          TabOrder = 0
        end
        object cmdQuotedFont: TButton
          Left = 12
          Top = 80
          Width = 101
          Height = 25
          Caption = '&Quoted font...'
          TabOrder = 3
          OnClick = cmdQuotedFontClick
        end
        object panQuotedFontSample: TPanel
          Left = 124
          Top = 80
          Width = 187
          Height = 25
          BevelOuter = bvLowered
          Caption = '<font name>'
          TabOrder = 4
        end
        object panFontSample: TPanel
          Left = 124
          Top = 48
          Width = 187
          Height = 25
          BevelOuter = bvLowered
          Caption = '<font name>'
          TabOrder = 2
        end
        object cmdDefaultFont: TButton
          Left = 12
          Top = 48
          Width = 101
          Height = 25
          Caption = 'Default &font...'
          TabOrder = 1
          OnClick = cmdDefaultFontClick
        end
      end
      object gbEditorTheme: TGroupBox
        Left = 11
        Top = 238
        Width = 353
        Height = 82
        Caption = 'Theme'
        TabOrder = 2
        object lblEditorCustomTheme: TLabel
          Left = 124
          Top = 47
          Width = 187
          Height = 13
          AutoSize = False
          Caption = '(custom theme filename)'
        end
        object lblEditorTheme: TLabel
          Left = 13
          Top = 23
          Width = 62
          Height = 13
          Caption = 'Editor &theme:'
        end
        object cbEditorTheme: TComboBox
          Left = 124
          Top = 20
          Width = 187
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnClick = cbEditorThemeClick
        end
      end
    end
    object tabDebugger: TTabSheet
      Caption = 'Debugger'
      ImageIndex = 3
      object gbDebuggerSettings: TGroupBox
        Left = 8
        Top = 7
        Width = 313
        Height = 146
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
          Top = 101
          Width = 289
          Height = 17
          Caption = '&Automatically recompile if no debug information available'
          TabOrder = 4
        end
        object chkDebuggerAutoResetBeforeCompiling: TCheckBox
          Left = 8
          Top = 122
          Width = 250
          Height = 17
          Caption = 'Automatically &reset debugger before recompiling'
          TabOrder = 5
        end
      end
    end
    object tabCharMap: TTabSheet
      Caption = 'Character Map'
      ImageIndex = 4
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
    object tabServer: TTabSheet
      Caption = 'Server'
      ImageIndex = 4
      object gbServer: TGroupBox
        Left = 8
        Top = 8
        Width = 313
        Height = 97
        Caption = 'Keyman Developer Server'
        TabOrder = 0
        object chkListLocalURLs: TCheckBox
          Left = 9
          Top = 60
          Width = 257
          Height = 17
          Caption = 'List local &URLs for Server'
          TabOrder = 1
        end
        object cmdConfigureServer: TButton
          Left = 9
          Top = 21
          Width = 121
          Height = 25
          Caption = '&Configure Server...'
          TabOrder = 0
          OnClick = cmdConfigureServerClick
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
  object dlgBrowseEditorTheme: TOpenDialog
    DefaultExt = 'json'
    Filter = 'Custom theme JSON (*.json)|*.json|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Browse for Editor Theme'
    Left = 212
    Top = 403
  end
  object dlgBrowseDefaultProjectPath: TBrowse4Folder
    InitialDir = 'C:\Users\mcdurdin\Desktop\'
    Root = Desktop
    Title = 'Browse for Default Project Folder'
    Left = 176
    Top = 400
  end
end
