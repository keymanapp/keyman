inherited frameOnScreenKeyboardEditor: TframeOnScreenKeyboardEditor
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 451
  ClientWidth = 801
  Color = clWhite
  KeyPreview = True
  OnDestroy = TntFormDestroy
  ExplicitWidth = 801
  ExplicitHeight = 451
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TPageControl
    Left = 0
    Top = 0
    Width = 801
    Height = 451
    ActivePage = pageDesign
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    OnChange = pagesChange
    OnChanging = pagesChanging
    object pageDesign: TTabSheet
      Caption = 'Design'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object panVK: TPanel
        Left = 0
        Top = 0
        Width = 793
        Height = 425
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object panBottom: TPanel
          Left = 0
          Top = 305
          Width = 793
          Height = 120
          Margins.Top = 4
          Margins.Bottom = 4
          Align = alBottom
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
          object panKeyContent: TPanel
            Left = 243
            Top = 0
            Width = 550
            Height = 72
            Align = alClient
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 0
            object panKeyText: TPanel
              Left = 0
              Top = 0
              Width = 550
              Height = 43
              Align = alClient
              AutoSize = True
              BevelOuter = bvNone
              TabOrder = 0
              object editVKKeyText: TEdit
                Left = 72
                Top = 7
                Width = 155
                Height = 21
                Cursor = crIBeam
                TabOrder = 0
                OnChange = editVKKeyTextChange
                OnClick = editVKKeyTextClick
                OnKeyUp = editVKKeyTextKeyUp
              end
              object rbKeyText: TRadioButton
                Left = 18
                Top = 9
                Width = 40
                Height = 15
                Caption = 'Te&xt'
                TabOrder = 1
                OnClick = rbKeyTextClick
              end
            end
            object panKeyBitmap: TPanel
              Left = 0
              Top = 43
              Width = 550
              Height = 29
              Align = alBottom
              BevelOuter = bvNone
              TabOrder = 1
              object rbKeyBitmap: TRadioButton
                Left = 18
                Top = 6
                Width = 50
                Height = 15
                Caption = '&Bitmap'
                TabOrder = 0
                OnClick = rbKeyBitmapClick
              end
              object cmdBrowseKeyBitmap: TButton
                Left = 71
                Top = 2
                Width = 75
                Height = 25
                Caption = 'B&rowse...'
                TabOrder = 1
                OnClick = cmdBrowseKeyBitmapClick
              end
            end
          end
          object panKeyPreview: TPanel
            Left = 0
            Top = 0
            Width = 243
            Height = 72
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            object panVKKey: TPanel
              Left = 6
              Top = 6
              Width = 235
              Height = 55
              TabOrder = 0
              object Label4: TLabel
                Left = 8
                Top = 19
                Width = 22
                Height = 13
                Caption = 'Key:'
              end
              object VKkeySample: TKeyBtn
                Left = 192
                Top = 12
                Width = 33
                Height = 33
                KeyType = kbtNormal
                DataFont.Charset = DEFAULT_CHARSET
                DataFont.Color = clWindowText
                DataFont.Height = -11
                DataFont.Name = 'Tahoma'
                DataFont.Style = []
                StdRow = 0
                StdLeft = 0
                Selected = False
                DrawDisabled = False
                DisplayUnderlyingChar = True
                Enabled = False
                TabOrder = 0
              end
              object VKkeySampleCtrl: TKeyBtn
                Left = 48
                Top = 12
                Width = 41
                Height = 33
                KeyText = 'Ctrl'
                KeyType = kbtControl
                DataFont.Charset = SYMBOL_CHARSET
                DataFont.Color = clWindowText
                DataFont.Height = -21
                DataFont.Name = 'Symbol'
                DataFont.Style = [fsBold]
                StdRow = 0
                StdLeft = 0
                Selected = False
                DrawDisabled = False
                DisplayUnderlyingChar = True
                Enabled = False
                TabOrder = 1
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                Visible = False
              end
              object VKkeySampleShift: TKeyBtn
                Left = 92
                Top = 12
                Width = 53
                Height = 33
                KeyText = 'Shift'
                KeyType = kbtControl
                DataFont.Charset = SYMBOL_CHARSET
                DataFont.Color = clWindowText
                DataFont.Height = -21
                DataFont.Name = 'Symbol'
                DataFont.Style = [fsBold]
                StdRow = 0
                StdLeft = 0
                Selected = False
                DrawDisabled = False
                DisplayUnderlyingChar = True
                Enabled = False
                TabOrder = 2
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                Visible = False
              end
              object VKkeySampleAlt: TKeyBtn
                Left = 148
                Top = 12
                Width = 41
                Height = 33
                KeyText = 'Alt'
                KeyType = kbtControl
                DataFont.Charset = SYMBOL_CHARSET
                DataFont.Color = clWindowText
                DataFont.Height = -21
                DataFont.Name = 'Symbol'
                DataFont.Style = [fsBold]
                StdRow = 0
                StdLeft = 0
                Selected = False
                DrawDisabled = False
                DisplayUnderlyingChar = True
                Enabled = False
                TabOrder = 3
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                Visible = False
              end
            end
          end
          object panOptions: TPanel
            Left = 0
            Top = 72
            Width = 793
            Height = 48
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            object chkVKDisplayUnderlyingChars: TCheckBox
              Left = 14
              Top = 4
              Width = 192
              Height = 15
              Caption = 'Display underlying &layout characters'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = chkVKDisplayUnderlyingCharsClick
            end
            object chkVKSplitCtrlAlt: TCheckBox
              Left = 14
              Top = 25
              Width = 285
              Height = 15
              Caption = '&Distinguish between left and right Ctrl/Alt'
              TabOrder = 1
              OnClick = chkVKSplitCtrlAltClick
            end
            object chkVKOnlyUseWithUnderlyingLayout: TCheckBox
              Left = 309
              Top = 4
              Width = 173
              Height = 15
              Caption = '&Associate with underlying layout'
              TabOrder = 2
              Visible = False
              OnClick = chkVKOnlyUseWithUnderlyingLayoutClick
            end
            object chkVKInclude102key: TCheckBox
              Left = 309
              Top = 25
              Width = 249
              Height = 15
              Caption = 'Display &102nd Key (as on European keyboards)'
              TabOrder = 3
              OnClick = chkVKInclude102keyClick
            end
            object cmdVKSelectLayout: TButton
              Left = 485
              Top = 2
              Width = 75
              Height = 21
              Caption = 'Select...'
              TabOrder = 4
              Visible = False
              OnClick = cmdVKSelectLayoutClick
            end
            object chkFillUnderlyingLayout: TCheckBox
              Left = 584
              Top = 24
              Width = 153
              Height = 17
              Caption = 'Auto-fill underlying layout'
              TabOrder = 5
            end
          end
        end
        object kbdOnScreen: TOnScreenKeyboard
          AlignWithMargins = True
          Left = 4
          Top = 49
          Width = 785
          Height = 252
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          OnSelectionChange = kbdOnScreenSelectionChange
          OnShiftChange = kbdOnScreenShiftChange
          DataFont.Charset = DEFAULT_CHARSET
          DataFont.Color = clWindowText
          DataFont.Height = -11
          DataFont.Name = 'Tahoma'
          DataFont.Style = []
          DisplayUnderlyingChar = True
          Align = alClient
          Color = 15921905
          OnDragOver = kbdOnScreenDragOver
          OnDragDrop = kbdOnScreenDragDrop
          ParentColor = False
          TabOrder = 1
        end
        object panTop: TPanel
          Left = 0
          Top = 0
          Width = 793
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
          object cmdVKImportKMX: TButton
            Left = 255
            Top = 11
            Width = 101
            Height = 25
            Caption = 'F&ill from layout'
            TabOrder = 2
            OnClick = cmdVKImportKMXClick
          end
          object cmdExportOnScreenKeyboard: TButton
            Left = 442
            Top = 10
            Width = 75
            Height = 25
            Caption = 'E&xport...'
            TabOrder = 0
            OnClick = cmdExportOnScreenKeyboardClick
          end
          object cmdImportOnScreenKeyboard: TButton
            Left = 362
            Top = 11
            Width = 75
            Height = 25
            Caption = '&Import...'
            TabOrder = 1
            OnClick = cmdImportOnScreenKeyboardClick
          end
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
  object dlgVKImportXML: TOpenDialog
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Import XML'
    Left = 48
    Top = 184
  end
  object dlgVKSaveExport: TSaveDialog
    DefaultExt = 'html'
    Filter = 
      'Web page (*.html)|*.htm?|Image (*.bmp)|*.bmp|Image (*.png)|*.png' +
      '|XML file (*.xml)|*.xml'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 48
    Top = 124
  end
  object dlgBrowseForKeyBitmap: TOpenPictureDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 48
    Top = 68
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 52
    Top = 300
  end
  object dlgSaveVisualKeyboard: TSaveDialog
    DefaultExt = 'kvk'
    Filter = 'Visual Keyboard Files (*.kvk)|*.kvk'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Visual Keyboard'
    Left = 852
    Top = 180
  end
  object tmrUpdateCharacterMap: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrUpdateCharacterMapTimer
    Left = 48
    Top = 240
  end
end
