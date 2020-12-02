object frmVisualKeyboard: TfrmVisualKeyboard
  Left = 204
  Top = 143
  Margins.Left = 0
  Margins.Top = 0
  Margins.Right = 0
  Margins.Bottom = 0
  AlphaBlend = True
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 269
  ClientWidth = 551
  Color = 4472358
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = mnuPopup
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnHelp = FormHelp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imgDown: TImage
    Left = 224
    Top = 80
    Width = 105
    Height = 105
    Picture.Data = {
      07544269746D61707E000000424D7E000000000000003E000000280000001000
      000010000000010001000000000040000000C40E0000C40E0000020000000200
      0000FFFFFF00000000000000000000000000000000000180000003C0000007E0
      00000FF000001FF800003FFC00007FFE00000000000000000000000000000000
      00000000000000000000}
    Visible = False
  end
  object panContent: TPanel
    AlignWithMargins = True
    Left = 2
    Top = 2
    Width = 547
    Height = 265
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object panCharMap: TPanel
      Left = 0
      Top = 27
      Width = 547
      Height = 219
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      Visible = False
    end
    object panEntryHelper: TPanel
      Left = 0
      Top = 27
      Width = 547
      Height = 219
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
    end
    object panKeyboard: TPanel
      Left = 0
      Top = 27
      Width = 547
      Height = 219
      Margins.Left = 2
      Margins.Top = 1
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      BevelOuter = bvNone
      Color = 15921905
      ParentBackground = False
      TabOrder = 2
      VerticalAlignment = taAlignTop
    end
    object status: TStatusBar
      Left = 0
      Top = 246
      Width = 547
      Height = 19
      Panels = <
        item
          Width = 64
        end
        item
          Width = 150
        end
        item
          Width = 64
        end
        item
          Width = 150
        end
        item
          Width = 50
        end>
      Visible = False
    end
    object panTop: TPaintPanel
      Left = 0
      Top = 0
      Width = 547
      Height = 27
      EraseBackground = False
      OnPaint = panTopPaint
      Align = alTop
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 4
      OnMouseDown = TitleMouseDown
      OnMouseMove = TitleMouseMove
      OnMouseUp = TitleMouseUp
      OnResize = panTopResize
      object panTitle: TPaintPanel
        AlignWithMargins = True
        Left = 7
        Top = 1
        Width = 114
        Height = 25
        Cursor = crSizeAll
        Margins.Left = 7
        Margins.Top = 1
        Margins.Right = 12
        Margins.Bottom = 1
        EraseBackground = False
        OnPaint = panTitlePaint
        Align = alLeft
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnMouseDown = TitleMouseDown
        OnMouseMove = TitleMouseMove
        OnMouseUp = TitleMouseUp
      end
      object tbLeft: TToolBar
        AlignWithMargins = True
        Left = 134
        Top = 2
        Width = 23
        Height = 23
        Margins.Left = 1
        Margins.Top = 2
        Margins.Right = 0
        Margins.Bottom = 2
        Align = alLeft
        AutoSize = True
        Images = ilToolbar
        TabOrder = 1
        Transparent = False
        Wrapable = False
        OnAdvancedCustomDraw = tbAdvancedCustomDraw
        OnMouseDown = TitleMouseDown
        OnMouseMove = TitleMouseMove
        OnMouseUp = TitleMouseUp
      end
      object tbRight: TToolBar
        AlignWithMargins = True
        Left = 523
        Top = 2
        Width = 23
        Height = 23
        Margins.Left = 0
        Margins.Top = 2
        Margins.Right = 1
        Margins.Bottom = 2
        Align = alRight
        AutoSize = True
        GradientEndColor = 11255750
        GradientStartColor = 12702681
        Images = ilToolbar
        TabOrder = 2
        Wrapable = False
        OnAdvancedCustomDraw = tbAdvancedCustomDraw
        OnMouseDown = TitleMouseDown
        OnMouseMove = TitleMouseMove
        OnMouseUp = TitleMouseUp
      end
      object tbKeyboards: TToolBar
        AlignWithMargins = True
        Left = 158
        Top = 2
        Width = 23
        Height = 23
        Margins.Left = 1
        Margins.Top = 2
        Margins.Right = 0
        Margins.Bottom = 2
        Align = alLeft
        AutoSize = True
        Images = ilKeyboards
        TabOrder = 3
        Transparent = False
        Wrapable = False
        OnAdvancedCustomDraw = tbAdvancedCustomDraw
        OnAdvancedCustomDrawButton = tbKeyboardsAdvancedCustomDrawButton
        OnMouseDown = TitleMouseDown
        OnMouseMove = TitleMouseMove
        OnMouseUp = TitleMouseUp
      end
    end
    object panKeyboardUsage: TPanel
      Left = 0
      Top = 27
      Width = 547
      Height = 219
      Margins.Left = 2
      Margins.Top = 1
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 5
      VerticalAlignment = taAlignTop
      Visible = False
    end
    object panFontHelper: TPanel
      Left = 0
      Top = 27
      Width = 547
      Height = 219
      Margins.Left = 2
      Margins.Top = 1
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alClient
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 6
      VerticalAlignment = taAlignTop
      Visible = False
    end
  end
  object tmrFade: TTimer
    Enabled = False
    Interval = 40
    OnTimer = tmrFadeTimer
    Left = 140
    Top = 132
  end
  object ilToolbar: TexImageList
    AllocBy = 16
    Left = 108
    Top = 64
  end
  object tmrStatus: TTimer
    Interval = 100
    OnTimer = tmrStatusTimer
    Left = 140
    Top = 168
  end
  object mnuPopup: TPopupMenu
    OwnerDraw = True
    OnPopup = mnuPopupPopup
    Left = 168
    Top = 104
  end
  object ilKeyboards: TexImageList
    Left = 136
    Top = 64
  end
  object mnuKeyboards: TPopupMenu
    Images = ilKeyboards
    Left = 264
    Top = 120
  end
end
