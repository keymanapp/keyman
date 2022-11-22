object frmCharacterMapNew: TfrmCharacterMapNew
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Character Map'
  ClientHeight = 469
  ClientWidth = 601
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object shpBorder: TShape
    Left = 0
    Top = 19
    Width = 601
    Height = 410
    Align = alClient
    Brush.Style = bsClear
    Pen.Color = clGray
    ExplicitTop = -1
    ExplicitHeight = 472
  end
  object grid: TTntFixedDrawGrid
    Left = 0
    Top = 19
    Width = 601
    Height = 410
    Align = alClient
    BorderStyle = bsNone
    ColCount = 2
    DefaultColWidth = 48
    DefaultRowHeight = 48
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 2
    FixedRows = 0
    GridLineWidth = 0
    Options = []
    ScrollBars = ssVertical
    TabOrder = 1
    OnClick = gridClick
    OnContextPopup = gridContextPopup
    OnDblClick = gridDblClick
    OnDrawCell = gridDrawCell
    OnKeyDown = gridKeyDown
    OnMouseActivate = gridMouseActivate
    OnMouseDown = gridMouseDown
    OnMouseMove = gridMouseMove
    OnMouseWheelDown = gridMouseWheelDown
    OnMouseWheelUp = gridMouseWheelUp
    OnSelectCell = gridSelectCell
    OnStartDrag = gridStartDrag
    OnTopLeftChanged = gridTopLeftChanged
    ColWidths = (
      48
      48)
    RowHeights = (
      48
      48)
  end
  object panHeader: TPanel
    Left = 0
    Top = 0
    Width = 601
    Height = 19
    Align = alTop
    BevelOuter = bvNone
    Color = 9474192
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    OnResize = panHeaderResize
    object lblSection: TLabel
      Left = 4
      Top = 1
      Width = 62
      Height = 16
      Caption = 'lblSection'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object panBottom: TPanel
    Left = 0
    Top = 429
    Width = 601
    Height = 40
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 2
    DesignSize = (
      601
      40)
    object panName: TPanel
      Left = 2
      Top = 1
      Width = 597
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 0
      object editCharName: TEdit
        Left = 1
        Top = 1
        Width = 595
        Height = 17
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
        OnMouseActivate = editCharNameMouseActivate
      end
    end
    object editFilter: TEdit
      Left = 1
      Top = 18
      Width = 560
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvNone
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = 'Filter by'
      OnChange = editFilterChange
      OnEnter = editFilterEnter
      OnExit = editFilterExit
      OnKeyDown = editFilterKeyDown
      OnMouseActivate = editFilterMouseActivate
    end
    object tbSize: TTrackBar
      Left = 561
      Top = 18
      Width = 41
      Height = 21
      Hint = 'Display Size'
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Anchors = [akTop, akRight]
      LineSize = 5
      Max = 100
      Min = 20
      ParentShowHint = False
      PageSize = 5
      Frequency = 20
      Position = 50
      ShowHint = True
      TabOrder = 2
      ThumbLength = 10
      OnChange = tbSizeChange
    end
    object cmdFilter: TButton
      Left = 541
      Top = 20
      Width = 18
      Height = 17
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 3
      OnClick = cmdFilterClick
    end
  end
  object mnuPopup: TPopupMenu
    OnPopup = mnuPopupPopup
    Left = 264
    Top = 164
    object mnuPopupInsertCode: TMenuItem
      Caption = '&Insert Code'
      ShortCut = 13
      OnClick = mnuPopupInsertCodeClick
    end
    object mnuSeparator1: TMenuItem
      Caption = '-'
    end
    object mnuPopupFilter: TMenuItem
      Caption = 'Fi&lter'
      ShortCut = 16454
      OnClick = mnuPopupFilterClick
    end
    object mnuGoto: TMenuItem
      Caption = '&Goto...'
      ShortCut = 16455
      OnClick = mnuGotoClick
    end
    object mnuSeparator2: TMenuItem
      Caption = '-'
    end
    object cmdShowFonts: TMenuItem
      Caption = '&Show fonts...'
      OnClick = cmdShowFontsClick
    end
    object cmdShowFontsSplit: TMenuItem
      Caption = '-'
    end
    object mnuPopupFont: TMenuItem
      Caption = '&Font...'
      ShortCut = 24646
      OnClick = mnuPopupFontClick
    end
    object mnuPopupInsert: TMenuItem
      Caption = 'Insert &Mode'
      object mnuPopupInsertModeCode: TMenuItem
        Caption = '&Code'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = mnuPopupInsertModeItemClick
      end
      object mnuPopupInsertModeCharacter: TMenuItem
        Caption = 'C&haracter'
        GroupIndex = 1
        RadioItem = True
        OnClick = mnuPopupInsertModeItemClick
      end
      object mnuPopupInsertModeName: TMenuItem
        Caption = '&Name'
        GroupIndex = 1
        RadioItem = True
        OnClick = mnuPopupInsertModeItemClick
      end
    end
    object mnuPopupQuality: TMenuItem
      Caption = 'Display &Quality'
      object mnuPopupDisplayQualityPlain: TMenuItem
        Caption = '&Plain'
        GroupIndex = 2
        RadioItem = True
        OnClick = mnuPopupDisplayQualityClick
      end
      object mnuPopupDisplayQualityAntialiased: TMenuItem
        Caption = '&Antialiased'
        GroupIndex = 2
        RadioItem = True
        OnClick = mnuPopupDisplayQualityClick
      end
      object mnuPopupDisplayQualityCleartype: TMenuItem
        Caption = '&ClearType'
        GroupIndex = 2
        RadioItem = True
        OnClick = mnuPopupDisplayQualityClick
      end
    end
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdForceFontExist, fdNoOEMFonts, fdNoSimulations, fdScalableOnly, fdApplyButton]
    OnApply = dlgFontApply
    Left = 296
    Top = 164
  end
end
