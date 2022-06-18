inherited frameWordlistEditor: TframeWordlistEditor
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'frameWordlistEditor'
  ClientHeight = 338
  ClientWidth = 651
  OnResize = FormResize
  ExplicitWidth = 651
  ExplicitHeight = 338
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TPageControl
    Left = 0
    Top = 0
    Width = 651
    Height = 338
    ActivePage = pageDesign
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    OnChanging = pagesChanging
    ExplicitWidth = 635
    ExplicitHeight = 299
    object pageDesign: TTabSheet
      Caption = 'Design'
      ImageIndex = -1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 627
      ExplicitHeight = 273
      object gridWordlist: TStringGridEditControlled
        Left = 0
        Top = 0
        Width = 643
        Height = 271
        Align = alClient
        ColCount = 3
        DefaultRowHeight = 16
        DefaultDrawing = False
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 0
        OnClick = gridWordlistClick
        OnDrawCell = gridWordlistDrawCell
        OnSetEditText = gridWordlistSetEditText
        ExplicitWidth = 627
        ExplicitHeight = 232
      end
      object panGridControls: TPanel
        Left = 0
        Top = 271
        Width = 643
        Height = 41
        Align = alBottom
        TabOrder = 1
        ExplicitTop = 232
        ExplicitWidth = 627
        object cmdDeleteRow: TButton
          Left = 0
          Top = 6
          Width = 75
          Height = 25
          Caption = '&Delete row'
          TabOrder = 0
          OnClick = cmdDeleteRowClick
        end
        object cmdSortByFrequency: TButton
          Left = 81
          Top = 6
          Width = 104
          Height = 25
          Caption = '&Sort by frequency'
          TabOrder = 1
          OnClick = cmdSortByFrequencyClick
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
