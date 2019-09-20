inherited frmWordlistEditor: TfrmWordlistEditor
  Left = 0
  Top = 0
  Caption = 'frmWordlistEditor'
  ClientHeight = 299
  ClientWidth = 635
  Font.Name = 'Tahoma'
  OnResize = FormResize
  ExplicitWidth = 635
  ExplicitHeight = 299
  PixelsPerInch = 96
  TextHeight = 13
  object pages: TPageControl
    Left = 0
    Top = 0
    Width = 635
    Height = 299
    ActivePage = pageDesign
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    OnChanging = pagesChanging
    object pageDesign: TTabSheet
      Caption = 'Design'
      ImageIndex = -1
      object gridWordlist: TStringGridEditControlled
        Left = 0
        Top = 0
        Width = 627
        Height = 232
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
        ExplicitHeight = 273
      end
      object panGridControls: TPanel
        Left = 0
        Top = 232
        Width = 627
        Height = 41
        Align = alBottom
        TabOrder = 1
        ExplicitLeft = 224
        ExplicitTop = 120
        ExplicitWidth = 185
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
    end
  end
end
