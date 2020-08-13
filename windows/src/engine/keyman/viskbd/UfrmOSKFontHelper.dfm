inherited frmOSKFontHelper: TfrmOSKFontHelper
  ClientHeight = 339
  ClientWidth = 635
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 635
  ExplicitHeight = 339
  PixelsPerInch = 96
  TextHeight = 13
  object panNoKeyboard: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 339
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object panFonts: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 339
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object grid: TDrawGrid
      Left = 0
      Top = 0
      Width = 635
      Height = 318
      Align = alClient
      DefaultRowHeight = 20
      FixedCols = 0
      FixedRows = 0
      TabOrder = 0
      OnDblClick = gridDblClick
      OnDrawCell = gridDrawCell
      OnKeyDown = gridKeyDown
      OnMouseWheelDown = gridMouseWheelDown
      OnMouseWheelUp = gridMouseWheelUp
    end
    object panControls: TPanel
      Left = 0
      Top = 318
      Width = 635
      Height = 21
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        635
        21)
      object tbSize: TTrackBar
        Left = 594
        Top = 0
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
        TabOrder = 0
        ThumbLength = 10
        OnChange = tbSizeChange
      end
    end
  end
end
