inherited frmDebugStatus_Elements: TfrmDebugStatus_Elements
  Caption = 'frmDebugStatus_Elements'
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object lvElements: TDebugListView
    Left = 0
    Top = 0
    Width = 434
    Height = 320
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Contents'
        MinWidth = 100
        Width = 123
      end>
    ColumnClick = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Comic Sans MS'
    Font.Style = []
    GridLines = True
    ParentFont = False
    ShowColumnHeaders = False
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDraw = lvElementsCustomDraw
    OnCustomDrawItem = lvElementsCustomDrawItem
    OnCustomDrawSubItem = lvElementsCustomDrawSubItem
    OnEnter = lvElementsEnter
    OnExit = lvElementsExit
  end
  object mnuPopupStores: TPopupMenu
    Left = 229
    Top = 101
    object mnuPopupStoresHexadecimalView: TMenuItem
      Caption = '&Hexadecimal view'
      OnClick = mnuPopupStoresHexadecimalViewClick
    end
  end
end
