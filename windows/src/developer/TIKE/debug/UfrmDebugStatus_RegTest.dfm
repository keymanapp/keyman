inherited frmDebugStatus_RegTest: TfrmDebugStatus_RegTest
  Caption = 'frmDebugStatus_RegTest'
  PixelsPerInch = 96
  TextHeight = 13
  object panRegTestCommands: TPanel
    Left = 0
    Top = 0
    Width = 61
    Height = 320
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object cmdRegTestStartStopLog: TDebugBitBtn
      Left = 0
      Top = 0
      Width = 57
      Height = 17
      Caption = 'Stop log'
      TabOrder = 0
      OnClick = cmdRegTestStartStopLogClick
    end
    object cmdRegTestStartStopTest: TDebugBitBtn
      Left = 0
      Top = 20
      Width = 57
      Height = 17
      Caption = 'Stop test'
      TabOrder = 1
      OnClick = cmdRegTestStartStopTestClick
    end
    object cmdRegTestOptions: TDebugBitBtn
      Left = 0
      Top = 40
      Width = 57
      Height = 17
      Caption = 'Options'
      Glyph.Data = {
        96000000424D9600000000000000760000002800000007000000080000000100
        04000000000020000000C40E0000C40E0000100000000000000000000000FFFF
        FF00000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000111011101110
        1110110001101100011010000010100000100000000000000000}
      Layout = blGlyphRight
      TabOrder = 2
      OnClick = cmdRegTestOptionsClick
    end
  end
  object lbRegTestLog: TDebugListBox
    Left = 61
    Top = 0
    Width = 373
    Height = 320
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
  object mnuRegTest: TPopupMenu
    AutoPopup = False
    TrackButton = tbLeftButton
    OnPopup = mnuRegTestPopup
    Left = 333
    Top = 133
    object mnuDebugRegTestClearLog: TMenuItem
      Caption = '&Clear log'
      OnClick = mnuDebugRegTestClearLogClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mnuDebugRegTestEdit: TMenuItem
      Caption = '&Edit regression test data...'
      OnClick = mnuDebugRegTestEditClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mnuDebugRegTestOpen: TMenuItem
      Caption = '&Open regression test...'
      OnClick = mnuDebugRegTestOpenClick
    end
    object mnuDebugRegTestSave: TMenuItem
      Caption = '&Save regression test...'
      OnClick = mnuDebugRegTestSaveClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuDebugRegTestBatch: TMenuItem
      Caption = '&Batch regression tests...'
      OnClick = mnuDebugRegTestBatchClick
    end
  end
  object dlgOpenRegtestBatch: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Select Batch Regression Tests'
    Left = 305
    Top = 133
  end
  object dlgOpenRegTest: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    Title = 'Open Regression Test'
    Left = 305
    Top = 105
  end
  object dlgSaveRegTest: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML files (*.xml)|*.xml|All files (*.*)|*.*'
    Left = 333
    Top = 105
  end
end
