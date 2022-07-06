inherited frmSelectSystemKeyboard: TfrmSelectSystemKeyboard
  Left = 217
  Top = 282
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select System Keyboard'
  ClientHeight = 273
  ClientWidth = 482
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 181
    Height = 13
    Caption = 'Select an underlying system keyboard:'
  end
  object lblKeyboardNotInstalled: TLabel
    Left = 8
    Top = 240
    Width = 346
    Height = 26
    Caption = 
      'The selected keyboard must be installed from your Windows instal' +
      'l media through Control Panel before it can be used as an underl' +
      'ying keyboard.'
    WordWrap = True
  end
  object cmdOK: TButton
    Left = 400
    Top = 8
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object gridKeyboards: TStringGrid
    Left = 8
    Top = 28
    Width = 381
    Height = 205
    ColCount = 4
    DefaultRowHeight = 16
    DefaultDrawing = False
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    TabOrder = 0
    OnClick = gridKeyboardsClick
    OnDblClick = gridKeyboardsDblClick
    OnDrawCell = gridKeyboardsDrawCell
    OnKeyPress = gridKeyboardsKeyPress
  end
  object cmdCancel: TButton
    Left = 400
    Top = 40
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object tmrResetLookup: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrResetLookupTimer
    Left = 400
    Top = 76
  end
end
