inherited frmUnderlyingLayout: TfrmUnderlyingLayout
  Left = 360
  Top = 413
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select Underlying Layout'
  ClientHeight = 285
  ClientWidth = 290
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblKeyboardNotInstalled: TLabel
    Left = 8
    Top = 240
    Width = 270
    Height = 39
    Caption = 
      'The selected layout must be installed from your Windows install ' +
      'media through Control Panel before it can be used as an underlyi' +
      'ng layout.'
    Visible = False
    WordWrap = True
  end
  object cmdOK: TButton
    Left = 208
    Top = 8
    Width = 73
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = cmdOKClick
  end
  object gridKeyboards: TStringGrid
    Left = 8
    Top = 28
    Width = 189
    Height = 205
    ColCount = 4
    DefaultRowHeight = 16
    DefaultDrawing = False
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    TabOrder = 0
    OnClick = gridKeyboardsClick
    OnDblClick = gridKeyboardsDblClick
    OnDrawCell = gridKeyboardsDrawCell
    OnKeyPress = gridKeyboardsKeyPress
  end
  object cmdCancel: TButton
    Left = 208
    Top = 40
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
