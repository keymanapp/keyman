inherited frmTouchKeyboard: TfrmTouchKeyboard
  BorderStyle = bsNone
  Caption = 'frmTouchKeyboard'
  ClientHeight = 281
  ClientWidth = 488
  OnCreate = FormCreate
  OnShow = FormShow
  ExplicitWidth = 488
  ExplicitHeight = 281
  PixelsPerInch = 96
  TextHeight = 13
  inherited web: TKeymanEmbeddedWB
    Width = 488
    Height = 281
    Silent = True
    ExplicitWidth = 488
    ExplicitHeight = 281
    ControlData = {
      4C00000078330000482100000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object tmrWatchTouchLayout: TTimer
    OnTimer = tmrWatchTouchLayoutTimer
    Left = 232
    Top = 128
  end
end
