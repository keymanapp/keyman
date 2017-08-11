inherited frmOSKOnScreenKeyboard: TfrmOSKOnScreenKeyboard
  ClientHeight = 227
  ClientWidth = 655
  Color = 15921905
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  ExplicitWidth = 655
  ExplicitHeight = 227
  PixelsPerInch = 96
  TextHeight = 13
  object kbd: TOnScreenKeyboard
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 647
    Height = 219
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    OnShiftChange = kbdShiftChange
    OnKeyPressed = kbdKeyPressed
    DataFont.Charset = DEFAULT_CHARSET
    DataFont.Color = clWindowText
    DataFont.Height = -11
    DataFont.Name = 'Tahoma'
    DataFont.Style = []
    DisplayUnderlyingChar = False
    SelectMode = False
    Align = alClient
    Color = 15921905
    ParentColor = False
    TabOrder = 0
  end
  object tmrCheck: TTimer
    Interval = 50
    OnTimer = tmrCheckTimer
    Left = 196
    Top = 36
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'html'
    Filter = 'Web pages (*.htm, *.html)|*.htm?'
    Left = 484
    Top = 132
  end
end
