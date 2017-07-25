inherited frmOSKCharacterMap: TfrmOSKCharacterMap
  Caption = 'frmOSKCharacterMap'
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object lblDesktopLight: TLabel
    Left = 48
    Top = 103
    Width = 4
    Height = 16
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object lblLightLink: TLabel
    Left = 48
    Top = 127
    Width = 4
    Height = 16
    Cursor = crHandPoint
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    Visible = False
  end
end
