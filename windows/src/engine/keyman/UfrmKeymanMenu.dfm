object frmKeymanMenu: TfrmKeymanMenu
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 632
  ClientWidth = 707
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClick = FormClick
  OnCreate = TntFormCreate
  OnDestroy = TntFormDestroy
  OnHide = TntFormHide
  OnKeyDown = TntFormKeyDown
  OnMouseLeave = FormMouseLeave
  OnMouseMove = FormMouseMove
  OnShow = TntFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object imgTitle: TImage
    Left = 28
    Top = 29
    Width = 109
    Height = 36
    AutoSize = True
    Transparent = True
    Visible = False
  end
  object imgScrollDown: TImage
    Left = 192
    Top = 29
    Width = 16
    Height = 16
    AutoSize = True
    Picture.Data = {
      07544269746D61707E000000424D7E000000000000003E000000280000001000
      000010000000010001000000000040000000C40E0000C40E0000020000000200
      0000FFFFFF00000000000000000000000000000000000180000003C0000007E0
      00000E7000001C380000381C0000700E00000000000000000000000000000000
      00000000000000000000}
    Transparent = True
    Visible = False
  end
  object imgScrollUp: TImage
    Left = 214
    Top = 29
    Width = 16
    Height = 16
    AutoSize = True
    Picture.Data = {
      07544269746D61707E000000424D7E000000000000003E000000280000001000
      000010000000010001000000000040000000C40E0000C40E0000020000000200
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000700E0000381C00001C3800000E70000007E0000003C00000018000000000
      00000000000000000000}
    Transparent = True
    Visible = False
  end
  object imgScrollDownDisabled: TImage
    Left = 192
    Top = 51
    Width = 16
    Height = 16
    AutoSize = True
    Picture.Data = {
      07544269746D61707E000000424D7E000000000000003E000000280000001000
      000010000000010001000000000040000000C40E0000C40E0000020000000200
      0000FFFFFF00C0C0C0000000000000000000000000000180000003C0000007E0
      00000E7000001C380000381C0000700E00000000000000000000000000000000
      00000000000000000000}
    Transparent = True
    Visible = False
  end
  object imgScrollUpDisabled: TImage
    Left = 214
    Top = 51
    Width = 16
    Height = 16
    AutoSize = True
    Picture.Data = {
      07544269746D61707E000000424D7E000000000000003E000000280000001000
      000010000000010001000000000040000000C40E0000C40E0000020000000200
      0000FFFFFF00C0C0C00000000000000000000000000000000000000000000000
      0000700E0000381C00001C3800000E70000007E0000003C00000018000000000
      00000000000000000000}
    Transparent = True
    Visible = False
  end
  object tmrScrollStart: TTimer
    Enabled = False
    Interval = 450
    OnTimer = tmrScrollStartTimer
    Left = 336
    Top = 304
  end
  object tmrScroll: TTimer
    Enabled = False
    Interval = 125
    OnTimer = tmrScrollTimer
    Left = 440
    Top = 304
  end
end
