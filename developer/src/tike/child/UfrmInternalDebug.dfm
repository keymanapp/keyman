inherit frmInternalDebug: TfrmInternalDebug
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 320
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 434
    Height = 320
    ActivePage = TabSheet4
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 1
    ExplicitTop = 1
    ExplicitWidth = 295
    ExplicitHeight = 137
    object TabSheet4: TTabSheet
      Caption = 'Keyman DEBUGGING'
      ImageIndex = 3
      ExplicitWidth = 287
      ExplicitHeight = 109
      object lbDEBUG: TListBox
        Left = 0
        Top = 0
        Width = 426
        Height = 292
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 287
        ExplicitHeight = 109
      end
    end
  end
end
