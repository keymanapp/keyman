object DownloadForm: TDownloadForm
  Left = 322
  Top = 111
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Downloads'
  ClientHeight = 217
  ClientWidth = 776
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListView: TListView
    Left = 0
    Top = 0
    Width = 776
    Height = 217
    Align = alClient
    Columns = <
      item
        Caption = 'File'
        Width = 120
      end
      item
        Caption = 'Status'
        Width = 120
      end
      item
        Alignment = taRightJustify
        Caption = 'Size'
        Width = 55
      end
      item
        Alignment = taRightJustify
        Caption = 'Progress'
        Width = 55
      end
      item
        Alignment = taRightJustify
        Caption = 'Speed'
        Width = 60
      end
      item
        Caption = 'Remaining'
        Width = 70
      end
      item
        Caption = 'Thread ID'
        Width = 60
      end
      item
        Caption = 'ActiveConnections'
        Width = 90
      end
      item
        Caption = 'Progress bar'
        Width = 150
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
