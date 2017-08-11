object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 314
  ClientWidth = 638
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btn_AddJob: TButton
    Left = 48
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Add Job'
    TabOrder = 0
    OnClick = btn_AddJobClick
  end
  object lv_Jobs: TListView
    Left = 8
    Top = 8
    Width = 622
    Height = 150
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'State'
        Width = 100
      end
      item
        Caption = 'GUID'
        Width = 200
      end>
    PopupMenu = pum_Jobs
    TabOrder = 1
    ViewStyle = vsReport
  end
  object tm_RefreshList: TTimer
    Interval = 5000
    OnTimer = tm_RefreshListTimer
    Left = 504
    Top = 216
  end
  object pum_Jobs: TPopupMenu
    Left = 400
    Top = 64
    object Cancel1: TMenuItem
      Caption = 'Cancel'
      OnClick = Cancel1Click
    end
    object Resume1: TMenuItem
      Caption = 'Resume'
      OnClick = Resume1Click
    end
    object Suspend1: TMenuItem
      Caption = 'Suspend'
      OnClick = Suspend1Click
    end
    object Complete1: TMenuItem
      Caption = 'Complete'
      OnClick = Complete1Click
    end
  end
  object dlg_Save: TSaveDialog
    Filter = 'Any file|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 152
    Top = 264
  end
end
