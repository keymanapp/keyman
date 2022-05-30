object form_Main: Tform_Main
  Left = 0
  Top = 0
  Caption = 'DiscBurner - Example'
  ClientHeight = 236
  ClientWidth = 370
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
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 29
    Height = 13
    Caption = 'Drive:'
  end
  object com_Drive: TComboBox
    Left = 43
    Top = 8
    Width = 294
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = com_DriveChange
  end
  object GroupBox1: TGroupBox
    Left = 43
    Top = 35
    Width = 294
    Height = 113
    Caption = 'Recorder-Props'
    TabOrder = 1
    object mem_Props: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 284
      Height = 90
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object Button1: TButton
    Left = 43
    Top = 160
    Width = 75
    Height = 25
    Caption = 'AddFiles'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 152
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Burn'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 262
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Erase'
    TabOrder = 4
    OnClick = Button3Click
  end
  object ProgressBar1: TProgressBar
    Left = 43
    Top = 200
    Width = 294
    Height = 16
    TabOrder = 5
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select files to burn ...'
    Left = 152
    Top = 96
  end
end
