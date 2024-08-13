object frmProjectSettings20: TfrmProjectSettings20
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Project Settings'
  ClientHeight = 219
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblOutputPath: TLabel
    Left = 16
    Top = 19
    Width = 59
    Height = 13
    Caption = '&Output path'
    FocusControl = editOutputPath
  end
  object Label2: TLabel
    Left = 81
    Top = 73
    Width = 64
    Height = 13
    Caption = 'Placeholders:'
  end
  object Label3: TLabel
    Left = 151
    Top = 73
    Width = 77
    Height = 13
    Caption = '$PROJECTPATH'
  end
  object Label6: TLabel
    Left = 248
    Top = 73
    Width = 122
    Height = 13
    Caption = 'Path that the project is in'
  end
  object lblSourcePath: TLabel
    Left = 16
    Top = 46
    Width = 58
    Height = 13
    Caption = '&Source path'
    FocusControl = editSourcePath
  end
  object editOutputPath: TEdit
    Left = 81
    Top = 16
    Width = 320
    Height = 21
    TabOrder = 0
  end
  object cmdOK: TButton
    Left = 131
    Top = 186
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 6
    OnClick = cmdOKClick
  end
  object cmdCancel: TButton
    Left = 212
    Top = 186
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object chkCompilerWarningsAsErrors: TCheckBox
    Left = 81
    Top = 114
    Width = 232
    Height = 17
    Caption = 'Treat compiler &hints and warnings as errors'
    TabOrder = 3
  end
  object chkWarnDeprecatedCode: TCheckBox
    Left = 81
    Top = 137
    Width = 160
    Height = 17
    Caption = 'Warn on &deprecated code'
    TabOrder = 4
  end
  object chkCheckFilenameConventions: TCheckBox
    Left = 81
    Top = 160
    Width = 160
    Height = 17
    Caption = '&Check filename conventions'
    TabOrder = 5
  end
  object editSourcePath: TEdit
    Left = 81
    Top = 43
    Width = 320
    Height = 21
    TabOrder = 1
  end
  object chkBuildMetadataFiles: TCheckBox
    Left = 81
    Top = 92
    Width = 232
    Height = 17
    Caption = 'Build &metadata files for Keyman Cloud'
    TabOrder = 2
  end
end
