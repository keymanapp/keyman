object frmProjectSettings: TfrmProjectSettings
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
    Top = 43
    Width = 64
    Height = 13
    Caption = 'Placeholders:'
  end
  object Label1: TLabel
    Left = 151
    Top = 43
    Width = 73
    Height = 13
    Caption = '$SOURCEPATH'
  end
  object Label3: TLabel
    Left = 151
    Top = 62
    Width = 77
    Height = 13
    Caption = '$PROJECTPATH'
  end
  object Label4: TLabel
    Left = 151
    Top = 81
    Width = 50
    Height = 13
    Caption = '$VERSION'
  end
  object Label5: TLabel
    Left = 248
    Top = 43
    Width = 137
    Height = 13
    Caption = 'Path that the source file is in'
  end
  object Label6: TLabel
    Left = 248
    Top = 62
    Width = 122
    Height = 13
    Caption = 'Path that the project is in'
  end
  object Label7: TLabel
    Left = 248
    Top = 81
    Width = 119
    Height = 13
    Caption = 'Version of the source file'
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
    TabOrder = 4
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
    TabOrder = 5
  end
  object chkCompilerWarningsAsErrors: TCheckBox
    Left = 81
    Top = 114
    Width = 232
    Height = 17
    Caption = 'Treat compiler &hints and warnings as errors'
    TabOrder = 1
  end
  object chkWarnDeprecatedCode: TCheckBox
    Left = 81
    Top = 137
    Width = 160
    Height = 17
    Caption = 'Warn on &deprecated code'
    TabOrder = 2
  end
  object chkCheckFilenameConventions: TCheckBox
    Left = 81
    Top = 160
    Width = 160
    Height = 17
    Caption = '&Check filename conventions'
    TabOrder = 3
  end
end
